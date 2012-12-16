#include "../include/common.h"
#include "../include/binary.h"
#include "simulator.h"
#include "logger.h"
#include "InputFile.h"
#include "Display.h"
#include "SDCard.h"

#include <cmath>
#include <cassert>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <getopt.h>
#include <signal.h>
#include <errno.h>
#include <libgen.h>
#include "fpu.h"

// 命令の各要素にアクセスする関数を定義
#define DEF_ELE_GET(name, shift, mask) \
	uint32_t name(uint32_t inst) {\
		return ((inst >> shift) & mask);\
	}
DEF_ELE_GET(get_opcode, 26, 0x3f)
DEF_ELE_GET(get_rs, 21, 0x1f)
DEF_ELE_GET(get_rt, 16, 0x1f)
DEF_ELE_GET(get_rd, 11, 0x1f)
DEF_ELE_GET(get_shamt, 6, 0x1f)
DEF_ELE_GET(get_funct, 0, 0x3f)
DEF_ELE_GET(get_address, 0, 0x3ffffff)
int32_t get_imm(uint32_t inst)
{
	if (inst & (1 << 15)) {
		// 即値は負の数のとき符号拡張する
		return (0xffff << 16) | (inst & 0xffff);
	}
	return inst & 0xffff;
}

//------------------------------------------------------------------

// 整数レジスタ
int32_t ireg[INTREG_NUM];
int32_t backup_ireg[INTREG_NUM];
// 浮動小数レジスタ
uint32_t freg[INTREG_NUM];
uint32_t backup_freg[INTREG_NUM];
// リンクレジスタ
uint32_t lreg;

void backup_registers() {
	rep(i, INTREG_NUM) {
		backup_ireg[i] = ireg[i];
		backup_freg[i] = freg[i];
	}
}

// いいかげんな call stack
#define CALL_STACK_SIZE (1 << 20)

// 即値
#define IMM get_imm(inst)
// rs（整数レジスタ）
#define IRS ireg[get_rs(inst)]
// rt（整数レジスタ）
#define IRT ireg[get_rt(inst)]
// rd（整数レジスタ）
#define IRD ireg[get_rd(inst)]
// rs（浮動小数レジスタ）
#define FRS freg[get_rs(inst)]
// rt（浮動小数レジスタ）
#define FRT freg[get_rt(inst)]
// rd（浮動小数レジスタ）
#define FRD freg[get_rd(inst)]
// フレームレジスタ
#define ZR ireg[0]
// ヒープレジスタ
#define FR ireg[1]
// ゼロレジスタ
#define HR ireg[2]
// リンクレジスタ
#define LR lreg

#define DEFAULT_HR 32  // to debug subprocess $r2 related problem

//------------------------------------------------------------------

// アドレスをバイト/ワードアドレッシングに応じて変換
#define addr(x) (x)
#define ADDRESSING_UNIT 1

#define rom_addr(x) (x)
#define ROM_ADDRESSING_UNIT 1

//------------------------------------------------------------------

// 停止命令か

#define isHalt(opcode, funct) (opcode == 0b111111)

// いくつ分レジスタを監視するか
// ゼロレジスタを抜いてr1,f1から数える
#define CHECKWIDTH 8
// 過去何回のレジスタデータを持つか
#define HISTORY 100

typedef struct insthist{
	uint8_t op[HISTORY];
	int32_t ir[HISTORY][CHECKWIDTH];
	uint32_t fr[HISTORY][CHECKWIDTH];
	int pointer;
}RH;

// 発行命令数
long long unsigned cnt;

// ROM
#define ROM_SIZE 20000
Binary ROM[ROM_SIZE];// sync with instruction memory

#define RAM_SIZE ((int)(RAM_NUM*1024*1024/4))
// RAM
uint32_t RAM[RAM_SIZE];
// プログラムカウンタ
uint32_t pc;

// call counter: count each operation
long long unsigned call_count[1 << 6];

float asF(uint32_t r)
{
	conv a;
	a.i = r;
	return a.f;
}

#define DUMP_PC { printf("\tcurrent_pc: %d %s\n", pc, ROM[pc-1].getInst().c_str()); }
#define DUMP_STACK { for (int i = 1; i < stack_pointer; i ++) {printf("\ts%2d: %5d %s\n", i, internal_stack[i]-1, ROM[internal_stack[i]-1].getInst().c_str());} }

#define DELAY_SLOT 2 //遅延スロットの数

#define ARGUMENT_HEAP_SIZE 128

//-----------------------------------------------------------------------------
//
// エンディアンの変換
//
//-----------------------------------------------------------------------------

#define toggle_endian(data) ((data << 24) | ((data << 8) & 0x00ff0000) | ((data >> 8) & 0x0000ff00) | ((data >> 24) & 0x000000ff))

volatile bool step = false;

void disable_step() {
	int flags = fcntl(0, F_GETFL);
	flags |= O_NONBLOCK;
	if (fcntl(0, F_SETFL, flags) != 0){
		perror("fcntl");
		exit(1);
	}
	step = false;
}

void enable_step() {
	int flags = fcntl(0, F_GETFL);
	flags ^= O_NONBLOCK;
	if (fcntl(0, F_SETFL, flags) != 0){
		perror("fcntl");
		exit(1);
	}
	step = true;
}

//無限ループのチェック
bool isLoop(RH h, uint8_t opcode, int32_t ireg[], uint32_t freg[]) {
	int i,j;
	for (i=0; i<HISTORY; i++) {
		if (h.op[i] == opcode) {
			for (j=1; j<CHECKWIDTH; j++) {
				if (h.ir[i][j] != ireg[j] || h.fr[i][j] != freg[j])
					break;
			}
			if (j == CHECKWIDTH) {
				printf("loop?");
				return true;
			}
		}
	}
	return false;
}
//記録の更新
void updateH(RH& h, uint8_t opcode, int32_t ireg[], uint32_t freg[]) {
	int i, j;
	h.pointer++;
	j = h.pointer;
	if (j >= HISTORY)
		j -= HISTORY;
	h.op[j] = opcode;
	for (i=1; i<CHECKWIDTH; i++) {
		h.ir[j][i] = ireg[i];
		h.fr[j][i] = freg[i];
	}
}

// プログラムのバイナリを読み込んで ROM に格納する
int load_program(simulation_options *opt)
{
	int pc = 0;
	// バイナリを読み込む
	FILE* srcFile = fopen(opt->target_binary, "rb");
	if (srcFile == NULL)
	{
		cerr << "couldn't open " << opt->target_binary << endl;
		return 1;
	}

	char *buf;
	size_t size;
	buf = (char *)calloc(1024, sizeof(char));
	while (getline(&buf, &size, srcFile) > 0) {
		uint32_t code;
		char * inst;
		sscanf(buf, "%x", &code);
		inst = strchr(buf, '\t') + 1; // skip tab
		Binary b(inst, code, false);
		ROM[pc] = b;
		pc ++;
	}
	fclose(srcFile);
	return 0;
}

//-----------------------------------------------------------------------------
//
// シミュレート
//
//-----------------------------------------------------------------------------
int simulate(simulation_options * opt)
{
	uint32_t inst;
	int print_count=-1;
	uint8_t opcode, funct;

	int internal_stack[CALL_STACK_SIZE];
	vector<int> jump_logger;
	int stack_pointer = 0;
	char command[1024];
	memset(internal_stack, 0, CALL_STACK_SIZE*sizeof(int));

	Display display = Display();

	if (load_program(opt) == 1)
		return 1;

	Logger logger = Logger(opt);
	InputFile input_file = InputFile(opt->input_file);
	InputFile key_file = InputFile(opt->key_file);
	SDCard sd_card = SDCard(opt->sd_file);

	bool debug_flag = false;

	int end_marker = 0;

	FR = RAM_SIZE-1;
	HR = DEFAULT_HR;

	// load argument heap from file
	if (opt->argument) {
		int length = strlen(opt->argument);
		if (length == ARGUMENT_HEAP_SIZE) {
			cerr << "Argument can be too big for ARGUMENT_HEAP_SIZE: " << ARGUMENT_HEAP_SIZE << endl;
			return 1;
		}
		rep(i, length) {
			RAM[DEFAULT_HR + i] = opt->argument[i];
		}
	}

	RAM[DEFAULT_HR + ARGUMENT_HEAP_SIZE - 1] = opt->current_directory_id;


	int dspc[DELAY_SLOT+1] = {0};      //遅延分岐のキュー。毎週,pcは先頭の要素分だけ加算される。
	int dshd = 0;                      //キューの先頭

	// metrics: FMVHI,  FMVLO
	map<uint32_t, int> fmv_counter;
	bool prev_is_fmvhi = false;
	uint32_t fmvhi_operand = 0;

	// metrics: memory
	uint32_t max_heap = HR, min_frame = FR;
	uint32_t max_call_depth = 0;


	// メインループ
	do
	{
		ZR = 0;
		freg[0] = 0;

		// フレーム/ヒープレジスタは絶対に負になることはない
		if (FR < 0)
		{
			cerr << "error> Frame Register(reg[1]) has become less than 0." << endl;
			break;
		}
		if(HR < 0)
		{
			cerr << "error> Heap Register(reg[2]) has become less than 0." << endl;
			break;
		}
		assert(FR < RAM_SIZE);
		assert(HR < RAM_SIZE);

		if (max_heap < (unsigned)HR) {
			max_heap = HR;
		}
		if (min_frame > (unsigned)FR && (unsigned)FR > 0x800) { // 初期値設定を読みとばす
			min_frame = FR;
		}
		if ((unsigned)stack_pointer > max_call_depth) {
			max_call_depth = stack_pointer;
		}

		dshd = (dshd+1)%(DELAY_SLOT+1);
		pc += dspc[dshd];
		dspc[dshd] = 0;

		assert(rom_addr(pc) >= 0);
		inst = ROM[rom_addr(pc)].getCode();

		logger.instruction(pc, inst);

		if (! opt->lib_test_mode) {
			if (!step && !(cnt % (1000000))) {
				if (read(0, command, 1) != -1) {
					enable_step();
				} else {
					if (errno != EAGAIN && errno != EWOULDBLOCK) {
						perror("read");
						exit(1);
					}
				}
			}
			if (print_count > 0) {
				printf("%8d: %08x\n", pc, inst);
				print_count --;
			} else if (print_count == 0) {
				print_count = -1;
				enable_step();
			}
			if (step) {
				printf("%8d: %08x\n", pc, inst);
				printf("> ");
				scanf("%s", command);
				switch (command[0]) {
				case 's':
					break;
				case '1':
					print_count = 100;
					disable_step();
					break;
				case 'r':
					for (int i = 0; i < INTREG_NUM; i ++) {
						printf("\t%02d: %08x\n", i, ireg[i]);
					}
					break;
				case 'f':
					for (int i = 0; i < INTREG_NUM; i ++) {
						printf("\t%02d: %08x %f\n", i, freg[i], asF(freg[i]));
					}
					break;
				case 'c':
					disable_step();
					break;
				case 'b':
					DUMP_STACK
						break;
				}
			}
		}

		opcode = get_opcode(inst);
		funct = get_funct(inst);
    //無限ループの可能性
    // if (isLoop(history, opcode, ireg, freg)) {
    // 	enable_step();
    // }
    //履歴を更新する
    // updateH(history, opcode, ireg, freg);

		if (ireg[0] != 0)
		{
			cerr << "g0 = " << ireg[0] << endl;
			exit(-1);
		}

		cnt++;
		pc += ROM_ADDRESSING_UNIT;



		if (debug_flag) {
			DUMP_PC
			for (int i = 0; i <= 1; i ++) {
				printf("\t%02d: %08x %f\n", i, freg[i], asF(freg[i]));
			}
		}

		// 1億命令発行されるごとにピリオドを一個ずつ出力する（どれだけ命令が発行されたか視覚的にわかりやすくなる）
		if (!(cnt % (100000000)))
		{
			cerr << "." << flush;
		}

		call_count[opcode] ++;

		if (prev_is_fmvhi) {
			if (opcode == FMVLO) {
				fmv_counter[fmvhi_operand + (IMM & 0xffff)] += 1;
			} else {
				fmv_counter[fmvhi_operand] += 1;
			}
			prev_is_fmvhi = false;
		}

		// 読み込んだopcode・functに対応する命令を実行する
		switch(opcode)
		{
			case ADD:
				logger.reg("ADD", get_rd(inst), IRS + IRT);
				IRD = IRS + IRT;
				break;
			case SUB:
				logger.reg("SUB", get_rd(inst), IRS - IRT);

				IRD = IRS - IRT;
				break;
			case XOR:
				logger.reg("XOR", get_rd(inst), IRS ^ IRT);
				IRD = IRS ^ IRT;
				break;
			case ADDI:
				logger.reg("ADDI", get_rt(inst), IRS + IMM);
				IRT = IRS + IMM;
				break;
			case SUBI:
				logger.reg("SUBI", get_rt(inst), IRS - IMM);
				IRT = IRS - IMM;
				break;
			case SLLI:
				assert(0 <= IMM && IMM < 32);
				logger.reg("SLLI", get_rt(inst), IRS << IMM);
				IRT = IRS << IMM;
				break;
			case SRAI:
				assert(0 <= IMM && IMM < 32);
				logger.reg("SRAI", get_rt(inst), IRS >> IMM);
				IRT = IRS >> IMM;
				break;
			case XORI:
				logger.reg("XORI", get_rt(inst), IRS ^ IMM);
				IRT = IRS ^ IMM;
				break;
			case FADD:
				logger.reg("FADD", get_rd(inst), myfadd(FRS, FRT));
				FRD = myfadd(FRS, FRT);
				break;
			case FSUB:
				logger.reg("FSUB", get_rd(inst), myfsub(FRS, FRT));
				FRD = myfsub(FRS, FRT);
				break;
			case FMUL:
				logger.reg("FMUL", get_rd(inst), myfmul(FRS, FRT));
				FRD = myfmul(FRS, FRT);
				break;
			case FMULN:
				logger.reg("FMULN", get_rd(inst), myfmul(FRS, FRT));
				FRD = myfneg(myfmul(FRS, FRT));
				break;
			case FINV:
				logger.reg("FINV", get_rd(inst), myfinv(FRS));
				FRD = myfinv(FRS);
				break;
			case FSQRT:
				logger.reg("FSQRT", get_rd(inst), myfsqrt(FRS));
				FRD = myfsqrt(FRS);
				break;
			case SETL:
				logger.reg("SETL", get_rt(inst), IMM);
				IRT = IMM;
				break;
			case IMOVF:
				logger.reg("IMOVF", get_rt(inst), IRS);
				memcpy(&FRT, &IRS, 4);
				break;
			case FMOVI:
				logger.reg("FMOVI", get_rt(inst), FRS);
				memcpy(&IRT, &FRS, 4);
				break;
			case FMVLO:
				logger.reg("FMVLO", get_rt(inst), (FRT & 0xffff0000) | (IMM & 0xffff));
				FRT = (FRT & 0xffff0000) | (IMM & 0xffff);
				break;
			case FMVHI:
				prev_is_fmvhi = true;
				fmvhi_operand = (uint32_t)IMM << 16;
				logger.reg("FMVHI", get_rt(inst), ((uint32_t)IMM << 16) | (FRT & 0xffff));
				FRT = ((uint32_t)IMM << 16);
				break;
			case J:
			  jump_logger.push_back(pc);
				pc = get_address(inst);

				break;
			case BEQ:
				if (IRS == IRT) dspc[dshd] = IMM + (-1) - DELAY_SLOT;
				break;
			case BLT:
				if (IRS <  IRT) dspc[dshd] = IMM + (-1) - DELAY_SLOT;
				break;
			case BLE:
				if (IRS <= IRT) dspc[dshd] = IMM + (-1) - DELAY_SLOT;
				break;
			case FBEQ:
				if (asF(FRS) == asF(FRT)) dspc[dshd] = IMM + (-1) - DELAY_SLOT;
				break;
			case FBLT:
				if (asF(FRS) <  asF(FRT)) dspc[dshd] = IMM + (-1) - DELAY_SLOT;
				break;
			case FBLE:
				if (asF(FRS) <= asF(FRT)) dspc[dshd] = IMM + (-1) - DELAY_SLOT;
				break;
			case JR:
				jump_logger.push_back(pc);
				pc = IRS;
				break;
			case CALL:
				jump_logger.push_back(pc);
				assert(stack_pointer < CALL_STACK_SIZE-1);
				internal_stack[++stack_pointer] = pc;
				pc = get_address(inst);
				break;
			case CALLR:
				jump_logger.push_back(pc);
				assert(stack_pointer < CALL_STACK_SIZE-1);
				internal_stack[++stack_pointer] = pc;
				pc = IRS;
				break;
			case RETURN:
				assert(stack_pointer > 0);
				pc = internal_stack[stack_pointer--];
				break;
			case LDR:
				logger.reg("LDR", get_rd(inst), RAM[(IRS + IRT)]);
				assert(IRS + IRT >= 0);
				assert(IRS + IRT < RAM_SIZE);
				IRD = RAM[(IRS + IRT)];

				break;
			case FLDR:
				logger.reg("FLDR", get_rd(inst), RAM[(IRS + IRT)]);
				assert(IRS + IRT >= 0);
				assert(IRS + IRT < RAM_SIZE);
				FRD = RAM[(IRS + IRT)];
				break;
			case STI:
				logger.memory("STI", IRS+IMM, IRT);
				assert(IRS + IMM >= 0);
				if (IRS + IMM >= RAM_SIZE) {
					DUMP_PC
					fprintf(stderr, "Fail: IRS + IMM >= RAM_SIZE\n");
					return 1;
				}
				RAM[(IRS + IMM)] = IRT;
				break;
			case LDI:
				logger.reg("LDI", get_rt(inst), RAM[(IRS + IMM)]);

				assert(IRS + IMM >= 0);
				assert(IRS + IMM < RAM_SIZE);
				IRT = RAM[(IRS + IMM)];
				break;
			case FSTI:
				logger.memory("STI", IRS + IMM, FRT);
				assert(IRS + IMM >= 0);
				assert(IRS + IMM < RAM_SIZE);
				RAM[(IRS + IMM)] = FRT;
				break;
			case FLDI:
				logger.reg("FLDI", get_rt(inst), RAM[(IRS + IMM)]);
				assert(IRS + IMM >= 0);
				assert(IRS + IMM < RAM_SIZE);
				FRT = RAM[(IRS + IMM)];
				break;
			case INPUTB:
				IRT = input_file.read();
				logger.reg("INPUTB", get_rt(inst), IRT);
				break;
			case OUTPUTB:
				if (!opt->disable_end_marker) {
					switch (IRT & 0xff) {
					case 231:
						if (end_marker == 0) { end_marker ++; }
						else end_marker = 0;
						break;
					case 181:
						if (end_marker == 1) { end_marker ++; }
						else end_marker = 0;
						break;
					case 130:
						if (end_marker == 2) { goto end_simulation; }
						else end_marker = 0;
						break;
					default:
						end_marker = 0;
						break;
					}
				}
				if (opt->enable_stdout &&
				    end_marker == 0) { // 終了マーカは無視。ログには出す
					printf("%c", (char)IRT);
				}
				logger.io((char)IRT);
				break;
			case DISPLAY:
				display.set(IRS, IRT);
				break;
			case READKEY:
				if (! opt->lib_test_mode) {
					display.preview();
					usleep(10*1000);
				}
				IRT = key_file.read();
				logger.reg("READKEY", get_rt(inst), IRT);
				break;
			case PROGRAM:
				{
					char dummy[] = "loaded";
					Binary b(dummy, IRT, false);
					assert (IRS < ROM_SIZE);
					ROM[IRS] = b;
					break;
				}
			case READSD:
				IRT = sd_card.read_at(IRS);
				logger.reg("READSD", get_rt(inst), IRT);
				break;
			case WRITESD:
				sd_card.write_at(IRS, IRT);
				break;
			case DEBUG:
				if (opt->lib_test_mode) {
					break;
				}
				switch(IMM) {
				case 1:
					enable_step();
					break;
				case 2:
					printf("calls: %d\n", ireg[28]);
					break;
				case 4:
					DUMP_PC
					printf("\tr3: %d\n", ireg[3]);
					rep(i, 10) {
						printf("%d: %d\n", FR - i, RAM[FR - i]);
					}
					break;
				case 5:
					DUMP_PC
					printf("\tHR: %d\n", ireg[2]);
					printf("\tr3: %d\n", ireg[3]);
					for (int i = jump_logger.size()-10; i < jump_logger.size(); i++) {
						int counter = jump_logger[i];
						printf("%d\t%d\t%s\n", i, counter-1, ROM[counter - 1].getInst().c_str());
					}

					break;
				case 6:
					DUMP_PC
					rep(j, 8) {
						printf("%d: %08x\n", j, ireg[j]);
					}

					rep(j, 8) {
						printf("%02x ", RAM[ireg[2] + j]);
					}
					printf("\n");
					rep(j, 8) {
						printf("%02x ", RAM[ireg[3] + j]);
					}
					printf("\n");
					break;
				case 8:
					printf("hp: %d\n", ireg[2]);
					printf("stack_top: %d\n", internal_stack[stack_pointer]);
					printf("%d: ", ireg[3]);
					rep(j, 20) {
						printf("%02x ", RAM[ireg[3] + j]);
					}
					printf("\n");

					printf("%d: ", ireg[4]);
					rep(j, 20) {
						printf("%02x ", RAM[ireg[4] + j]);
					}
					printf("\n");

					break;
				default:
					break;
				}

				break;
			case HALT:
				goto end_simulation;
			default:
				cerr << "invalid opcode. (opcode = " << (int)opcode << ", funct = " << (int)funct <<  ", pc = " << pc << ")" << endl;
				goto end_simulation;
		}
		fflush(stdout);
	}
	while (!isHalt(opcode, funct)); // haltが来たら終了

 end_simulation:

	if (!opt->lib_test_mode) {
		printf("ADD\t%lld\n", call_count[ADD]);
		printf("SUB\t%lld\n", call_count[SUB]);
		printf("XOR\t%lld\n", call_count[XOR]);
		printf("ADDI\t%lld\n", call_count[ADDI]);
		printf("SUBI\t%lld\n", call_count[SUBI]);
		printf("XORI\t%lld\n", call_count[XORI]);
		printf("SLLI\t%lld\n", call_count[SLLI]);
		printf("SRAI\t%lld\n", call_count[SRAI]);

		printf("SETL\t%lld\n", call_count[SETL]);
		printf("FMVLO\t%lld\n", call_count[FMVLO]);
		printf("FMVHI\t%lld\n", call_count[FMVHI]);
		printf("IMOVF\t%lld\n", call_count[IMOVF]);
		printf("FMOVI\t%lld\n", call_count[FMOVI]);

		printf("FADD\t%lld\n", call_count[FADD]);
		printf("FSUB\t%lld\n", call_count[FSUB]);
		printf("FMUL\t%lld\n", call_count[FMUL]);
		printf("FMULN\t%lld\n", call_count[FMULN]);
		printf("FINV\t%lld\n", call_count[FINV]);
		printf("FSQRT\t%lld\n", call_count[FSQRT]);

		printf("LDI\t%lld\n", call_count[LDI]);
		printf("LDR\t%lld\n", call_count[LDR]);
		printf("STI\t%lld\n", call_count[STI]);
		printf("FLDI\t%lld\n", call_count[FLDI]);
		printf("FSTI\t%lld\n", call_count[FSTI]);
		printf("FLDR\t%lld\n", call_count[FLDR]);

		printf("BEQ\t%lld\n", call_count[BEQ]);
		printf("BLT\t%lld\n", call_count[BLT]);
		printf("BLE\t%lld\n", call_count[BLE]);
		printf("FBEQ\t%lld\n", call_count[FBEQ]);
		printf("FBLT\t%lld\n", call_count[FBLT]);
		printf("FBLE\t%lld\n", call_count[FBLE]);

		printf("J\t%lld\n", call_count[J]);
		printf("JR\t%lld\n", call_count[JR]);
		printf("CALL\t%lld\n", call_count[CALL]);
		printf("CALLR\t%lld\n", call_count[CALLR]);
		printf("RETURN\t%lld\n", call_count[RETURN]);
		printf("INPUTB\t%lld\n", call_count[INPUTB]);
		printf("OUTPUTB\t%lld\n", call_count[OUTPUTB]);
		printf("HALT\t%lld\n", call_count[HALT]);
		printf("DEBUG\t%lld\n", call_count[DEBUG]);

		printf("DISPLAY\t%lld\n", call_count[DISPLAY]);
		printf("READKEY\t%lld\n", call_count[READKEY]);
		printf("PROGRAM\t%lld\n", call_count[PROGRAM]);
		printf("READSD\t%lld\n", call_count[READSD]);
		printf("WRITESD\t%lld\n", call_count[WRITESD]);


		cout << "Floating constant load operation usage:" << endl;
		map<uint32_t, int>::iterator it = fmv_counter.begin();
		while( it != fmv_counter.end() ) {
			cout << (*it).first << "\t" << (asF((*it).first)) << "\t" << (*it).second << endl;
			++it;
		}

		cout << "Memory usage:" << endl;

		cout << "max_heap\t" << max_heap - DEFAULT_HR << endl;
		cout << "min_frame\t" << min_frame << "\tUsage\t" << (0x7ffffff - min_frame) << endl;
		cout << "max call depth\t" << max_call_depth << endl;
	}

	if (!opt->lib_test_mode)
		display.preview();

	if (opt->enable_show_heap) {
		char heap[ARGUMENT_HEAP_SIZE];
		rep(i, ARGUMENT_HEAP_SIZE) {
			heap[i] = RAM[DEFAULT_HR + i] & 0xff;
		}
		printf("%s\n", heap);
	}

	return 0;
}

int main(int argc, char** argv)
{
	int c;
	int ret;
	int length = 0;
	char dirpath[255];

	simulation_options opt;
	opt.enable_stdout             = true;
	opt.enable_record_instruction = false;
	opt.enable_record_mem         = false;
	opt.enable_record_register    = false;
	opt.enable_record_io          = false;
	opt.disable_end_marker        = false;
	opt.lib_test_mode             = false;
	opt.enable_show_heap          = false;
	opt.input_file                = NULL;
	opt.key_file                  = NULL;
	opt.sd_file                   = NULL;
	opt.argument                  = NULL;
	opt.target_binary             = NULL;
	opt.current_directory_id      = 0;

	strcpy(dirpath, argv[0]);
	dirname(dirpath);

	while (1) {
		int option_index = 0;
		static struct option long_options[] = {
			{"reg",        no_argument,       0,  'r' },
			{"inst",       no_argument,       0,  'i' },
			{"mem",        no_argument,       0,  'm' },
			{"io",         no_argument,       0,  'o' },
			{"no-stdout",  no_argument,       0,  'S' },
			{"input",      required_argument, 0,  'f' },
			{"keyread",    required_argument, 0,  'k' },
			{"sdcard",     required_argument, 0,  's' },
			{"argument",   required_argument, 0,  'a' },
			{"cd",         required_argument, 0,  'c' },
			{"show_heap",  no_argument,       0,  'p' },
			{"libtest",    no_argument,       0,  't' },
			{"no_end",     no_argument,       0,  'x' },
			{0,            0,                 0,  0   }
		};

		c = getopt_long(argc, argv, "rimoSf:tk:xs:a:pc:", long_options, &option_index);
		if (c == -1)
			break;

		switch (c) {
		case 'r':
			opt.enable_record_register = true;
			break;

		case 'i':
			opt.enable_record_instruction = true;
			break;

		case 'm':
			opt.enable_record_mem = true;
			break;

		case 'o':
			opt.enable_record_io = true;
			break;

		case 'S':
			opt.enable_stdout = false;
			break;

		case 't':
			opt.lib_test_mode = true;
			break;

		case 'x':
			opt.disable_end_marker = true;
			break;

		case 'f':
			length = strlen(optarg);
			opt.input_file = (char*)calloc(length + 1, sizeof(char));
			strcpy(opt.input_file, optarg);
			break;

		case 'k':
			length = strlen(optarg);
			opt.key_file = (char*)calloc(length + 1, sizeof(char));
			strcpy(opt.key_file, optarg);
			break;

		case 's':
			length = strlen(optarg);
			opt.sd_file = (char*)calloc(length + 1, sizeof(char));
			strcpy(opt.sd_file, optarg);
			break;

		case 'a':
			length = strlen(optarg);
			opt.argument = (char*)calloc(length + 1, sizeof(char));
			strcpy(opt.argument, optarg);
			break;

		case 'p':
			opt.enable_show_heap = true;
			break;

		case 'c':
			sscanf(optarg, "%d", &opt.current_directory_id);
			break;

		default:
			printf("?? getopt returned character code 0%o ??\n", c);
		}
	}

	if (optind < argc) {
		length = strlen(argv[optind]);
		opt.target_binary = (char*)calloc(length + 1, sizeof(char));
		strcpy(opt.target_binary, argv[optind]);
	} else {
		cerr << "usage: ./simulator [OPTIONS] binaryfile" << endl;
		return 1;
	}

	disable_step();

	load_tables(dirpath); // FPU

	cerr << "<simulate> " << endl;

	try {
		ret = simulate(&opt);
	} catch (string exception) {
		cerr << "Exception: " << exception << endl;
		return 1;
	}

	cerr << endl;
        cerr << "issued instructions : " << cnt <<endl;
	cerr << hex << "FR = " << FR << " , " << "HR = " << HR << endl;

	return (ret);
}
