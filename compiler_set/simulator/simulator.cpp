#include "../include/common.h"
#include "../include/binary.h"
#include "simulator.h"
#include "logger.h"
#include "InputFile.h"
#include "Display.h"

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
vector<Binary> ROM;

#define RAM_SIZE ((int)(RAM_NUM*1024*1024/4))
// RAM
uint32_t RAM[RAM_SIZE];
// プログラムカウンタ
uint32_t pc;

float asF(uint32_t r)
{
	conv a;
	a.i = r;
	return a.f;
}

#define DUMP_PC { printf("\tcurrent_pc: %d %s\n", pc, ROM[pc-1].getInst().c_str()); }
#define DUMP_STACK { for (int i = 1; i < stack_pointer; i ++) {printf("\ts%2d: %5d %s\n", i, internal_stack[i]-1, ROM[internal_stack[i]-1].getInst().c_str());} }

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
		ROM.push_back(b);
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
	int counter = 0;
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
	InputFile input_file = InputFile(opt);

	bool debug_flag = false;

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
				logger.reg("FMVHI", get_rt(inst), ((uint32_t)IMM << 16) | (FRT & 0xffff));
				FRT = ((uint32_t)IMM << 16);
				break;
			case J:
			  jump_logger.push_back(pc);
				pc = get_address(inst);

				break;
			case BEQ:
				if (IRS == IRT) pc += IMM + (-1);
				break;
			case BLT:
				if (IRS <  IRT) pc += IMM + (-1);
				break;
			case BLE:
				if (IRS <= IRT) pc += IMM + (-1);
				break;
			case FBEQ:
				if (asF(FRS) == asF(FRT)) pc += IMM + (-1);
				break;
			case FBLT:
				if (asF(FRS) < asF(FRT)) pc += IMM + (-1);
				break;
			case FBLE:
				if (asF(FRS) <= asF(FRT)) pc += IMM + (-1);
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
				if (opt->enable_stdout &&
				    IRT != 231 && IRT != 181 && IRT != 130) { // 終了マーカは無視。ログには出す
					printf("%c", (char)IRT);
				}
				logger.io((char)IRT);
				break;
			case DISPLAY:
				display.set(IRS, IRT);
				break;
			case READKEY:
				if (! opt->lib_test_mode) {
					fprintf(stderr, "Send 0x1C(A) for READKEY\n");
					usleep(10*1000);
				}
				counter++;
				IRT = counter % 20 == 0 ? 0x0a : 0x1c;
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
					break;
				case 8:
					display.preview();
					break;
				default:
					break;
				}

				break;
			case HALT:
				break;
			default:
				cerr << "invalid opcode. (opcode = " << (int)opcode << ", funct = " << (int)funct <<  ", pc = " << pc << ")" << endl;
				break;
		}
		fflush(stdout);
	}
	while (!isHalt(opcode, funct)); // haltが来たら終了

	if (!opt->lib_test_mode)
		display.preview();
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
	opt.lib_test_mode             = false;
	opt.input_file                = NULL;
	opt.target_binary             = NULL;

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
			{"libtest",    no_argument,       0,  't' },
			{0,            0,                 0,  0   }
		};

		c = getopt_long(argc, argv, "rimoSf:t", long_options, &option_index);
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

		case 'f':
			length = strlen(optarg);
			opt.input_file = (char*)calloc(length + 1, sizeof(char));
			strcpy(opt.input_file, optarg);
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
