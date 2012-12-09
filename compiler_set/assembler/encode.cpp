#include "assembler.h"

DEFINE_R(_add, ADD, 0, 0);
DEFINE_R(_sub, SUB, 0, 0);
DEFINE_R(_xor, XOR, 0, 0);
DEFINE_I(_addi, ADDI);
DEFINE_I(_subi, SUBI);
DEFINE_I(_xori, XORI);
DEFINE_I(_slli, SLLI);
DEFINE_I(_srai, SRAI);

DEFINE_I(_setl, SETL);
DEFINE_I(_fmvlo, FMVLO);
DEFINE_I(_fmvhi, FMVHI);
DEFINE_R(_imovf, IMOVF, 0, 0);
DEFINE_R(_fmovi, FMOVI, 0, 0);

DEFINE_R(_fadd, FADD, 0, 0);
DEFINE_R(_fsub, FSUB, 0, 0);
DEFINE_R(_fmul, FMUL, 0, 0);
DEFINE_R(_fmuln, FMULN, 0, 0);
DEFINE_R(_finv, FINV, 0, 0);
DEFINE_R(_fsqrt, FSQRT, 0, 0);

DEFINE_I(_ldi, LDI);
DEFINE_I(_sti, STI);
DEFINE_R(_ldr, LDR, 0, 0);
DEFINE_I(_fldi, FLDI);
DEFINE_I(_fsti, FSTI);
DEFINE_R(_fldr, FLDR, 0, 0);

DEFINE_I(_beq, BEQ);
DEFINE_I(_blt, BLT);
DEFINE_I(_ble, BLE);
DEFINE_I(_fbeq, FBEQ);
DEFINE_I(_fblt, FBLT);
DEFINE_I(_fble, FBLE);

DEFINE_J(_j, J);
DEFINE_R(_jr, JR, 0, 0);
DEFINE_J(_call, CALL);
DEFINE_R(_callr, CALLR, 0, 0);
DEFINE_R(_return, RETURN, 0, 0);
DEFINE_R(_inputb, INPUTB, 0, 0);
DEFINE_R(_outputb, OUTPUTB, 0, 0);
DEFINE_R(_halt, HALT, 0, 0);
DEFINE_I(_debug, DEBUG);
DEFINE_R(_display, DISPLAY, 0, 0);
DEFINE_R(_readkey, READKEY, 0, 0);
DEFINE_R(_program, PROGRAM, 0, 0);
DEFINE_R(_readsd,  READSD, 0, 0);
DEFINE_R(_writesd, WRITESD, 0, 0);

typedef union
{
	uint32_t i;
	float f;
} conv;

uint32_t double2bin(double d)
{
	conv f;
	f.f = (float)d;
	return f.i;
}

uint32_t gethi(double d)
{
	return (double2bin(d) >> 16) & 0xffff;
}

uint32_t getlo(double d)
{
	return double2bin(d) & 0xffff;
}

//-----------------------------------------------------------------------------
//
// 命令コマンドを解釈してバイナリに変換
//
//-----------------------------------------------------------------------------
bool encode(char* instName, char* buffer, map<uint32_t, string>& labelNames, uint32_t currentLine, uint32_t& code, bool& useLabel)
{
	uint32_t rs = 0;
	uint32_t rt = 0;
	uint32_t rd = 0;
	uint32_t imm = 0;
	char label[MAX_LINE_SIZE];
	char dummy[MAX_LINE_SIZE];

	if (eq(instName, "add"))
	{
		int n = sscanf(buffer, formRRR, dummy, &rd, &rs, &rt);
		if (n == 4)
		{
			code = _add(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "sub"))
	{
		int n = sscanf(buffer, formRRR, dummy, &rd, &rs, &rt);
		if (n == 4)
		{
			code = _sub(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "xor"))
	{
		int n = sscanf(buffer, formRRR, dummy, &rd, &rs, &rt);
		if (n == 4)
		{
			code = _xor(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "addi"))
	{
		int n = sscanf(buffer, formRRI, dummy, &rt, &rs, &imm);
		if (n == 4)
		{
			code = _addi(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "setl"))
	{
		int n = sscanf(buffer, formRL, dummy, &rt, label);
		if (n == 3)
		{
			labelNames[currentLine] = string(label);
			useLabel = true;
			code = _setl(rs, rt, 0);
			return true;
		}
	}
	if (eq(instName, "subi"))
	{
		int n = sscanf(buffer, formRRI, dummy, &rt, &rs, &imm);
		if (n == 4)
		{
			code = _subi(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "xori"))
	{
		int n = sscanf(buffer, formRRI, dummy, &rt, &rs, &imm);
		if (n == 4)
		{
			code = _xori(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "slli"))
	{
		int n = sscanf(buffer, formRRI, dummy, &rt, &rs, &imm);
		if (n == 4)
		{
			code = _slli(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "srai"))
	{
		int n = sscanf(buffer, formRRI, dummy, &rt, &rs, &imm);
		if (n == 4)
		{
			code = _srai(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "fadd"))
	{
		int n = sscanf(buffer, formFFF, dummy, &rd, &rs, &rt);
		if (n == 4)
		{
			code = _fadd(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "fsub"))
	{
		int n = sscanf(buffer, formFFF, dummy, &rd, &rs, &rt);
		if (n == 4)
		{
			code = _fsub(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "fmul"))
	{
		int n = sscanf(buffer, formFFF, dummy, &rd, &rs, &rt);
		if (n == 4)
		{
			code = _fmul(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "fmuln"))
	{
		int n = sscanf(buffer, formFFF, dummy, &rd, &rs, &rt);
		if (n == 4)
		{
			code = _fmuln(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "finv"))
	{
		int n = sscanf(buffer, formFF, dummy, &rd, &rs);
		if (n == 3)
		{
			code = _finv(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "fsqrt"))
	{
		int n = sscanf(buffer, formFF, dummy, &rd, &rs);
		if (n == 3)
		{
			code = _fsqrt(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "imovf"))
	{
		int n = sscanf(buffer, formFR, dummy, &rt, &rs);
		if (n == 3)
		{
			code = _imovf(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "fmovi"))
	{
		int n = sscanf(buffer, formRF, dummy, &rt, &rs);
		if (n == 3)
		{
			code = _fmovi(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "fmvlo"))
	{
		int n = sscanf(buffer, formFI, dummy, &rt, &imm);
		if (n == 3)
		{
			code = _fmvlo(rt, rt, imm);
			return true;
		}
	}
	if (eq(instName, "fmvhi"))
	{
		int n = sscanf(buffer, formFI, dummy, &rt, &imm);
		if (n == 3)
		{
			code = _fmvhi(rt, rt, imm);
			return true;
		}
	}
	if (eq(instName, "j"))
	{
		int n = sscanf(buffer, formL, dummy, label);
		if (n == 2)
		{
			labelNames[currentLine] = string(label);
			useLabel = true;
			code = _j(0);
			return true;
		}
	}
	if (eq(instName, "beq"))
	{
		int n = sscanf(buffer, formRRL, dummy, &rs, &rt, label);
		if (n == 4)
		{
			labelNames[currentLine] = string(label);
			useLabel = true;
			code = _beq(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "blt"))
	{
		int n = sscanf(buffer, formRRL, dummy, &rs, &rt, label);
		if (n == 4)
		{
			labelNames[currentLine] = string(label);
			useLabel = true;
			code = _blt(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "ble"))
	{
		int n = sscanf(buffer, formRRL, dummy, &rs, &rt, label);
		if (n == 4)
		{
			labelNames[currentLine] = string(label);
			useLabel = true;
			code = _ble(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "fbeq"))
	{
		int n = sscanf(buffer, formFFL, dummy, &rs, &rt, label);
		if (n == 4)
		{
			labelNames[currentLine] = string(label);
			useLabel = true;
			code = _fbeq(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "fblt"))
	{
		int n = sscanf(buffer, formFFL, dummy, &rs, &rt, label);
		if (n == 4)
		{
			labelNames[currentLine] = string(label);
			useLabel = true;
			code = _fblt(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "fble"))
	{
		int n = sscanf(buffer, formFFL, dummy, &rs, &rt, label);
		if (n == 4)
		{
			labelNames[currentLine] = string(label);
			useLabel = true;
			code = _fble(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "jr"))
	{
		int n = sscanf(buffer, formR, dummy, &rs);
		if (n == 2)
		{
			code = _jr(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "call"))
	{
		int n = sscanf(buffer, formL, dummy, label);
		if (n == 2)
		{
			labelNames[currentLine] = string(label);
			useLabel = true;
			code = _call(0);
			return true;
		}
	}
	if (eq(instName, "callr"))
	{
		int n = sscanf(buffer, formR, dummy, &rs);
		if (n == 2)
		{
			code = _callr(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "return"))
	{
		int n = sscanf(buffer, form, dummy);
		if (n == 1)
		{
			code = _return(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "ldr"))
	{
		int n = sscanf(buffer, formRRR, dummy, &rd, &rs, &rt);
		if (n == 4)
		{
			code = _ldr(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "fldr"))
	{
		int n = sscanf(buffer, formFRR, dummy, &rd, &rs, &rt);
		if (n == 4)
		{
			code = _fldr(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "sti"))
	{
		int n = sscanf(buffer, formRRI, dummy, &rt, &rs, &imm);
		if (n == 4)
		{
			code = _sti(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "ldi"))
	{
		int n = sscanf(buffer, formRRI, dummy, &rt, &rs, &imm);
		if (n == 4)
		{
			code = _ldi(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "fsti"))
	{
		int n = sscanf(buffer, formFRI, dummy, &rt, &rs, &imm);
		if (n == 4)
		{
			code = _fsti(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "fldi"))
	{
		int n = sscanf(buffer, formFRI, dummy, &rt, &rs, &imm);
		if (n == 4)
		{
			code = _fldi(rs, rt, imm);
			return true;
		}
	}
	if (eq(instName, "inputb"))
	{
		int n = sscanf(buffer, formR, dummy, &rt);
		if (n == 2)
		{
			code = _inputb(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "outputb"))
	{
		int n = sscanf(buffer, formR, dummy, &rt);
		if (n == 2)
		{
			code = _outputb(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "halt"))
	{
		int n = sscanf(buffer, form, dummy);
		if (n == 1)
		{
			code = _halt(rs, rt, rd);
			return true;
		}
	}
	if (eq(instName, "debug"))
	{
	  int n = sscanf(buffer, formI, dummy, &imm);
	  if (n == 2)
	    {
	      code = _debug(rs, rt, imm);
	      return true;
	    }
	}
	if (eq(instName, "display"))
	{
	  int n = sscanf(buffer, formRR, dummy, &rs, &rt);
	  if (n == 3)
	    {
	      code = _display(rs, rt, rd);
	      return true;
	    }
	}
	if (eq(instName, "readkey"))
	{
	  int n = sscanf(buffer, formR, dummy, &rt);
	  if (n == 2)
	    {
	      code = _readkey(rs, rt, rd);
	      return true;
	    }
	}
	if (eq(instName, "program"))
	{
	  int n = sscanf(buffer, formRR, dummy, &rs, &rt);
	  if (n == 3)
	    {
	      code = _program(rs, rt, rd);
	      return true;
	    }
	}
	if (eq(instName, "readsd"))
	{
	  int n = sscanf(buffer, formRR, dummy, &rs, &rt);
	  if (n == 3)
	    {
	      code = _readsd(rs, rt, rd);
	      return true;
	    }
	}
	if (eq(instName, "writesd"))
	{
	  int n = sscanf(buffer, formRR, dummy, &rs, &rt);
	  if (n == 3)
	    {
	      code = _writesd(rs, rt, rd);
	      return true;
	    }
	}

	return false;
}

//-----------------------------------------------------------------------------
//
// 擬似命令（ニーモニック）の解決
// 返り値は分解された各命令がラベルを使うかどうか
//
//-----------------------------------------------------------------------------
vector<bool> mnemonic(char* instName, char mnemonicBuffer[][MAX_LINE_SIZE], map<uint32_t, string>& labelNames, uint32_t currentLine)
{
	uint32_t rs = 0;
	uint32_t rt = 0;
	double d = 0;
	char dummy[MAX_LINE_SIZE];
	vector<bool> useLabels;

	if (eq(instName, "nop"))
	{
		if (sscanf(mnemonicBuffer[0], form, dummy) == 1)
		{
			sprintf(mnemonicBuffer[0], "add\t$r0, $r0, $r0");
			useLabels.push_back(false);
		}
		return	useLabels;
	}
	if (eq(instName, "mov"))
	{
		if (sscanf(mnemonicBuffer[0], formRR, dummy, &rt, &rs) == 3)
		{
			sprintf(mnemonicBuffer[0], "add\t$r%d, $r%d, $r0", rt, rs);
			useLabels.push_back(false);
		}
		return	useLabels;
	}
	if (eq(instName, "not"))
	{
		if (sscanf(mnemonicBuffer[0], formRR, dummy, &rt, &rs) == 3)
		{
			sprintf(mnemonicBuffer[0], "nor\t$r%d, $r%d, $r0", rt, rs);
			useLabels.push_back(false);
		}
		return	useLabels;
	}
	if (eq(instName, "neg"))
	{
		if (sscanf(mnemonicBuffer[0], formRR, dummy, &rt, &rs) == 3)
		{
			sprintf(mnemonicBuffer[0], "sub\t$r%d, $r0, $r%d", rt, rs);
			useLabels.push_back(false);
		}
		return	useLabels;
	}
	if (eq(instName, "fliw"))
	{
		if (sscanf(mnemonicBuffer[0], formFD, dummy, &rs, &d) == 3)
		{
			sprintf(mnemonicBuffer[0], "fmvlo\t$f%d, %d", rs, gethi(d));
			useLabels.push_back(false);
			sprintf(mnemonicBuffer[1], "fmvhi\t$f%d, %d", rs, getlo(d));
			useLabels.push_back(false);
		}
		return	useLabels;
	}
	if (eq(instName, "fliw"))
	{
		if (sscanf(mnemonicBuffer[0], formFD, dummy, &rs, &d) == 3)
		{
			sprintf(mnemonicBuffer[0], "fmvlo\t$f%d, %d", rs, gethi(d));
			useLabels.push_back(false);
			sprintf(mnemonicBuffer[1], "fmvhi\t$f%d, %d", rs, getlo(d));
			useLabels.push_back(false);
		}
		return	useLabels;
	}
	useLabels.push_back(false);
	return useLabels;
}
