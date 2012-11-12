#include "assembler.h"

// ニーモニックが複数の命令に分解されたときの格納先
char mnemonicBuffer[16][MAX_LINE_SIZE];
// 命令読み込みに使われるバッファ
char* buffer = mnemonicBuffer[0];
// 命令コード
char instName[MAX_LINE_SIZE];
// ヒープサイズ
uint32_t heapSize = 0;
// ラベルが使用される命令の番号と、どのラベルが使われるのかの対応表
map<uint32_t, string> labelNames;
// ラベルと対応する行数
map<string, uint32_t> labels;
// 現在何行目を読み込んでいるか
uint32_t cur = 1;
// アセンブルして得られたバイナリ列. 第二要素はラベルを使うか。setLを分解したときのaddiの処理などに使う
vector<Binary> binaries;

// 入力・出力ファイル
FILE* srcFile;
FILE* dstFile;

//-----------------------------------------------------------------------------
//
// ラベル名から対応するアドレスを得る。base = 0なら絶対アドレス
//
//-----------------------------------------------------------------------------
uint32_t getAddr(string label, uint32_t base = 0)
{
	if (labels.count(label) == 0)
	{
		cerr << label << " is not assigned in labels" << endl;
		exit(-1);
	}
	uint32_t addr = labels[label];
	
//	cerr << "addr of " << label << " is " << addr << endl; 

	return  addr - base;
}

//-----------------------------------------------------------------------------
//
// 命令コマンドを順に読んでいく
//
//-----------------------------------------------------------------------------
bool readInstructions()
{
	bool error = false;
	char* str = NULL;

	while(fgets(buffer, MAX_LINE_SIZE, srcFile) != NULL)
	{
		if(sscanf(buffer, "%s", instName) == 1)
		{
 	 	 	if(strchr(buffer,':'))
			{
 	 	 		// ラベル
 	 	 		if((str = strtok(instName, ":")) == NULL)
				{
					cerr << "error at label line " << cur << " >" << buffer << endl;
					error = true;
				}
				labels[str] = binaries.size();
			}
			else if (string(instName).find("#") == 0)
			{
				// コメント
			}
			else
			{
				// ニーモニックを解決
				vector<bool> useLabels = mnemonic(instName, mnemonicBuffer, labelNames, binaries.size());

				// 普通useLabels.size()は１だが、instNameがニーモニックで複数命令に分解されたら１より大きい値になる
				rep(i, useLabels.size())
				{
					sscanf(mnemonicBuffer[i], "%s", instName);
					uint32_t enc = 0;
					bool useLabel = false;
					bool result = encode(instName, mnemonicBuffer[i], labelNames, binaries.size(), enc, useLabel);
					if (result == false)
					{
						cerr << "error at inst line " << cur << " >" << buffer << endl;
						error = true;
					}
					else
					{
						Binary b(mnemonicBuffer[i], enc, useLabel | useLabels[i]);
						binaries.push_back(b);
					}
				}
			}
		}
		cur++;
	}

	return error == false;
}

//-----------------------------------------------------------------------------
//
// ラベル解決
//
//-----------------------------------------------------------------------------
void resolveLabels()
{
	for (int i = 0; i < binaries.size(); i++)
	{
		// ラベルを使わない命令なら飛ばす
		if (! binaries[i].useLabel())
		{
			continue;
		}
		
		// 命令の種類を取得
		string name;
		switch (binaries[i].instType())
		{ 
			// I形式
			case BEQ:
		  case BNE:
			case BLT:
			case BLE:
			case FBEQ:
			case FBNE:
			case FBLT:
			case FBLE:
				if (labelNames.count(i) <= 0)
				{
					cout << i << " is not assigned in labelNames.(" << i << ")" << endl;
					exit(-1);
				} 
				name = labelNames[i];
				binaries[i].setImm(getAddr(name, i + 0));
				break;
			case ADDI:
			case SUBI:
			case MULI:
			case SLLI:
			case SRAI:
			case ANDI:
			case ORI:
			case NORI:
			case XORI:
			case MVLO:
			case MVHI:
			case FMVLO:
			case FMVHI:
			case STI:
			case LDI:
			case FSTI:
			case FLDI:
				if (labelNames.count(i) <= 0)
				{
					cout << i << " is not assigned in labelNames.(" << i << ")" << endl;
					exit(-1);
				} 
				name = labelNames[i];
				binaries[i].setImm(getAddr(name));
				break;
			case J:
			case CALL:
				// 絶対アドレス
				if (labelNames.count(i) <= 0)
				{
					cout << i << " is not assigned in labelNames.(" << i << ")" << endl;
					exit(-1);
				} 
				name = labelNames[i];
				binaries[i].setJumpImm(getAddr(name));
				break;
			default:
				break;
		}
	}
}

//-----------------------------------------------------------------------------
//
// アセンブル
//
//-----------------------------------------------------------------------------
bool assemble(const char* srcPath, const char* dstPath)
{
	bool error = false;
	
	// 入力ファイルを開く
	srcFile = fopen(srcPath, "r");
	if (srcFile == NULL)
	{
		cerr << "couldn't open " << srcPath << endl;
		return false;
	}

	bool result = false;


	// ラベルや各コマンドを読み込む
	result = readInstructions();
	if (result == false)
	{
		cerr << "couldn't read instruction datas correctly" << endl;
		error = true;
	}

	// 入力ファイルを閉じる
	fclose(srcFile);

	// エラーが起きてればこの時点で戻るa
	if (error)
	{
		return false;
	}

	// ラベル解決
	resolveLabels();

	// 出力ファイルを開く
	dstFile = fopen(dstPath, "w");
	if (dstFile == NULL)
	{
		cerr << "couldn't open " << dstPath << endl;
		return false;
	}

	rep(i, binaries.size())
	{
		binaries[i].print(dstFile);
	}

	// 出力ファイルを閉じる
	fclose(dstFile);

	return true;
}

int main(int argc, char** argv)
{
	// コマンド引数から入力ファイル・出力ファイルを探す
	int src = 0, dst = 0;
	repi(i, argc)
	{
		if (argv[i][0] != '-')
		{
			if (src <= 0) src = i;
			else dst = i;
		}
	}

	if (src <= 0 || dst <= 0)
	{
		cerr << "usage: ./assembler src dst" << endl;
		return 1;
	}

	cerr << "<assemble> ";
	
	bool result = assemble(argv[src], argv[dst]);
	
	if (result)
	{
		cerr << argv[src] << " => " << argv[dst] << endl;
	}
	else
	{
		cerr << "couldn't assemble " << argv[src] << endl;
		return 1;
	}

	return 0;
}


