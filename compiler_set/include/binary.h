#ifndef _BINARY_H_
#define _BINARY_H_

#include "common.h"

class Binary {
 private:
	string original;
	uint32_t code;
	bool use_label;

	uint32_t toBigEndian(uint32_t data) {
		return (data << 24) | ((data << 8) & 0x00ff0000) | ((data >> 8) & 0x0000ff00) | ((data >> 24) & 0x000000ff);
	}

	char * trim(const char * toTrim) {
		char * ret = (char *)malloc(strlen(toTrim) + 1);
		strcpy(ret, toTrim);
		while (*ret == ' ' || *ret == '\t' || *ret == '\n')
			ret++;

		int end = strlen(ret)-1;
		while (ret[end] == ' ' || ret[end] == '\t' || ret[end] == '\n')
			end--;

		ret[end + 1] = '\0';
		return ret;
	}

 public:
	Binary(char * inst, uint32_t a_code, bool a_use_label) {
		code = a_code;
		use_label = a_use_label;
		original = string(trim(inst));
	}

	// create dummy instruction for simulator
	Binary() {
		code = 0;
		use_label = false;
		original = string();
	}

	void print(FILE * fp) {
		fprintf(fp, "%08x\t%s\n", code, original.c_str());
	}

	bool useLabel() {
		return use_label;
	}

	int instType() {
		return code >> 26;
	}

	void setImm(int immediate) {
		code = (code & 0xffff0000) | (immediate & 0xffff);
	}

	void setJumpImm(int immediate) {
		code = (code & 0xfc000000) | (immediate & 0x3FFffFF);
	}

	uint32_t getCode() {
		return code;
	}

	string getInst() {
		return original;
	}
};

#endif /* _BINARY_H_ */
