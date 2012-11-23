#include "InputFile.h"
#include <string>

using namespace std;

InputFile::InputFile(simulation_options * opt) {
	if (opt->input_file) {
		fp = fopen(opt->input_file, "r");
		if (fp == NULL) {
			throw(string("input file is enabled, but failed to open: ") + string(opt->input_file));
		}
	} else {
		fp = NULL;
	}
}

uint32_t InputFile::read() {
	if (!fp) {
		throw(string("Specify input file with -f"));
	} else {
		int ret = fgetc(fp);
		if (ret == EOF) {
			throw(string("Input file reach EOF, but program requires more bytes"));
		}
		return ret & 0xff;
	}
}
