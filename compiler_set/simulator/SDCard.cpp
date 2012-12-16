#include "SDCard.h"
#include <string>

using namespace std;

SDCard::SDCard(const char * filename) {
	if (filename) {
		fp = fopen(filename, "r+b");
		if (fp == NULL) {
			perror("fopen");
			throw(string("SDCard file is enabled, but failed to open: ") + string(filename));
		}
	} else {
		fp = NULL;
	}
}

SDCard::~SDCard() {
	if (fp) {
		if (fclose(fp) != 0) {
			throw(string("Failed to save SDCard file on exit"));
		}
		fp = NULL;
	}
}

uint32_t SDCard::read_at(uint32_t address) {
	if (!fp) {
		if (address == 0) {
			return 0;
		} else {
			throw(string("Specify SDCard file with -s"));
		}
	} else {
		fseek(fp, address, SEEK_SET);
		int ret = fgetc(fp);
		if (ret == EOF) {
			throw(string("Input file reach EOF, but program requires more bytes"));
		}
		return ret & 0xff;
	}
}

void SDCard::write_at(uint32_t address, uint32_t data) {
	if (!fp) {
		throw(string("Specify SDCard file with -s"));
	} else {
		fseek(fp, address, SEEK_SET);
		int ret = fputc(data, fp);
		if (ret == EOF) {
			throw(string("fputc error"));
		}
	}
}
