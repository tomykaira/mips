#ifndef _INPUTFILE_H_
#define _INPUTFILE_H_

#include <stdint.h>
#include <cstdio>
#include "simulator.h"

class InputFile {
 private:
	FILE * fp;

 public:
	InputFile(const char * filename);
	uint32_t read();
};

#endif /* _INPUTFILE_H_ */
