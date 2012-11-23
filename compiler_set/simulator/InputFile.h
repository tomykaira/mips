#ifndef _INPUTFILE_H_
#define _INPUTFILE_H_

#include <stdint.h>
#include <cstdio>
#include "simulator.h"

class InputFile {
 private:
	FILE * fp;

 public:
	InputFile(simulation_options * opt);
	uint32_t read();
};

#endif /* _INPUTFILE_H_ */
