#ifndef _LOGGER_H_
#define _LOGGER_H_

#include <cstdio>
#include "simulator.h"
#include <stdint.h>

class Logger {
 private:
	FILE * fp;
	simulation_options * opt;

 public:
	Logger(simulation_options * opt);

	void instruction(uint8_t pc, uint32_t inst);
	void reg(const char * inst, int dest, uint32_t value);
	void memory(const char * inst, int dest, uint32_t value);
	void io(char ch);
	bool is_enabled();
};


#endif /* _LOGGER_H_ */
