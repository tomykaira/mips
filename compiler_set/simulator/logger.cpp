#include "logger.h"
#include <sys/types.h>
#include <unistd.h>
#include <iostream>
#include <stdint.h>

using namespace std;

Logger::Logger(simulation_options * a_opt) {
	opt = a_opt;
	int id = getpid();
	char log_path[255];
	if (is_enabled()) {
		sprintf(log_path, "%s.%d.log", opt->target_binary, id);
		fp = fopen(log_path, "w");
		if (!fp) {
			throw(string("log is enabled, but failed to open log file: ") + string(log_path));
		}
	} else {
		fp = NULL;
	}
}

void Logger::instruction(uint8_t pc, uint32_t inst) {
	if (fp && opt->enable_record_instruction) {
		fprintf(fp, "INST: %8d %08x\n", pc, inst);
	}
}

void Logger::reg(const char * inst, int dest, uint32_t value) {
	if (fp && opt->enable_record_register) {
		fprintf(fp, "REG: %s %02X %08X\n", inst, dest, value);
	}
}

void Logger::memory(const char * inst, int dest, uint32_t value) {
	if (fp && opt->enable_record_mem) {
		fprintf(fp, "MEM: %s %d %d\n", inst, dest, value);
	}
}

void Logger::io(char ch) {
	if (fp && opt->enable_record_io) {
		fprintf(fp, "IO: %c\n", ch);
	}
}

bool Logger::is_enabled() {
	return opt->enable_record_io || opt->enable_record_mem
		|| opt->enable_record_register || opt->enable_record_instruction;
}
