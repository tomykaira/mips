#ifndef _SIMULATOR_H_
#define _SIMULATOR_H_

typedef struct _options {
	bool enable_stdout;
	bool enable_record_instruction;
	bool enable_record_mem;
	bool enable_record_register;
	bool enable_record_io;
	bool lib_test_mode;
	bool disable_end_marker;
	char * input_file;
	char * key_file;
	char * sd_file;
	char * target_binary;
} simulation_options;


#endif /* _SIMULATOR_H_ */
