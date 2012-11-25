#include <sys/types.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <sys/select.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <termios.h>
#include <iostream>
#include <string>

using namespace std;

extern char *optarg;
extern int optind, opterr, optopt;

#define IO_ASCII 0

class Option {
public:
  int baud_rate;
  int io_type;
  bool callback;
  bool blocking;
  bool no_read;
  char * program_file;
  FILE * input_fp;
  char io_format[10];

  Option() {
    baud_rate = B460800;
    io_type = IO_ASCII;
    callback = false;
    blocking = false;
    no_read = false;
    program_file = NULL;
    input_fp = NULL;
  }

  void set_io_format() {
    switch(io_type) {
    case 0:
      sprintf(io_format, "%%c");
      break;
    default:
      sprintf(io_format, "%%0%dx", io_type*2);
      break;
    }
  }

  ~Option() {
    if (program_file) {
      free(program_file);
      program_file = NULL;
    }
    if (input_fp) {
      fclose(input_fp);
      input_fp = NULL;
    }
  }

  int read_option(int argc, char* argv[]);

};

int read_byte_from_rs(int fdr, int write_done, Option *opts);
int read_from_stdin_blocking(int *sending_data, Option *opts);
int read_from_stdin_nonblocking(int *sending_data, Option *opts);
int write_byte_to_rs(int fdw, int sending_data, Option *opts);
