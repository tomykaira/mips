// TODO: ボーレートを設定可能にする
//       入出力形式をえらべるようにする(ASCII, Hex...)
//       window を設定可能にする
#include "readwrite.h"


#define max(x, y) ((x < y) ? y : x)
#define toggle_endian(data) ((data << 24) | ((data << 8) & 0x00ff0000) | ((data >> 8) & 0x0000ff00) | ((data >> 24) & 0x000000ff))
#define INST_ROM_SIZE 20000

int init_port(int fd, int baud_rate) {
  struct termios oldOptions, newOptions;

  if (tcgetattr(fd, &oldOptions) < 0){
    close(fd);
    perror("cgetattr");
    return -1;
  }
  bzero(&newOptions, sizeof(newOptions));

  newOptions.c_cflag = baud_rate | CS8 | CLOCAL | CREAD;
  newOptions.c_iflag = IGNPAR;
  newOptions.c_oflag = 0;
  newOptions.c_lflag = 0;
  newOptions.c_cc[VTIME] = 0;
  newOptions.c_cc[VMIN] = 1;

  tcflush(fd, TCIFLUSH);
  if (tcsetattr(fd, TCSANOW, &newOptions) < 0){
    close(fd);
    perror("tcsetattr");
    return -1;
  }
  return 0;
}

int init_async_stdin() {
  struct termios ttystate;

  //get the terminal state
  tcgetattr(STDIN_FILENO, &ttystate);
  //turn off canonical mode and echo
  ttystate.c_lflag &= ~(ICANON | ECHO);
  //minimum of number input read.
  ttystate.c_cc[VMIN] = 1;

  //set the terminal attributes.
  return tcsetattr(STDIN_FILENO, TCSANOW, &ttystate);
}

int rollback_async_stdin() {
  struct termios ttystate;

  //get the terminal state
  tcgetattr(STDIN_FILENO, &ttystate);
  //turn off canonical mode and echo
  ttystate.c_lflag |= ICANON | ECHO;

  //set the terminal attributes.
  return tcsetattr(STDIN_FILENO, TCSANOW, &ttystate);
}

int watch(int fdw, int fdr, Option *opts) {
  fd_set rset, wset;
  int write_done = 0;
  bool pending = false;
  int sending_data = 0;

  while (fdw && fdr) {
    int maxfd = 0;

    FD_ZERO(&rset);
    FD_ZERO(&wset);
    FD_SET(fdr, &rset);
    if (! opts->blocking) {
      FD_SET(STDIN_FILENO, &rset);
    }
    FD_SET(fdw, &wset);
    maxfd = max(fdr, fdw);

    if (select(maxfd + 1, &rset, &wset, NULL, NULL) == - 1) {
      if (errno == EINTR)
        continue;
      perror("select");
      return - 1;
    }

    if (FD_ISSET(fdr, &rset)) {
      switch(read_byte_from_rs(fdr, write_done, opts)) {
      case 0:
        break;
      case -1:
        return -1;
      case 1:
        return 0;
      }
    }

    if (! write_done) {
      if (opts->blocking && FD_ISSET(fdw, &wset) && ! write_done) {
        int val = 0;

        // 標準入力を使用
        // ここで IO 待ちが発生する可能性もある
        if (opts->input_fp) {
          if (fscanf(opts->input_fp, opts->io_format, &val) == EOF) {
            write_done = 1;
          }
        } else {
          write_done = read_from_stdin_blocking(&val, opts) || write_done;
        }
        if (!write_done) {
          write_byte_to_rs(fdw, val, opts);
        }
      }

      if (! opts->blocking) {
        if (pending) {
          if (FD_ISSET(fdw, &wset)) {
            write_byte_to_rs(fdw, sending_data, opts);
            pending = false;
          }
        } else if (FD_ISSET(STDIN_FILENO, &rset)) {
          switch(read_from_stdin_nonblocking(&sending_data, opts)) {
          case -1:
            return 1;
          case 0:
            continue;
          case 1:
            pending = true;
            break;
          case 2:
            write_done = 1;
            break;
          }
        }
      }
    }
  }
  return 0;
}

int send_program(const char * program_file, int fdw) {
  FILE *program_fp;
  unsigned int inst, counter;

  if (!program_file) return 0;

  program_fp = fopen(program_file, "r");
  if (program_fp == NULL) {
    perror("fopen");
    cerr << "Error: failed to read " << program_file << " as the program file" << endl;
    return 1;
  }

  cerr << "Start to send file " << program_file << endl;

  counter = 0;
  while(fscanf(program_fp, "%x", &inst) != EOF) {
    if (inst == 0xffffffff) {
      cerr << "Instruction 0xffffffff is not allowed, because it is instruction terminator.";
      return 1;
    }
    // program data is big-endian, only here.
    inst = toggle_endian(inst);
    if (write(fdw, &inst, 4) != 4) {
      perror("write fdw");
      return 1;
    }
    counter ++;
  }

  int end_marker = 0xffffffff;
  if (write(fdw, &end_marker, 4) != 4) {
    perror("write fdw");
    return 1;
  }

  if (counter >= INST_ROM_SIZE) {
    fprintf(stderr, "Program size maybe too big, the limit is %d, but this file is %d.\nPlease consult the Core creator.\n", INST_ROM_SIZE, counter);
    return 1;
  }

  cerr << "Send file done" << endl;

  return 0;
}

// Bi-directional RS232C communication program
int main(int argc, char* argv[]){
  int fdw = -1, fdr = -1;

  Option opts = Option();
  if (opts.read_option(argc, argv) != 0) {
    return 1;
  }

  fprintf(stderr, "opening...\n");
  for (int i = 0; i < 3; ++i){
    char filename[16];
    //使用するUSBポートの選択。選び方が適当なので、正しく動かない可能性もある。
    sprintf(filename, "/dev/ttyUSB%d", i);
    fprintf(stderr, "  %s\n", filename);
    fdw = open(filename, O_WRONLY | O_NOCTTY);
    if (fdw < 0) {
      fprintf(stderr, "Failed to open %s\n", filename);
    } else if (opts.no_read) {
      break;
    } else {
      fdr = open(filename, O_RDONLY | O_NOCTTY);
      if (fdr > 0) {
        fprintf(stderr, "Successfully opened: %s\n", filename);
        break;
      } else {
        fprintf(stderr, "Failed to open %s\n", filename);
      }
    }
  }
  if (fdw < 0 || (!opts.no_read && fdr < 0)){
    return 1;
  }

  if (init_port(fdw, opts.baud_rate) != 0) {
  }

  if (!opts.no_read && init_port(fdr, opts.baud_rate) != 0) {
    return 1;
  }

  if (send_program(opts.program_file, fdw)) {
    return 1;
  }

  if (opts.no_read) {
    return 0;
  }

  opts.set_io_format();

  if (! opts.blocking) {
    if (init_async_stdin() != 0) {
      return 1;
    }
  }

  int result = watch(fdw, fdr, &opts);
  rollback_async_stdin();
  return result;
}
