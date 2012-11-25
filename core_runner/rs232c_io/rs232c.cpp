#include "readwrite.h"

static int end_marker = 0;
static int recv_cnt = 0;

// return code
// 0: continue
// 1: normal exit
// -1: error exit
int read_byte_from_rs(int fdr, int write_done, Option *opts) {
  unsigned char buf[16];
  int ret;

  // 読み込み長は 1 に固定したほうがいい??
  switch ((ret = read(fdr, buf, 16))) {
  case - 1:
    if (errno == EINTR || errno == EAGAIN)
      return 0;
    perror("read");
    return - 1;
  case 0:
    if (write_done == 1)
      return 1;
    break;
  default:
    for (int i = 0; i < ret; i++) {
      switch (buf[i]) {
      case 231:
        if (end_marker == 0) { end_marker ++; goto no_print; }
        else end_marker = 0;
        break;
      case 181:
        if (end_marker == 1) { end_marker ++; goto no_print; }
        else end_marker = 0;
        break;
      case 130:
        if (end_marker == 2) { return 1; }
        else end_marker = 0;
        break;
      default:
        end_marker = 0;
        break;
      }
      if (opts->io_type == 0) {
        printf("%c", buf[i]);
      } else {
        printf("%02x", buf[i]);
      }
      recv_cnt++;
      if (opts->io_type != IO_ASCII && (recv_cnt % 4) == 0) {
        printf("\n");
      }
    no_print:
      fflush(stdout);
    }
    break;
  }

  return 0;
}

static char line[16];
static int line_ptr = 0;

// return code
// 0: continue
// 1: write done
// -1: error exit
int read_from_stdin_blocking(int *sending_data, Option *opts) {
  int ret;
  ret = scanf(opts->io_format, sending_data);
  if (ret == EOF) {
    return 1;
  }
  return 0;
}

// return code
// 0: continue
// 1: ready
// 2: exit
// -1: error exit
int read_from_stdin_nonblocking(int *sending_data, Option *opts) {
  int ret = line[line_ptr++] = fgetc(stdin);
  if (ret == EOF) {
    return 2;
  } else if (ret == '\n') {
    line[line_ptr] = '\0';
    sscanf(line, opts->io_format, sending_data);
    line_ptr = 0;
    return 1;
  }
  return 0;
}

int write_byte_to_rs(int fdw, int sending_data, Option *opts) {
  char sendbuf[4];
  int send_size = opts->io_type == 0 ? 1 : opts->io_type * sizeof(char);

  for (int i = 0; i < send_size ; i ++) {
    sendbuf[send_size - 1 - i] = (sending_data >> 8*i) & 0xff;
  }
  if (opts->callback) {
    printf("> ");
    printf(opts->io_format, sending_data);
    printf("\n");
  }
  for (int i = 0; i < send_size ; i ++) {
    sendbuf[send_size - 1 - i] = (sending_data >> 8*i) & 0xff;
  }
  write(fdw, sendbuf, send_size);
  return 0;
}
