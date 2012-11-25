#include "readwrite.h"

#define OPTSTRING "B:ah:cbd:Ri:"

int Option::read_option(int argc, char* argv[]) {
  while (1) {
    switch (getopt(argc, argv, OPTSTRING)) {
    case - 1:
      return 0;
    case ':':
      return -1;
    case '?':
      return -1;
    case 'b':
      blocking = true;
      break;
    case 'R':
      no_read = true;
      break;
    case 'B':
      fprintf(stderr, "baud_rate: %d\n", atoi(optarg));
      switch (atoi(optarg)) {
      case 9600:
        baud_rate = B9600;
        break;
      case 230400:
        baud_rate = B230400;
        break;
      case 460800:
        baud_rate = B460800;
        break;
      case 921600:
        baud_rate = B921600;
        break;
      }
      break;
    case 'a':
      io_type = IO_ASCII;
      fprintf(stderr, "io: ascii\n");
      break;
    case 'h':
      io_type = atoi(optarg);
      fprintf(stderr, "io: hex %d\n", io_type);
      break;
    case 'c':
      callback = true;
      fprintf(stderr, "callback: yes\n");
      break;
    case 'd':
      program_file = (char *)malloc(strlen(optarg) + 1);
      strcpy(program_file, optarg);
      break;
    case 'i':
      blocking = true;
      input_fp = fopen(optarg, "r");
      if (input_fp == NULL) {
        perror("fopen input file");
        return -1;
      }
      break;
    default:
      fprintf(stderr, "Unknown option\n");
      return -1;
    }
  }
}
