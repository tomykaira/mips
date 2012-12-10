#include <stdio.h>
#include <unistd.h>

void print_int(int value)
{
  char vv = '\n';
  write(1, &value, 4);
  write(1, &vv, 1);
}

void print_char(char c)
{
  write(1, &c, 1);
}

void send_rs(char * c, int length)
{
  int i = 0;
  for (i = 0; i < length; i ++) {
    print_char(c[i]);
  }
}
