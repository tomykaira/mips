#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "util.h"


#define WRITE(x) { fprintf(fp, "%x\n", (x)); }

int main(int argc, char *argv[])
{
  FILE * fp = fopen("exponent.vec", "w");
  int i = 0;
  if (fp == NULL) {
    perror("fopen");
    return 1;
  }
  for (i = 1; i < 0xff; i++) {
    WRITE((i << 23) + (0x712900));
  }
  fclose(fp);

  fp = fopen("exponent_minus.vec", "w");
  if (fp == NULL) {
    perror("fopen");
    return 1;
  }
  for (i = 1; i < 0xff; i++) {
    WRITE((1 << 31) + (i << 23) + (0x712900));
  }
  fclose(fp);

  fp = fopen("mantissa.vec", "w");
  if (fp == NULL) {
    perror("fopen");
    return 1;
  }
  for (i = 0; i < (1 << 23) - 1; i ++) {
    WRITE(MAN_TO_FLOAT(i));
  }
  fclose(fp);

  fp = fopen("mantissa_minus.vec", "w");
  if (fp == NULL) {
    perror("fopen");
    return 1;
  }
  for (i = 0; i < (1 << 23) - 1; i ++) {
    WRITE((1 << 31) + MAN_TO_FLOAT(i));
  }
  fclose(fp);

  fp = fopen("mantissa_even.vec", "w");
  if (fp == NULL) {
    perror("fopen");
    return 1;
  }
  for (i = 0; i < (1 << 23) - 1; i ++) {
    WRITE(MAN_TO_FLOAT(i) + (1 << 23));
  }
  fclose(fp);

  fp = fopen("corners.vec", "w");
  if (fp == NULL) {
    perror("fopen");
    return 1;
  }
  WRITE(0); // zero
  WRITE(0x3f800000); // 1
  WRITE(0xbf800000); // -1
  fclose(fp);

  fp = fopen("random.vec", "w");
  if (fp == NULL) {
    perror("fopen");
    return 1;
  }
  for (i = 0; i < 10*1000*1000; ++i) {
    WRITE(rand_float());
  }
  fclose(fp);

  fp = fopen("random_minus.vec", "w");
  if (fp == NULL) {
    perror("fopen");
    return 1;
  }
  for (i = 0; i < 10*1000*1000; ++i) {
    WRITE(((rand() % 2) << 31) + rand_float());
  }
  fclose(fp);

  return 0;
}
