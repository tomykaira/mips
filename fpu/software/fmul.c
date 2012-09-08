#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <limits.h>
#include <float.h>

#define swap(a,b) { int temp = a; a = b; b = temp; }
#define DEBUG 0
#define DOTS 0
#define TESTCASE 1
#define D(x) { if (DEBUG) { x ; } }

union IntAndFloat {
    unsigned int ival;
    float fval;
};

unsigned int fmul(unsigned int a, unsigned int b)
{
  return 0x40800000;
}

void test(unsigned int a, unsigned int b, unsigned int s) {
  union IntAndFloat i, v, res, res2, minRes;
  float diff;

  i.ival = a;
  v.ival = b;

  res2.fval = i.fval * v.fval;

  res.ival = fmul(i.ival, v.ival);

  if (TESTCASE) {
    printf("%08x %08x %08x\n", a, b, s);
  }

  if (((s&0x7f800000) >> 23) - 22 > 0)
    minRes.ival = s - (22 << 23);
  else
    minRes.ival = 0x00800000;

  diff = minRes.fval - (res.fval > res2.fval ? 1 : -1)*(res.fval - res2.fval);
  if (!DEBUG && s == res2.ival && abs(res.ival - res2.ival) < 2) {
    if (DOTS) {printf(".");}
  } else {
    printf("a: %x, b: %x\n", a, b);
    printf("  expected: %08x, %lf\n", res2.ival, res2.fval);
    printf("  actual  : %08x, %lf\n", res.ival, res.fval);
  }
}

// test: 負数
int main(int argc, char *argv[])
{
  FILE * fp = fopen("fmul", "r");
  unsigned int a, b, s;
  if (fp == NULL) {
    printf("File error");
    return 1;
  }
  while (fscanf(fp, "%x %x %x ", &a, &b, &s) != EOF) {
    test(a, b, s);
  }
  fclose(fp);

  return 0;
}
