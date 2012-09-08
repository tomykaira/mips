#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <limits.h>
#include <float.h>

#define swap(a,b) { int temp = a; a = b; b = temp; }
#define DEBUG 0
#define DOTS 0
#define TESTCASE 0
#define D(x) { if (DEBUG) { x ; } }

union IntAndFloat {
    unsigned int ival;
    float fval;
};

unsigned int fmul(unsigned int a, unsigned int b)
{
  unsigned int ah, al, bh, bl;
  unsigned int hh, hl, lh, m;
  unsigned int ae, be, exp, exp1;
  unsigned int sign, is_zero;
  // stage 1
  ah = ((a >> 11) & 0xfff) + 0x1000;
  bh = ((b >> 11) & 0xfff) + 0x1000;
  al = a & 0x7ff;
  bl = b & 0x7ff;
  hh = ah*bh;
  hl = (ah*bl);
  lh = (al*bh);
  ae = (a >> 23) & 0xff;
  be = (b >> 23) & 0xff;
  exp = ae == 0 || be == 0 ? 0 : ae + be - 127; // -127+2
  sign = (a >> 31) ^ (b >> 31);
  is_zero = (a&0x7fffffff) == 0 || (b&0x7fffffff) == 0;

  // stage 2
  m = hh + (hl >> 11) + (lh >> 11) + 2;
  exp1 = exp + 1;

  // stage 3
  if ((m >> 25) > 0) {
    exp = exp1;
    m = m >> 2;
  } else {
    exp = exp;
    m = m >> 1;
  }

  // inf is disposed to 0
  if ((exp & 0x100) > 0) { exp = 0; m = 0; }
  return (sign << 31) + (is_zero ? 0 : (exp << 23) + (m & 0x7fffff));
}

void print_float(unsigned int x) {
  int i = 31;
  for (;i >= 0;i--) {
    printf("%d", (x >> i) & 1);
    if (i == 31 || i == 23)
      printf("|");
  }
  
}

unsigned int rand_float() {
  int exp = 0;
  while (exp <= 0 || exp >= 255) { exp = (rand() & 0xff); }
  return ((rand() % 2) << 31) + (exp << 23) + (rand() & 0x7fffff);
}

void test(unsigned int a, unsigned int b) {
  union IntAndFloat i, v, res, res2;
  int exp;

  if (TESTCASE) {
    printf("%08x %08x\n", a, b);
  }

  i.ival = a;
  v.ival = b;

  res2.fval = i.fval * v.fval;
  // do not test inf, nan.
  exp = (res2.ival >> 23) & 0xff;
  if (exp == 0xff || exp == 0) { return; }

  res.ival = fmul(i.ival, v.ival);

  if (!DEBUG && abs(res.ival - res2.ival) < 2) {
    if (DOTS) {printf(".");}
  } else {
    printf("a: %x, b: %x\n", a, b);
    printf("  expected: ");
    print_float(res2.ival);
    printf("\n");

    printf("    actual: ");
    print_float(res.ival);
    printf("\n");
    exit(1);
  }
}

// test: 負数
int main(int argc, char *argv[])
{
  FILE * fp = fopen("fmul.txt", "r");
  unsigned int a, b, s;
  int i;
  if (fp == NULL) {
    printf("File error");
    return 1;
  }
  while (fscanf(fp, "%x %x %x ", &a, &b, &s) != EOF) {
    test(a, b);
  }
  for (i = 0;i < 10*1000*1000; i++) {
    test(rand_float(), rand_float());
  }
  fclose(fp);

  return 0;
}
