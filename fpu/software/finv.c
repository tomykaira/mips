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
#define MANTISSA(x) (0x800000 + (x & 0x7fffff))
#define EXP(x) ((x & 0x7f800000) >> 23)

union IntAndFloat {
    unsigned int ival;
    float fval;
};

unsigned int x0(unsigned int key)
{
  union IntAndFloat v1, v2;
  // 2^n has nothing to do with mantissa
  v1.fval = 1/((float) key);
  v2.fval = 1/(((float) key + 1));
  return EXP(v1.ival) != EXP(v2.ival) ? MANTISSA(v1.ival)/2 + MANTISSA(v2.ival) : MANTISSA(v1.ival)/2 + MANTISSA(v2.ival)/2;
}
unsigned int finv_table_const(unsigned int key)
{
  unsigned int x = x0(key);
  printf("%d %x\n", x, x);
  return ((x << 2) - ((key*x*x) >> 33));
}

unsigned int finv_table_inc(unsigned int key)
{
  unsigned int x = x0(key);
  return (x*x) >> 46;
}

unsigned int finv(unsigned int a)
{
  unsigned long c0;
  unsigned long diff, key;
  key = (a >> 13) & 0x3ff;
  printf("const: %d %x, inc: %d %x\n", finv_table_const(key), finv_table_const(key), finv_table_inc(key), finv_table_inc(key));
  diff = finv_table_inc(key)*(a&0x1fff);
  c0 = finv_table_const(key) - ((diff >> 12) & 0x1fff);
  return (a&0x80000000) + (((key == 0 && diff == 0 ? 254 : 253) - EXP(a)) << 23) + (c0 & 0x7fffff);
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

void test(unsigned int a) {
  union IntAndFloat i, res, res2;
  int exp;

  i.ival = a;

  res2.fval = 1 / i.fval;
  // do not test inf, nan.
  exp = (res2.ival >> 23) & 0xff;
  if (exp == 0xff || exp == 0) { return; }

  res.ival = finv(i.ival);

  // generate testcase for verilog
  if (TESTCASE) {
    printf("%08x\n%08x\n", a, res.ival);
  }

  if (!DEBUG && abs(res.ival - res2.ival) < 4) {
    if (DOTS) {printf(".");}
  } else {
    printf("a: %x\n", a);
    printf("  expected: ");
    print_float(res2.ival);
    printf("\n");

    printf("    actual: ");
    print_float(res.ival);
    printf("\n");
    // exit(1);
  }
}

// test: 負数
int main(int argc, char *argv[])
{
  long long i;
  union IntAndFloat input;

  while (scanf("%f", &input.fval) != EOF) {
    test(input.ival);
  }

  return 0;
}
