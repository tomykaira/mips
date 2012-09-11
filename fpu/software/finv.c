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

unsigned int finv(unsigned int a)
{
  unsigned int x, man;
  float x;
  man = 0x800000 + (a & 0x7fffff);
  x = 1/((man >> 13)*1024.0*2.0) + 1/(((man >> 13) + 1)*1024.0*2.0);
  printf("%f", x);
  printf("%f", 2*x + man*x*x);
  return 0;
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
    printf("%08x\n%08x\n%08x\n", a, res.ival);
  }

  if (!DEBUG && abs(res.ival - res2.ival) < 4) {
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
  unsigned int a, b, s;
  long long i;
  for (i = 0;i < 10; i++) {
    test(rand_float());
  }

  return 0;
}
