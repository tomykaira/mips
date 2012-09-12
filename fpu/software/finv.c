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
#define MANTISSA(x) (0x800000 + (x & 0x7fffff))
#define EXP(x) ((x & 0x7f800000) >> 23)
#define MAN_TO_FLOAT(x) ((127 << 23) + ((x) & 0x7fffff))
#define KEYLEN 10

union IntAndFloat {
    unsigned int ival;
    float fval;
};

float x0(unsigned int key)
{
  union IntAndFloat v1, v2, k, k1;
  // 2^n has nothing to do with mantissa
  k.ival = MAN_TO_FLOAT(key << (23 - KEYLEN));
  v1.fval = 1.0/k.fval;
  k1.ival = k.ival | ((1 << (23 - KEYLEN))-1);
  v2.fval = 1.0/k1.fval;
  return (v1.fval + v2.fval) / 2.0;
}

unsigned int int_x(unsigned int key)
{
  union IntAndFloat x;
  unsigned int man;
  x.fval = x0(key);
  man = 0x800000 + (x.ival & 0x7fffff);
  printf("exp: %d\n", EXP(x.ival));
  if (EXP(x.ival)>127){
    man = man << (EXP(x.ival) - 127);
  } else {
    man = man >> (127 - EXP(x.ival));
  }
  return man;
}

float f_finv_table_const(unsigned int key)
{
  float x = x0(key);
  return (x*2 - (MAN_TO_FLOAT(key << 13)*x*x));
}

float f_finv_table_diff(unsigned int key, unsigned int dist)
{
  float x = x0(key);
  return (x*x*(MAN_TO_FLOAT(dist)-1.0));
}

unsigned int finv_table_const(unsigned int key)
{
  unsigned long long x = int_x(key);
  printf("%lld, %d %lld\n", x, (0x400 + key), ((((long long)(0x400 + key))*x*x) >> 33));
  return ((x << 1) - ((((long long)(0x400 + key))*x*x) >> 33));
}

unsigned int finv_table_diff(unsigned int key, unsigned int dist)
{
  unsigned long long x = int_x(key);
  printf("x: %lld dist: %d\n", x, dist);
  return (unsigned int)((x*x*((long long)dist)) >> 46);
}

unsigned int finv(unsigned int a)
{
  unsigned long diff = 1, key, dist;
  union IntAndFloat f_const, f_diff, ans, af;
  // top 10 bits
  key = (a >> 13) & 0x3ff;
  dist = a & 0x1fff;
  // x = finv_table_const(key) - ((dist*finv_table_inc(key)) >> 26);

  // default
  af.ival = MAN_TO_FLOAT(a);
  float xz = x0(key);
  ans.fval = 2*xz - af.fval*xz*xz;
  printf("def  : %x (%d)\n", ans.ival & 0x7fffff, ans.ival & 0x7fffff);

  // int direct
  unsigned long long x, direct, man;
  x = int_x(key);
  man = MANTISSA(a);
  printf("%lld, %lld, %lld\n", man, x, (man*x*x) >> 46);
  direct = 2*x - ((man*x*x) >> 46);
  printf("dir  : %x (%d)\n", (unsigned int)direct, (unsigned int)direct);

  // int
  /*
  printf("int  : %x (%x %x)\n", (finv_table_const(key) - finv_table_diff(key, dist)) & 0x7fffff,
         finv_table_const(key), finv_table_diff(key, dist));
  // x = finv_table_const(key) - finv_table_diff(key, dist);
  // ans.fval = f_const.fval - f_diff.fval;
  */
  return (a&0x80000000) + (((key == 0 && diff == 0 ? 254 : 253) - EXP(a)) << 23) + (ans.ival & 0x7fffff);
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
  union IntAndFloat input;

  while (scanf("%f", &input.fval) != EOF) {
    test(input.ival);
  }

  return 0;
}
