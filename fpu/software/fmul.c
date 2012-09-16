#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <limits.h>
#include <float.h>
#include "util.h"

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

void test(FILE * fp, unsigned int a, unsigned int b) {
  union IntAndFloat i, v, res, ref;

  i.ival = a;
  v.ival = b;

  ref.fval = i.fval * v.fval;
  res.ival = fmul(i.ival, v.ival);

  if (! (is_normal(a) && is_normal(b) && is_normal(ref.ival) && is_normal(res.ival))) {
    return ;
  }

  // generate testcase for verilog
  if (TESTCASE) {
    fprintf(fp, "%08x %08x %08x\n", a, b, res.ival);
  }

  if (! in_ulp(ref.ival, res.ival, 2)) {
    printf("a: %x, b: %x\n", a, b);
    printf("  expected: ");
    print_float(ref.ival);
    printf("  %f\n", ref.fval);

    printf("    actual: ");
    print_float(res.ival);
    printf("  %f\n", res.fval);

    if (TESTCASE) {
       exit(1);
    }
  }
}

int main(int argc, char *argv[])
{
  int i = 0, a, b = 0;
  FILE * fp, * f_out = fopen("fmul.vec", "w");
  
  char files[6][30] = {"random_minus.vec"};

  for (i = 0; i < 1; i++) {
    fp = fopen(files[i], "r");
    b = 0;
    while (fscanf(fp, "%x", &a) != EOF) {
      test(f_out, a, b);
      b = a;
    }
    fclose(fp);
  }

  fclose(f_out);
  return 0;
}
