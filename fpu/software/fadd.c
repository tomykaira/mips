#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <limits.h>
#include <float.h>
#include "util.h"

unsigned int fadd(unsigned int a, unsigned int b)
{
  unsigned int ae = (a >> 23) & 0xff, be = (b >> 23) & 0xff, diff, sig = 0, msb;
  int se;
  unsigned long long am, bm, sm, sm_orig, x;
  D(printf("%x, ", a));
  D(printf("%x, ", b));
  // 0 の扱いはどうにかしたい
  if ((a & 0x7f800000) == 0x7f800000 && a & 0x007fffff) { return a; } // NaN
  if ((b & 0x7f800000) == 0x7f800000 && b & 0x007fffff) { return b; } // NaN
  if (a == 0 && b == 0x80000000) { return 0; }
  if (a == 0 || a == 0x80000000) { return b; }
  if (b == 0 || b == 0x80000000) { return a; }
  if ((a == 0x7f800000 && b == 0xff800000) 
      || (b == 0x7f800000 && a == 0xff800000)) { return 0xffc00000; }
  if (be > ae) {
    swap(ae, be);
    swap(a, b);
  }
  D(printf("be: %x, ", be));
  // 非正規化数に対処
  if (ae == 0) {
    ae = 1;
    am = (a & 0x7fffff);
  } else {
    am = ((a & 0x7fffff) + 0x800000);
  }

  if (be == 0) {
    be = 1;
    bm = (b & 0x7fffff);
  } else {
    bm = ((b & 0x7fffff) + 0x800000);
  }

  diff = ae - be;
  if (diff > 24) { return a; } // ケタの差がありすぎると計算不能

  D(printf("%llx, %llx, ", am << diff, bm));
  se = be;
  sm = (am << diff) * (a >> 31 ? -1 : 1) + bm * (b >> 31 ? -1 : 1);
  D(printf("-a : %llx -> %llx, ", am, (am << diff) * (a >> 31 ? -1 : 1)));
  D(printf("%016llx, ", sm));
  if (sm == 0) {
    D(printf("0, "));
    return 0;
  }

  if ((sm >> 63) == 1) {
    sig = 1;
    sm = - sm;
  }

  sm_orig = sm;

  // overflow??
  msb = 63;
  while ((sm >> msb) == 0) {
    msb --;
  }
  se = be + msb - 23;
  // sm を 23 桁で頭出し
  sm = msb > 23 ? (sm >> (msb - 23)) : (sm << (23 - msb));
  // underflow
  if (se <= 0) {
    return (sig << 31) + (sm >> (1-se));
  }
  if (se >= 255) {
    return (sig << 31) + (0xff << 23);
  }
  
  x = sm_orig & ((1 << (se - be))-1);
  if (x > (1 << (se - be - 1))
      || (x ==  (1 << (se - be - 1)) && ((sm) & 0x1) == 1)) {
    sm += 1;
  }
  if ((sm >> 23) > 1) {
    sm = sm >> 1;
    se++;
  }
  D(printf("se: %x, sm: %llx, ", se, sm & 0x7fffff));
  
  return (sig << 31) + (se << 23) + (sm & 0x7fffff);
}

void test(FILE * fp, int a, int b)
{
  fi i, v, res, ref;
  i.ival = a;
  v.ival = b;

  ref.fval = i.fval + v.fval;

  res.ival = fadd(i.ival, v.ival);

  if (! (is_normal(a) && is_normal(b) && is_normal(ref.ival) && is_normal(res.ival))) {
    return ;
  }

  if (TESTCASE) {
    fprintf(fp, "%08x %08x %08x\n", i.ival, v.ival, res.ival);
  }

  if (! in_ulp(ref.ival, res.ival, 1)) {
    printf("a: %x, b: %x\n", a, b);
    printf("  expected: ");
    print_float(ref.ival);
    printf("\n");

    printf("    actual: ");
    print_float(res.ival);
    printf("\n");
    if (TESTCASE) {
       exit(1);
    }
  }
}

int main(int argc, char *argv[])
{
  int i = 0, a, b = 0;
  FILE * fp, * f_out = fopen("fadd.vec", "w");
  
  char files[6][30] = {"corners.vec", "exponent_minus.vec", "exponent.vec", "mantissa.vec", "random_minus.vec", "random.vec"};

  for (i = 0; i < 6; i++) {
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
