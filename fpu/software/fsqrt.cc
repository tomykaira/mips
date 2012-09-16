#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <climits>
#include <cfloat>
#include <map>
#include <utility>
#include <set>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <algorithm>
#include <cassert>

using namespace std;

#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <limits.h>
#include <float.h>
#include "util.h"
#include "table.h"

#define KEY_TO_FLOAT(x) ((((x) ^ (1 << 9)) << 14) + (127 << 23))

unsigned int generate_x(unsigned int a) {
  fi x,aa;
  aa.ival = KEY_TO_FLOAT(a);
  aa.ival &= ~F(14);
  x.fval=sqrt(aa.fval);
  aa.ival |= F(14);
  x.fval+=sqrt(aa.fval);
  x.fval/=2;

  return MANTISSA(x.ival);
}

unsigned int table_const(unsigned int key) {
  fi x;

  int a0 = (key & (1 << 9)) ? key >> 1 : ((key | (1 << 9)));
  x.ival = MAN_TO_FLOAT(generate_x(key));
  float x2 = MANTISSA(x.ival) / 2.0f;
  float a2x = (float)(a0 << 15) / x.fval / 2.0f;
  float constant = x2 + a2x;
  return (ui)constant >> 1; // 23 bit
}

unsigned int table_inc(unsigned int key) {
  fi x;
  x.ival = MAN_TO_FLOAT(generate_x(key));
  ll inc = 16384.0f / x.fval / 2.0f; // 16384 = (1 << 14)
  return inc; // 13 bit
}

unsigned int fsqrt(unsigned a){
  assert(! (a&0x80000000)); // not minus

  fi answer;
  int key = (a >> 14) & F(10);
  ll a1 = ((a & (1 << 23)) ? MANTISSA(a) : MANTISSA(a) << 1) & F(15);

  D(printf("a1: %llx, inc: %x, ", a1, inc_table[key]));

  ui i_constant = const_table[key] << 1;
  ui diff = (a1 * inc_table[key]) >> 14;

  D(printf("constant: %x, raw: %llx, diff: %x, ", i_constant, a1 * inc_table[key], diff));

  ll mantissa = i_constant + diff;
  int exponent = (63 + ((((a >> 23)&F(8)) + 1) >> 1));

  D(printf("mantissa: 0x%llx exponent: 0x%x", mantissa, exponent));

  answer.ival = (exponent << 23) + (mantissa & F(23));

  D(printf("%f\n", answer.fval));

  return answer.ival;
}

void test(FILE * fp, unsigned int a) {
  union IntAndFloat i, res, ref;

  i.ival = a;

  ref.fval = sqrt(i.fval);

  if (! (is_normal(a) && is_normal(ref.ival))) {
    return ;
  }

  res.ival = fsqrt(i.ival);

  // generate testcase for verilog
  if (TESTCASE) {
    fprintf(fp, "%08x %08x\n", a, res.ival);
  }

  if (! in_ulp(ref.ival, res.ival, 8)) {
    printf("a: %x\n", a);
    printf("  expected: ");
    print_float(ref.ival);
    printf("  %f\n", ref.fval);

    printf("    actual: ");
    print_float(res.ival);
    printf("  %f\n", res.fval);
    printf("ulp %d\n", ulp(ref.ival, res.ival));
    if (TESTCASE) {
      exit(1);
    }
  }
}

ll sumDiff(int k0, unsigned x_const_diff, unsigned int x_inc_diff)
{
  ll diff = 0;
  fi input, answer;
  unsigned int a;

  const_table[k0] += x_const_diff;
  inc_table[k0] += x_inc_diff;

  for (int i = 0; i < (1 << 13) - 1; i ++) {
    a = MAN_TO_FLOAT((k0 << 13) + i);
    input.ival = a;

    answer.fval = sqrt(input.fval);
    ll expected = answer.ival;
    ll actual = fsqrt(a);

    diff += (expected - actual)*(expected - actual);
    if (diff > (1ll << 31ll)) {
      break;
    }
  }

  const_table[k0] -= x_const_diff;
  inc_table[k0] -= x_inc_diff;

  return diff;
}

int main(int argc, char *argv[])
{
  int i = 0, a;
  FILE * fp, * f_out = fopen("fsqrt.vec", "w");
  
  char files[6][30] = {"random.vec"};

  initialize_tables();

  for (i = 0; i < 1; i++) {
    fp = fopen(files[i], "r");
    while (fscanf(fp, "%x", &a) != EOF) {
      test(f_out, a);
    }
    fclose(fp);
  }

  fclose(f_out);
  return 0;
}
