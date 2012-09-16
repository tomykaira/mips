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

unsigned int table_const(unsigned int k) {
  ll x = generate_x(k);
  ll a0 = MANTISSA(k << 14) >> 13;
  return 2*x-(a0*x*x>>33);
}

unsigned int table_inc(unsigned int k) {
  fi x, rev;
  ui inc;
  x.ival = MAN_TO_FLOAT(generate_x(k));
  rev.fval = 1 / x.fval;
  inc = MANTISSA(rev.ival); // 24 bit
  inc >>= 2;
  return inc >> 9; // 13 bit
}

unsigned int fsqrt(unsigned a){
  assert(! (a&0x80000000)); // not minus

  fi x, answer;
  int key = (a >> 14) & F(10);
  int man = (a & (1 << 23)) ? MANTISSA(a) : MANTISSA(a) << 1;
  ll a0 = man >> 14, a1 = man & F(14);

  x.ival = MAN_TO_FLOAT(generate_x(key));

  float x2 = MANTISSA(x.ival) / 2.0f;
  float a2x = (float)(a0 << 14) / x.fval / 2.0f;
  D(printf("x: %f, 1:%f, 2:%f\n", x.fval, x2, a2x));
  float constant = x2 + a2x;
  float diff = (float)a1 / x.fval / 2.0;

  D(printf("%f, %f\n", constant, diff));

  ll mantissa = (ll)constant + (ll)diff;

  D(printf("0x%llx %lld\n", mantissa, mantissa));

  answer.ival = ((63 + ((((a >> 23)&F(8)) + 1) >> 1)) << 23) + (mantissa & F(23));

  D(printf("%f\n", answer.fval));

  return answer.ival;
}

void test(unsigned int a) {
  union IntAndFloat i, res, res2;

  i.ival = a;

  res2.fval = sqrt(i.fval);
  // do not test inf, nan.
  int exp = (a >> 23) & 0xff;
  int answer_exp = (res2.ival >> 23) & 0xff;
  if (exp == 0xff || exp == 0 || answer_exp == 0xff || answer_exp == 0) { return; }

  res.ival = fsqrt(i.ival);

  // generate testcase for verilog
  if (TESTCASE) {
    printf("%08x\n%08x\n", a, res.ival);
  }

  if (!DEBUG &&
      max(res.ival,res2.ival) - min(res.ival,res2.ival) < 8) {
    if (DOTS) {printf(".");}
  } else {
    printf("a: %x\n", a);
    printf("  expected: ");
    print_float(res2.ival);
    printf("\n");

    printf("    actual: ");
    print_float(res.ival);
    printf("\n");
    printf("upl %d\n",max(res.ival,res2.ival) - min(res.ival,res2.ival));
    // exit(1);
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
  fi input;
  initialize_tables();

  // while (scanf("%f", &input.fval) != EOF) {
  //   test(input.ival);
  // }

  for (int i = 1; i < 0xff; i++) {
    test((i << 23) + (0x712900));
  }

  for (int i = 0; i < (1 << 23) - 1; i ++) {
    test(MAN_TO_FLOAT(i));
  }

  return write_tables();
}
