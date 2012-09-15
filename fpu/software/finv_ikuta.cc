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
#include <functional>
#include <sstream>
#include <complex>
#include <stack>
#include <queue>
#include <cstring>
#include <sstream>
#include <cassert>
#include <ctime>
#include <list>
#include <numeric>
#include <iomanip>


using namespace std;
static const double EPS = 1e-6;
typedef long long ll;
typedef unsigned long long ull;
typedef pair<int,int> PI;
#ifndef M_PI
const double M_PI=acos(-1);
#endif
#define rep(i,n) for(int i=0;i<(int)(n);++i)
#define FOR(i,c) for(__typeof((c).begin())i=(c).begin();i!=(c).end();++i)
#define ALL(c) (c).begin(), (c).end()
#define mp(a,b) make_pair(a,b)
#define pb(a) push_back(a)
#define SZ(a) (int((a).size()))
#define F first
#define S second
int dx[]={0,1,0,-1,1,1,-1,-1},dy[]={1,0,-1,0,1,-1,1,-1};


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
#define EXP(x) ((int)((x & 0x7f800000) >> 23)-127)
#define MAN_TO_FLOAT(x) ((127 << 23) + ((x) & 0x7fffff))
#define KEYLEN 10

union IntAndFloat {
    unsigned int ival;
    float fval;
};

typedef IntAndFloat fi;

#define MAX_KEY 1024

unsigned int const_table[MAX_KEY];
unsigned int inc_table[MAX_KEY];

int read_tables() {
  FILE * fp = fopen("const.dat", "r");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      fscanf(fp, "%x\n", &const_table[i]);
    }
    if (fclose(fp) != 0) {
      perror("fclose const.dat");
      return 1;
    }
  } else {
    perror("fopen const.dat");
    return 1;
  }

  fp = fopen("inc.dat", "r");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      fscanf(fp, "%x\n", &inc_table[i]);
    }
    if (fclose(fp) != 0) {
      perror("fclose inc.dat");
      return 1;
    }
  } else {
    perror("fopen inc.dat");
    return 1;
  }

  return 0;
}

int write_tables() {
  FILE * fp = fopen("const.dat", "w");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      fprintf(fp, "%x\n", const_table[i]);
    }
    if (fclose(fp) != 0) {
      perror("fclose const.dat");
      return 1;
    }
  } else {
    perror("fopen const.dat");
    return 1;
  }

  fp = fopen("inc.dat", "w");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      fprintf(fp, "%x\n", inc_table[i]);
    }
    if (fclose(fp) != 0) {
      perror("fclose inc.dat");
      return 1;
    }
  } else {
    perror("fopen inc.dat");
    return 1;
  }

  return 0;
}

unsigned int generate_x(unsigned int a) {
  fi x,aa;
  aa.ival=MAN_TO_FLOAT(a << 13);
  aa.ival &= ~((1<<13)-1);
  x.fval=1/aa.fval;
  aa.ival |=(1<<13)-1;
  x.fval+=1/aa.fval;
  x.fval/=2;

  return MANTISSA(x.ival) >> 1;
}

unsigned int place(ll x) {
  int place = 0;
  while (x > 0) {
    x >>= 1;
    place ++;
  }
  return place;
}

unsigned int table_const(unsigned int k) {
  assert(k<MAX_KEY);
  return const_table[k];
}

ll table_inc(unsigned int k) {
  assert(k<MAX_KEY);
  return inc_table[k];
}

void initialize_tables() {
  for (int i = 0; i<MAX_KEY; i++) {
    const_table[i] = table_const(i);
    inc_table[i] = table_inc(i);
  } 
}

unsigned int finv(unsigned a){
  int key = (a >> 13) & 0x3ff;
  int a1=MANTISSA(a)&((1<<13)-1);
  int e=EXP(a);

  // 初期状態で 23 桁のみ
  ll b=table_const(key);

  b -= (a1*table_inc(key))>>13;

  // ここは適当かどうか自信がない
  int be = - e - 1;
  b<<=1;

  unsigned int answer;

  answer = a&(1LL<<31LL);
  answer |= (be+127)<<23;
  answer |= b&((1<<23)-1);
  return answer;
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

  i.ival = a;

  res2.fval = 1 / i.fval;
  // do not test inf, nan.
  int exp = (a >> 23) & 0xff;
  int answer_exp = (res2.ival >> 23) & 0xff;
  if (exp == 0xff || exp == 0 || answer_exp == 0xff || answer_exp == 0) { return; }

  res.ival = finv(i.ival);

  // generate testcase for verilog
  if (TESTCASE) {
    printf("%08x\n%08x\n", a, res.ival);
  }

  if (!DEBUG &&
      max(res.ival,res2.ival) - min(res.ival,res2.ival) < 6) {
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

    answer.fval = 1 / input.fval;
    ll expected = answer.ival;
    ll actual = finv(a);

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
  ll diff, diff_p1, diff_m1;
  read_tables();

  for (int k0 = 0; k0<1024; k0++) {
    while (1) {
      diff = sumDiff(k0, 0, 0);
      diff_p1 = sumDiff(k0, 0, 1);
      diff_m1 = sumDiff(k0, 0, -1);
      printf("%lld %lld %lld\n", diff, diff_p1, diff_m1);

      if (diff_m1 < diff)
        inc_table[k0] -= 1;
      else if (diff_p1 < diff)
        inc_table[k0] += 1;
      else
        break;
    }
  }

  return write_tables();
}
