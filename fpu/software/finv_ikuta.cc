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
  ll x = generate_x(k);
  int a0 = MANTISSA(k << 13) >> 13;
  return 2*x-(a0*x*x>>33);
}

ll table_inc(unsigned int k) {
  ll x = generate_x(k);
  // 桁は 45, k <= 423 のとき 46
  // 13 bit のこしてシフト => 誤りの最大値が 5 ulp に拡大
  return x*x >> 33;
}

unsigned int finv(unsigned a){
  int key = (a >> 13) & 0x3ff;
  int a1=MANTISSA(a)&(1<<13)-1;
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
  answer |= b&(1<<23)-1;
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

int main(int argc, char *argv[])
{
  union IntAndFloat input;

  for (int i = 1; i < 0xff; i++) {
    test((i << 23) + (0x712900));
  }

  for (int i = 0; i < (1 << 23) - 1; i ++) {
    test(MAN_TO_FLOAT(i));
  }

  return 0;
}
