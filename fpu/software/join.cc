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

#define F(x) ((1 << x) - 1)

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

int join_tables() {
  FILE * fp = fopen("finv_join.dat", "w");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      assert((const_table[i] & F(23)) == const_table[i]);
      assert((inc_table[i] & F(13)) == inc_table[i]);
      fprintf(fp, "%llx\n", (((ull)const_table[i] & F(23ll)) << 13ll) + (inc_table[i] & F(13)));
    }
    if (fclose(fp) != 0) {
      perror("fclose finv_join.dat");
      return 1;
    }
  } else {
    perror("fopen finv_join.dat");
    return 1;
  }

  return 0;
}
int main(int argc, char *argv[])
{
  read_tables();

  return join_tables();
}
