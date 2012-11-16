#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "fpu.h"

#define swap(a,b) { int temp = a; a = b; b = temp; }
#define NOT_IMPLEMENTED(inst_name) {  fprintf(stderr, "%s is not hardware implemented.\n", inst_name); }
#define DEBUG(x) { ; }
#define MANTISSA(x) (0x800000 + (x & 0x7fffff))
#define EXP(x) ((int)((x & 0x7f800000) >> 23)-127)
#define MAN_TO_FLOAT(x) ((127 << 23) + ((x) & 0x7fffff))
#define F(x) ((1 << x) - 1)
#define MANTISSA_ONLY(x) ((((x) & F(23)) + (127 << 23)))

// table
#define CONST(table) ((table) >> 13)
#define INC(table) ((table) & F(13))
#define MAX_KEY 1024

typedef long long ll;
typedef unsigned long long ull;
typedef unsigned int ui;

ull finv_table[MAX_KEY];
ull fsqrt_table[MAX_KEY];

static int initialized = 0;

void load_tables(const char * dirpath)
{
	char finv_path[255], fsqrt_path[255];
	strcpy(finv_path, dirpath);
	strcat(finv_path, "/finv.dat");
	strcpy(fsqrt_path, dirpath);
	strcat(fsqrt_path, "/fsqrt.dat");
	if (initialized) return;
	initialized = 1;
  // not easy to use relative path in C
  FILE * fp = fopen(finv_path, "r");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      if (fscanf(fp, "%llx\n", &finv_table[i]) == EOF) {
	      fprintf(stderr, "Not enough finv table\n");
      }
    }
    if (fclose(fp) != 0) {
      perror("fclose finv.dat");
      exit(1);
    }
  } else {
    perror("fopen finv.dat");
    exit(1);
  }

  fp = fopen(fsqrt_path, "r");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      if (fscanf(fp, "%llx\n", &fsqrt_table[i]) == EOF) {
	      fprintf(stderr, "Not enough finv table\n");
      }
    }
    if (fclose(fp) != 0) {
      perror("fclose fsqrt.dat");
      exit(1);
    }
  } else {
    perror("fopen fsqrt.dat");
    exit(1);
  }
}

uint32_t myfadd(uint32_t rs, uint32_t rt)
{
  unsigned int a = rs;
  unsigned int b = rt;
  unsigned int ae = (a >> 23) & 0xff, be = (b >> 23) & 0xff, diff, sig = 0, msb;
  int se;
  unsigned long long am, bm, sm, sm_orig, x;
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

  se = be;
  sm = (am << diff) * (a >> 31 ? -1 : 1) + bm * (b >> 31 ? -1 : 1);
  if (sm == 0) {
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

  uint32_t answer = (sig << 31) + (se << 23) + (sm & 0x7fffff);

  DEBUG(printf("fadd %x %x %x\n", rs, rt, answer));
  return answer;
}

uint32_t myfsub(uint32_t rs, uint32_t rt)
{
  return myfadd(rs, rt ^ 0x80000000);
}

uint32_t myfmul(uint32_t rs, uint32_t rt)
{
  unsigned int a = rs, b = rt;
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

  uint32_t answer = (sign << 31) + (is_zero ? 0 : (exp << 23) + (m & 0x7fffff));
	DEBUG(printf("fmul %x %x %x\n", rs, rt, answer));
  return answer;
}

uint32_t myfinv(uint32_t rs)
{
	assert(initialized);
  conv c, s;
  c.i = rs;
  s.f = 1 / c.f;

  unsigned int a = rs;
  int key = (a >> 13) & 0x3ff;
  int a1=MANTISSA(a)&((1<<13)-1);
  int e=EXP(a);

  // 初期状態で 23 桁のみ
  ll b = CONST(finv_table[key]);

  b -= (a1*INC(finv_table[key]))>>13;

  // ここは適当かどうか自信がない
  int be = - e - 1;
  b<<=1;

  unsigned int answer;

  answer = a&(1LL<<31LL);
  answer |= ((a & 0x7fffff) == 0 ? be + 128 : be+127)<<23;
  answer |= (a & 0x7fffff) == 0 ? 0 : b&((1<<23)-1);

  if (!(abs((signed)s.i - (signed)answer) < 8)) {
	  fprintf(stderr, "finv %d should %d but answer %d\n", rs, s.i, answer);
  }

  DEBUG(printf("finv %x %x\n", rs, answer));
  return answer;
}

uint32_t myfdiv(uint32_t rs, uint32_t rt)
{
	return myfmul(rs, myfinv(rt));
}

uint32_t myfsqrt(uint32_t rs)
{
	assert(initialized);
  conv c, s;
  c.i = rs;
  s.f = sqrt(c.f);

  unsigned int a = rs;
  assert(! (a&0x80000000)); // not minus

  unsigned int answer;
  int key = (a >> 14) & F(10);
  ll a1 = ((a & (1 << 23)) ? MANTISSA(a) : MANTISSA(a) << 1) & F(15);

  ui i_constant = CONST(fsqrt_table[key]) << 1;
  ui diff = (a1 * INC(fsqrt_table[key])) >> 14;

  ll mantissa = i_constant + diff;
  int exponent = (63 + ((((a >> 23)&F(8)) + 1) >> 1));

  answer = (exponent << 23) + (mantissa & F(23));

  if (!(abs((signed)s.i - (signed)answer) < 8)) {
	  fprintf(stderr, "fsqrt %d should %d but answer %d\n", rs, s.i, answer);
  }

  DEBUG(printf("fsqrt %x %x\n", rs, answer));
  return answer;
}

uint32_t myfabs(uint32_t rs)
{
  // NOT_IMPLEMENTED("fabs");
  if (rs & 0x80000000)
	  return myfneg(rs);
  else
	  return rs;
}
uint32_t myfneg(uint32_t rs)
{
  return rs ^ 0x80000000;
}
uint32_t myfloor(uint32_t rs)
{
  NOT_IMPLEMENTED("floor");
  conv a, b;
  a.i = rs;
  b.f = floor(a.f);
  return b.i;
}
uint32_t myfsin(uint32_t rs)
{
  NOT_IMPLEMENTED("sin");
  conv a, b;
  a.i = rs;
  b.f = sin(a.f);
  return b.i;
}
uint32_t myfcos(uint32_t rs)
{
  NOT_IMPLEMENTED("cos");
  conv a, b;
  a.i = rs;
  b.f = cos(a.f);
  return b.i;
}
uint32_t myftan(uint32_t rs)
{
  NOT_IMPLEMENTED("tan");
  conv a, b;
  a.i = rs;
  b.f = tan(a.f);
  return b.i;
}
uint32_t myfatan(uint32_t rs)
{
  NOT_IMPLEMENTED("atan");
  conv a, b;
  a.i = rs;
  b.f = atan(a.f);
  return b.i;
}
