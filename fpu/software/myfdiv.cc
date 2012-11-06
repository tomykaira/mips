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

using namespace std;

#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <limits.h>
#include <float.h>
#include <time.h>
#include "util.h"
#include "table.h"

//不一致として表示するulpの下限値
#define CHECKNUM 9

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

unsigned int table_const(unsigned int k) {
  ll x = generate_x(k);
  int a0 = MANTISSA(k << 13) >> 13;
  return 2*x-(a0*x*x>>33);
}

unsigned int table_inc(unsigned int k) {
  ll x = generate_x(k);
  return x*x >> 33;
}

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

unsigned int finv(unsigned a){
  int key = (a >> 13) & 0x3ff;
  int a1=MANTISSA(a)&((1<<13)-1);
  int e=EXP(a);

  // 初期状態で 23 桁のみ
  ll b=const_table[key];

  b -= (a1*inc_table[key])>>13;

  // ここは適当かどうか自信がない
  int be = - e - 1;
  b<<=1;

  unsigned int answer;

  answer = a&(1LL<<31LL);
  answer |= ((a & 0x7fffff) == 0 ? be + 128 : be+127)<<23;
  answer |= (a & 0x7fffff) == 0 ? 0 : b&((1<<23)-1);
  return answer;
}

int test(unsigned int a, unsigned int b) {
  union IntAndFloat i, j, res, ref;
  int k,p,q;

  i.ival = a;
  j.ival = b;

  ref.fval = i.fval / j.fval;
  res.ival = fmul(i.ival, finv(j.ival));

  if (! (is_normal(a))) {
    return -2555;
  }
  if (! (is_normal(b) && is_normal(finv(b)))) {
    return -2554;
  }
  if (! (is_normal(ref.ival))) {
    return -2553;
  }
  if (! (b!=0 && finv(b)!=0)) {
    return -2552;
  }

  //A/Bがでかい場合は除きたい
  p = (0x0ff) & (i.ival >> 23); //a指数部
  q = (0x0ff) & (j.ival >> 23); //b指数部
  if (( p-q < -127 ) || ( p-q > 127 )) {
    return -2551;
  }
  
  k = ulp_signed(ref.ival, res.ival);
  if (! in_ulp(ref.ival, res.ival, CHECKNUM)) {
    printf("a: %x, b: %x\n", a, b);
    printf("  expected: ");
    print_float(ref.ival);
    printf("  %f\n", ref.fval);
    
    printf("    actual: ");
    print_float(res.ival);
    printf("  %f\n", res.fval);
    printf("upl %d\n",k);
  }
  return k;
}

void chcount(ui ch[], ui err[], int j) {
  switch (j) {
  case -2551:
  case -2552:
  case -2553:
  case -2554:
  case -2555:
    err[j+2555]++;
    break;
  case 8:
  case 7:
  case 6:
  case 5:
  case 4:
  case 3:
  case 2:
  case 1:
  case 0:
  case -1:
  case -2:
  case -3:
  case -4:
  case -5:
  case -6:
    case -7:
  case -8:
    ch[8-j]++;
    break;
  default:
    ch[17]++;
  }
  return;
}


int main(void) {
  FILE *fp1, *fp2;
  ui a,b,c,step;
  ui BL[675], ch[18] = {0}, err[5] = {0};
  int i,j,w=0,p,q;
  srand((unsigned)time(NULL));
  //誤差の大きかった仮数部のパターンを抽出
  FILE * f_lis = fopen("blacklist.vec", "r");
  if (f_lis) {
    for (i=0; i<675; i++) {
      fscanf(f_lis, "%x\n", &BL[i]);
    }
    if (fclose(f_lis) != 0) {
      perror("fclose blacklist.vec");
      return 1;
    }
  }
  char files1[6][30] = {"random.vec"};
  char files2[6][30] = {"random_minus.vec"};
  read_tables();
  fp1 = fopen(files1[0], "r");
  fp2 = fopen(files2[0], "r");
  p=0;
  step=0;
  while (fscanf(fp1, "%x", &a) != EOF) {
    b = rand()%511;
    i = rand()%675;
    c = (b << 23) + BL[i];
    q=0;
    while(!( is_normal(c) && is_normal(finv(c)) && 
	     c!=0 && finv(c)!=0) && q<10) {     
      b = rand()%511;
      i = rand()%675;
      c = (b << 23) + BL[i];
      q++;
    }
    j=test(a,c);
    chcount(ch,err,j);
    if (j < -2555 || j > -2551)
      w = max(w,abs(j));
    p++;
    if(p%1000000==0) {
      step++;
      p=0;
      printf("%d000000.(1 of 2)\n",step);
    } 
  }
  p=0;
  step=0;
  while (fscanf(fp2, "%x", &a) != EOF) {
    b = ((a >> 23) & 0x0ff);
    if (b <= 230)
      b += rand()%20;
    else 
      b -= rand()%20;
    if (b==0) b += rand()%10;
    i = rand()%675;
    c = (b << 23) + BL[i];
    q=0;
    while(!( is_normal(c) && is_normal(finv(c)) && 
	     c!=0 && finv(c)!=0) && q<10) {     
      i = rand()%675;
      c = (b << 23) + BL[i];
      q++;
    }
    j=test(a,c);
    chcount(ch,err,j);
    if (j < -2555 || j > -2551)
      w = max(w,abs(j));
    p++;
    if(p%1000000==0) {
      step++;
      p=0;
      printf("%d000000.(2 of 2)\n",step);
    } 
  }
  fclose(fp1);
  fclose(fp2);
  printf("Test cases are all finished:\n");
  for (i=8;i>0;i--)
    printf("+%d ulp: %d\n",i,ch[8-i]);
  printf(" 0 ulp: %d\n",ch[8]);
  for (i=1;i<9;i++)
    printf("-%d ulp: %d\n",i,ch[8+i]);
  printf("\nignored:\n");
  printf("a is denormalized: %d\n",err[0]);
  printf("b or finv(b) are denormalized: %d\n",err[1]);
  printf("answer is denormalized: %d\n",err[2]);
  printf("b or finv(b) are 0: %d\n",err[3]);
  printf("exponential is too large: %d\n",err[4]);
  printf("over +-9 ulp: %d\n",ch[17]);
  printf("Worst case: +-%d ulp\n",w);
  j=0;
  for(i=0;i<18;i++)
    j += ch[i];
  for(i=0;i<5;i++)
    j += err[i];
  printf("total: %d\n",j);
  return 0;
}
