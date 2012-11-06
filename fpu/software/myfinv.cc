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
#include "util.h"
#include "table.h"

//不一致として表示するulpの下限値
#define CHECKNUM 4
//1の時、テーブル初期化→最適化して実行
//0の時、テーブルをただ読んで実行
#define WRITABLE 1

//テーブル最適化用
struct rep {
  double s;         //分散スコア
  long long int p;  //k^2スコア
  int num4;         //±4ulpスコア
  int num5;         //±5ulp以上スコア
};


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
  // 桁は 45, k <= 423 のとき 46
  // 13 bit のこしてシフト => 誤りの最大値が 5 ulp に拡大
  return x*x >> 33;
}

unsigned int finv(unsigned a){
  int key = (a >> 13) & 0x3ff;
  int a1=MANTISSA(a)&((1<<13)-1);
  int e=EXP(a);

  D(printf("key: %x ", key));

  // 初期状態で 23 桁のみ
  ll b=const_table[key];

  D(printf("const: %x inc: %x lower: %llx ", const_table[key], inc_table[key], (ll)a1*(ll)inc_table[key]));

  b -= (a1*inc_table[key])>>13;

  D(printf("b:%llx ", b));

  // ここは適当かどうか自信がない
  int be = - e - 1;
  b<<=1;

  unsigned int answer;

  D(printf("be: %x\n", be + 127));

  answer = a&(1LL<<31LL);
  answer |= ((a & 0x7fffff) == 0 ? be + 128 : be+127)<<23;
  answer |= (a & 0x7fffff) == 0 ? 0 : b&((1<<23)-1);
  return answer;
}

int test(FILE * fp, unsigned int a, FILE * fn) {
  union IntAndFloat i, res, ref;
  int j;

  i.ival = a;

  ref.fval = 1 / i.fval;
  res.ival = finv(i.ival);


  if (! (is_normal(a) && is_normal(ref.ival))) {
    return 0;
  }

  // generate testcase for verilog
  if (TESTCASE) {
    fprintf(fp, "%08x %08x\n", a, res.ival);
  }

  j = ulp_signed(ref.ival, res.ival);
  if (! in_ulp(ref.ival, res.ival, CHECKNUM)) {
    fprintf(fn, "%08x\n", a & 0x007fffff);
    printf("a: %x\n", a);
    printf("  expected: ");
    print_float(ref.ival);
    printf("  %f\n", ref.fval);

    printf("    actual: ");
    print_float(res.ival);
    printf("  %f\n", res.fval);
    printf("upl %d\n",j);
    //if (TESTCASE) {
    //  exit(1);
    //}
  }
  return j;
}


struct rep sumDiff(int k0, unsigned int x_const_diff, unsigned int x_inc_diff)
{
  ll expected, actual;
  int tab[8192];
  fi input, answer;
  unsigned int a;
  double av = 0.0;
  int i;
  struct rep ans;
  ans.s    = 0.0;
  ans.p    = 0;
  ans.num4 = 0;
  ans.num5 = 0;

  const_table[k0] += x_const_diff;
  inc_table[k0] += x_inc_diff;

  for (i = 0; i < 8192; i++) {
    a = MAN_TO_FLOAT((k0 << 13) + i); 
    input.ival = a;
    answer.fval = 1 / input.fval;
    expected = answer.ival;
    actual = finv(a);
    tab[i] = ulp(expected,actual);
    if (tab[i]>=5)
      ans.num5++;
    else if (tab[i]>=4)
      ans.num4++;
    av += tab[i];
    ans.p += tab[i]*tab[i];
  }
  av /= 8192.0;
  for (i = 0; i < 8192; i++) 
    ans.s += (av - tab[i])*(av - tab[i]);
  
  ans.s /= 8192.0;
  
  const_table[k0] -= x_const_diff;
  inc_table[k0] -= x_inc_diff;

  return ans;
}

void opt_tables() {
  int i, p, q, op, oq;
  struct rep par;
  double mins = -1.0;
  ll minp = -1;
  int min4 = -1, ch[1024];
  for (i = 0; i < 1024; i++){
    mins = -1.0;
    minp = -1;
    min4 = -1; 
    for (p = -2; p <= 2; p++) {
      for (q = -2; q <= 2; q++) {
	par = sumDiff(i, p, q);
	if((minp<0 || minp>=par.p) && (mins<0.0 || mins>=par.s) &&
	      (min4<0 || min4>=par.num4) && par.num5==0) {
	  op = p;
	  oq = q;
	  minp = par.p;
	  mins = par.s;
	  min4 = par.num4;
	}
      }
    }
    const_table[i] += op;
    inc_table[i] += oq;
    if (min4 <= 0)
      ch[i]=0;
    else
      ch[i]=min4;
  }
  //±4ulpの誤差があった行について、詳細に調査
  for (i = 0; i < 1024; i++){
    if (ch[i]>0) {
      mins = -1.0;
      minp = -1;
      min4 = -1; 
      for (p = -5; p <= 5; p++) {
	for (q = -5; q <= 5; q++) {
	  par = sumDiff(i, p, q);
	  if((minp<0 || minp>=par.p) && (mins<0.0 || mins>=par.s) &&
	     (min4<0 || min4>=par.num4) && par.num5==0) {
	    op = p;
	    oq = q;
	    minp = par.p;
	    mins = par.s;
	  }
	}
      }
      const_table[i] += op;
      inc_table[i] += oq;
    }
  }
}
  



int main(int argc, char *argv[])
{
  FILE * fp, * f_out = fopen("finv.vec", "w");
  char files[6][30] = {"mytest.vec"};
  ui i, a, b;
  int check[1024][10], ch[10], j,k, w = 0;

  //ここからfdivのチェック用、終わったら消してよし
  FILE * fn = fopen("blacklist.vec", "w");
  //ここまで
  
  //エラーの起きた行
  for (j=0;j<10;j++) {
    for (k=0;k<1024;k++)
      check[k][j] = 0;
    ch[j] = 0;
  }

  if (WRITABLE) {
    initialize_tables();  
    write_tables();
    read_tables();
    opt_tables();//最適化
    write_tables();
  }
  read_tables();  
  printf("ok?\n");

  for (i = 0; i < 1; i++) {
    fp = fopen(files[i], "r");
    while (fscanf(fp, "%x", &a) != EOF) {
      b = (a & 0x007fe000) >> 13;	
      switch(j=test(f_out, a, fn)) {
      case 4:
	check[b][0]++;
	break;
      case 3:
	check[b][1]++;
	break;
      case 2:
	check[b][2]++;
	break;
      case 1:
	check[b][3]++;
	break;
      case 0:
	check[b][4]++;
	break;
      case -1:
	check[b][5]++;
	break;
      case -2:
	check[b][6]++;
	break;
      case -3:
	check[b][7]++;
	break;
      case -4:
	check[b][8]++;
	break;
      default:
	check[b][9]++;
      }
      w = max(w,abs(j));
    }
    
    for (j=0;j<10;j++) 
      for (k=0;k<1024;k++)
	ch[j] += check[k][j];
    


    printf("+4 ulp: %d\n",ch[0]);
    printf("+3 ulp: %d\n",ch[1]);
    printf("+2 ulp: %d\n",ch[2]);
    printf("+1 ulp: %d\n",ch[3]);
    printf(" 0 ulp: %d\n",ch[4]);
    printf("-1 ulp: %d\n",ch[5]);
    printf("-2 ulp: %d\n",ch[6]);
    printf("-3 ulp: %d\n",ch[7]);
    printf("-4 ulp: %d\n",ch[8]);

    printf("over +-5 ulp: %d\n",ch[9]);
    printf("Worst case: +-%d ulp\n",w);
    j=0;
    for(k=0;k<10;k++)
      j += ch[k];
    printf("total: %d\n",j);

    fclose(fp);
  }
  fp = fopen("rec.csv","w");
  fprintf(fp,"col 4 3 2 1 0 -1 -2 -3 -4 5\n");
  for (j=0;j<1024;j++) {
    fprintf(fp,"%04d: ",j);
    for (k=0;k<10;k++) 
      fprintf(fp,"%04d ",check[j][k]);
      fprintf(fp,"\n");
  }    
  return 0;
}
