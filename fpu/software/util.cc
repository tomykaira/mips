#include <cstdio>
#include <cstdlib>
#include <algorithm>
#include "util.h"

using namespace std;

void print_float(ui x) {
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

unsigned int place(ll x) {
  int place = 0;
  while (x > 0) {
    x >>= 1;
    place ++;
  }
  return place;
}

int is_normal(ui floating) {
  // 0
  if (floating == 0)
    return 1;

  // -0
  if (floating == 0x80000000)
    return 0;

  // Inf or NaN
  if (((floating >> 23) & F(8)) == 0xff)
    return 0;

  // denormalized
  if (((floating >> 23) & F(8)) == 0)
    return 0;

  return 1;
}

int ulp(ui expected, ui actual) {
  return max(expected, actual) - min(expected, actual);
}

int ulp_signed(ui expected, ui actual) {
  return actual - expected;
}

int in_ulp(ui expected, ui actual, int max_ulp) {
  return ulp(expected, actual) < max_ulp;
}
