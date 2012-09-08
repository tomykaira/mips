#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <limits.h>
#include <float.h>

union IntAndFloat {
    unsigned int ival;
    float fval;
};

unsigned int fmul(unsigned int a, unsigned int b)
{
  return 0;
}
