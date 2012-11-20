/*
 *      Fast log(x) computation
 *
 * Copyright (c) 2010, Naoaki Okazaki
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the names of the authors nor the names of its contributors
 *       may be used to endorse or promote products derived from this
 *       software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*

from http://www.chokkan.org/blog/archives/352

To compile this code using gcc:
gcc -o fastexp -O3 -fomit-frame-pointer -msse2 -mfpmath=sse -ffast-math -lm fastexp.c

To compile this code using Microsoft Visual C++ 2008 (or later):
Simply create a console project, and add this file to the project.

*/

#include <math.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

union IntAndFloat {
    unsigned int i;
    float f;
};


/*
    Useful macro definitions for memory alignment:
        http://homepage1.nifty.com/herumi/prog/gcc-and-vc.html#MIE_ALIGN       
 */

#ifdef _MSC_VER
#define MIE_ALIGN(x) __declspec(align(x))
#else
#define MIE_ALIGN(x) __attribute__((aligned(x)))
#endif

#ifdef _MSC_VER
    #include <malloc.h>
#else
    #include <stdlib.h>
    static inline void *_aligned_malloc(size_t size, size_t alignment)
    {
        void *p;
        int ret = posix_memalign(&p, alignment, size);
        return (ret == 0) ? p : 0;
    }
#endif

#define CONST_128D(var, val) \
    MIE_ALIGN(16) static const float var[2] = {(val), (val)}
#define CONST_128I(var, v1, v2, v3, v4) \
    MIE_ALIGN(16) static const int var[4] = {(v1), (v2), (v3), (v4)}

typedef struct {
    const char *name;
    void (*func)(float *values, int n);
    float error_peak;
    float error_rms;
    clock_t elapsed_time;
    float *values;
} performance_t;

static const float MAXLOG =  7.08396418532264106224E2;     /* log 2**1022 */
static const float MINLOG = -7.08396418532264106224E2;     /* log 2**-1022 */
static const float LOG2E  =  1.4426950408889634073599;     /* 1/log(2) */
static const float C1 = 6.93145751953125E-1;
static const float C2 = 1.42860682030941723212E-6;
#define E (2.7182818284590)

void print_hex(float x)
{
    union IntAndFloat u;
    u.f = x;
    printf("%X\n", u.i);
}

/*
  Too much error:
  libc    0.000000        0.000000e+00    0.000000e+00
  Remez 5th [1, e]        0.000000        2.449893e-05    8.815392e-06
*/

void remez5_1_e(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        int y = 0;
        float a, px, x = values[i];

        if (x <= 0)
            continue;
        /* log (x * e^y) = log x + y */
        while (x < 1) {
            y -= 1;
            x *= E;
        }

        while (x > E) {
            y += 1;
            x /= E;
        }

/*
  from lolremez

  x**0*-1.792326254
  +x**1*3.021214659
  +x**2*-1.779754431
  +x**3*6.818078915e-1
  +x**4*-1.434572234e-1
  +x**5*1.259063141e-2

               -1.792326254
  +x         *  3.021214659
  +x*x       * -1.779754431
  +x*x*x     *  6.818078915e-1
  +x*x*x*x   * -1.434572234e-1
  +x*x*x*x*x *  1.259063141e-2
*/
        /* Compute e^x using a polynomial approximation. */
        a = 1.259063141e-2;
        a *= x;
        a += -1.434572234e-1;
        a *= x;
        a += 6.818078915e-1;
        a *= x;
        a += -1.779754431;
        a *= x;
        a += 3.021214659;
        a *= x;
        a += -1.792326254;

        values[i] = a + (float)y;
        print_hex(values[i]);
    }
}

void veclog_libc(float *values, int n)
{
    int i;
    for (i = 0;i < n;++i) {
        values[i] = logf(values[i]);
    }
}

float *read_source(FILE *fp, int *num)
{
    int n = 0;
    char line[1024];
    float *values = NULL;

    while (fgets(line, 1023, fp) != NULL) {
        values = realloc(values, sizeof(float) * (n+1));
        values[n++] = atof(line);
    }

    *num = n;
    return values;
}

void measure(performance_t *perf, float *values, int n)
{
    int i;
    performance_t *p;

    for (p = perf;p->func != NULL;++p) {
        p->values = (float*)_aligned_malloc(sizeof(float) * n, 16);
        for (i = 0;i < n;++i) {
            p->values[i] = values[i];
        }
    }

    for (p = perf;p->func != NULL;++p) {
        p->elapsed_time = clock();
        p->func(p->values, n);
        p->elapsed_time = clock() - p->elapsed_time;
    }

    for (p = perf;p->func != NULL;++p) {
        for (i = 0;i < n;++i) {
            float ex = perf[0].values[i];
            float exf = p->values[i];

            // printf("%s: %f -> %f %f\n", p->name, values[i], ex, exf);
            float err = fabs(exf - ex) / ex;
            if (p->error_peak < err) {
                p->error_peak = err;
            }
            p->error_rms += (err * err);
        }

        p->error_rms /= n;
        p->error_rms = sqrt(p->error_rms);
    }
}

float random_float()
{
    int exp = 0;
    union IntAndFloat u;
    while (exp <= 117 || exp >= 130) { exp = (rand() & 0xff); }
    u.i = ((rand() % 2) << 31) + (exp << 23) + (rand() & 0x7fffff);
    return u.f;
}

int main(int argc, char *argv[])
{
    int n;
    float *values = NULL;
    performance_t *p = NULL;

    performance_t perf[] = {
        {"libc", veclog_libc, 0., 0., 0, NULL},
        {"Remez 5th [1, e]", remez5_1_e, 0., 0., 0, NULL},
        {NULL, NULL, 0., 0., 0},
    };

    FILE *fp = fopen("xxx", "r");
    values = read_source(fp, &n);
    fclose(fp);
    measure(perf, values, n);

    for (p = perf;p->func != NULL;++p) {
        printf(
            "%s\t%f\t%e\t%e\n",
            p->name,
            p->elapsed_time / (float)CLOCKS_PER_SEC,
            p->error_peak,
            p->error_rms
            );
    }
    
    return 0;
}
