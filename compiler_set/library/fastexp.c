/*
 *      Fast exp(x) computation (with SSE2 optimizations).
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

typedef union {
    float d;
    unsigned short s[4];
} ieee754;

static const float MAXLOG =  7.08396418532264106224E2;     /* log 2**1022 */
static const float MINLOG = -7.08396418532264106224E2;     /* log 2**-1022 */
static const float LOG2E  =  1.4426950408889634073599;     /* 1/log(2) */
static const float C1 = 6.93145751953125E-1;
static const float C2 = 1.42860682030941723212E-6;

void remez5_0_log2(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = floor(x / log 2) */
        a = LOG2E * x;
        a -= (a < 0);
        n = (int)a;

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 1.185268231308989403584147407056378360798378534739e-2;
        a *= x;
        a += 3.87412011356070379615759057344100690905653320886699e-2;
        a *= x;
        a += 0.16775408658617866431779970932853611481292418818223;
        a *= x;
        a += 0.49981934577169208735732248650232562589934399402426;
        a *= x;
        a += 1.00001092396453942157124178508842412412025643386873;
        a *= x;
        a += 0.99999989311082729779536722205742989232069120354073;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void remez7_0_log2(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = floor(x / log 2) */
        a = LOG2E * x;
        a -= (a < 0);
        n = (int)a;

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 2.8177033701883768252768416240498659720165776534637e-4;
        a *= x;
        a += 1.2890480764261415880540047007217296074165060445915e-3;
        a *= x;
        a += 8.3937617380265463210782576208636760245107957044397e-3;
        a *= x;
        a += 4.16466393513638680290956986766301695821268933286258e-2;
        a *= x;
        a += 0.16667025104576204064704670601843171167692234960251;
        a *= x;
        a += 0.49999968588211828322575537372426392301459912262932;
        a *= x;
        a += 1.00000001047169007093329328214028329840435313172085;
        a *= x;
        a += 0.99999999994275232965232540108706952362552348759169;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void remez9_0_log2(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = floor(x / log 2) */
        a = LOG2E * x;
        a -= (a < 0);
        n = (int)a;

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 3.9099787920346160288874633639268318097077213911751e-6;
        a *= x;
        a += 2.299608440919942766555719515783308016700833740918e-5;
        a *= x;
        a += 1.99930498409474044486498978862963995247838069436646e-4;
        a *= x;
        a += 1.38812674551586429265054343505879910146775323730237e-3;
        a *= x;
        a += 8.3335688409829575034112982839739473866857586300664e-3;
        a *= x;
        a += 4.1666622504201078708502686068113075402683415962893e-2;
        a *= x;
        a += 0.166666671414320541875332123507829990378055646330574;
        a *= x;
        a += 0.49999999974109940909767965915362308135415179642286;
        a *= x;
        a += 1.0000000000054730504284163017295863259125942049362;
        a *= x;
        a += 0.99999999999998091336479463057053516986466888462081;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void remez11_0_log2(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = floor(x / log 2) */
        a = LOG2E * x;
        a -= (a < 0);
        n = (int)a;

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 3.5524625185478232665958141148891055719216674475023e-8;
        a *= x;
        a += 2.5535368519306500343384723775435166753084614063349e-7;
        a *= x;
        a += 2.77750562801295315877005242757916081614772210463065e-6;
        a *= x;
        a += 2.47868893393199945541176652007657202642495832996107e-5;
        a *= x;
        a += 1.98419213985637881240770890090795533564573406893163e-4;
        a *= x;
        a += 1.3888869684178659239014256260881685824525255547326e-3;
        a *= x;
        a += 8.3333337052009872221152811550156335074160546333973e-3;
        a *= x;
        a += 4.1666666621080810610346717440523105184720007971655e-2;
        a *= x;
        a += 0.166666666669960803484477734308515404418108830469798;
        a *= x;
        a += 0.499999999999877094481580370323249951329122224389189;
        a *= x;
        a += 1.0000000000000017952745258419615282194236357388884;
        a *= x;
        a += 0.99999999999999999566016490920259318691496540598896;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void remez13_0_log2(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = floor(x / log 2) */
        a = LOG2E * x;
        a -= (a < 0);
        n = (int)a;

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 2.2762932529150460619497906285755631573256951680928e-10;
        a *= x;
        a += 1.93367224471636363463651078554697568501742984768854e-9;
        a *= x;
        a += 2.52543927629810215309605055241333435158089916072836e-8;
        a *= x;
        a += 2.7540144801860636516400552945824228138166510066936e-7;
        a *= x;
        a += 2.75583147053220552447847947870361102182338458956017e-6;
        a *= x;
        a += 2.4801546952196268386551625341270924245448949820144e-5;
        a *= x;
        a += 1.98412709907914555147826749325174275063119441397255e-4;
        a *= x;
        a += 1.38888888661019235625906849288500655817315802824057e-3;
        a *= x;
        a += 8.333333333639598748175600716777066054934952111002e-3;
        a *= x;
        a += 4.16666666666400203177888506088903193769823751590758e-2;
        a *= x;
        a += 0.16666666666666805461760233769080472413024408831279;
        a *= x;
        a += 0.499999999999999962289068852705038969538821467770838;
        a *= x;
        a += 1.0000000000000000004034789178104888993769939051368;
        a *= x;
        a += 0.99999999999999999999928421898456045238677548461656;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}



void vecexp_remez5_05_05(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 8.4330373844473099813810362019505406631926085091323e-3;
        a *= x;
        a += 4.21902068347366333044988703875445200415500811946999e-2;
        a *= x;
        a += 0.166651841763284853673636070741102751158699113784309;
        a *= x;
        a += 0.499950835134960574061545109283769441275186262483568;
        a *= x;
        a += 1.00000058571014555295260592037508552163073364874505;
        a *= x;
        a += 1.00000068337676053288862820129773498043487842807125;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void vecexp_remez7_05_05(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 2.00141610818912228248905083859257590733113260526108e-4;
        a *= x;
        a += 1.40133811518724818098307842775997566282188412967947e-3;
        a *= x;
        a += 8.3329654693594055317268781464679686290363549855757e-3;
        a *= x;
        a += 4.166471896964041208094102436547981534517053537243e-2;
        a *= x;
        a += 0.166666696439690960142075219571022911788612657807629;
        a *= x;
        a += 0.50000009744621522508878089211950835930852534338768;
        a *= x;
        a += 0.99999999932306797486003312890413784132553482368976;
        a *= x;
        a += 0.99999999923843009897272522306505037302103095520883;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void vecexp_remez9_05_05(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 2.7745788375840179681283717651801389074131600858024e-6;
        a *= x;
        a += 2.4974359052778718615679108099578563503364071042705e-5;
        a *= x;
        a += 1.98407490538622509507316519103953447383315553069056e-4;
        a *= x;
        a += 1.38885105592078787329129443877314802341416378733678e-3;
        a *= x;
        a += 8.3333339723641809789104572730499312924226776107297e-3;
        a *= x;
        a += 4.16666700463431739324947223978815539635978032483308e-2;
        a *= x;
        a += 0.166666666634013689476902841482489499264356163708049;
        a *= x;
        a += 0.49999999989435323455294486871496414870608658630883;
        a *= x;
        a += 1.00000000000048028927516239905073191568245953555709;
        a *= x;
        a += 1.00000000000052833535680371242873466617726870356609;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void vecexp_remez11_05_05(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 2.51929868958558990100711431834993496570205380722036e-8;
        a *= x;
        a += 2.7714302972476470857291821430459163185492029688565e-7;
        a *= x;
        a += 2.7556840833993121572460386718599369519143729132621e-6;
        a *= x;
        a += 2.48011454027599465130934662141475239085795284566129e-5;
        a *= x;
        a += 1.98412706284313544001695569595213030418978110809054e-4;
        a *= x;
        a += 1.38888894619673011407756064437561180652889917616113e-3;
        a *= x;
        a += 8.3333333326960625302778843136952722007056714994369e-3;
        a *= x;
        a += 4.1666666663307925829807454939715320634892173110281e-2;
        a *= x;
        a += 0.16666666666668912253462202236231047014798117425631;
        a *= x;
        a += 0.50000000000007198509320027789925087826877822951327;
        a *= x;
        a += 0.99999999999999976925988096235016345824070346859229;
        a *= x;
        a += 0.9999999999999997500224010478732616185808429624127;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void vecexp_remez13_05_05(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 1.61356848991757396195626449324190427883666354504187e-10;
        a *= x;
        a += 2.09773502429720720042832865218235470540902290986604e-9;
        a *= x;
        a += 2.50517997973487340520345973228530569065189960125308e-8;
        a *= x;
        a += 2.7556973177905466877853160305636358948188615327725e-7;
        a *= x;
        a += 2.7557319860870868162733628841564031959051602041504e-6;
        a *= x;
        a += 2.48015878916569323181318984106104816451563207545696e-5;
        a *= x;
        a += 1.98412698405602139306149764767811806786694849547848e-4;
        a *= x;
        a += 1.38888888883724638191056667131613472090590184962952e-3;
        a *= x;
        a += 8.333333333333743243093579329746662890641071879753e-3;
        a *= x;
        a += 4.16666666666688187524214985734081300874557496783939e-2;
        a *= x;
        a += 0.16666666666666665609766712753641211515446088211247;
        a *= x;
        a += 0.49999999999999996637019704330727593814705189927683;
        a *= x;
        a += 1.00000000000000000008007233620462705038544342452684;
        a *= x;
        a += 1.0000000000000000000857966908786376708355989802095;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}



void vecexp_taylor5(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 1. / 120.;
        a *= x;
        a += 4.1666666666666666666666666666666666666666666666666e-2;
        a *= x;
        a += 0.166666666666666666666666666666666666666666666666665;
        a *= x;
        a += 0.5;
        a *= x;
        a += 1.0;
        a *= x;
        a += 1.0;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void vecexp_taylor7(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 1. / 5040.;
        a *= x;
        a += 1.38888888888888888888888888888888888888888888888889e-3;
        a *= x;
        a += 8.3333333333333333333333333333333333333333333333333e-3;
        a *= x;
        a += 4.1666666666666666666666666666666666666666666666666e-2;
        a *= x;
        a += 0.166666666666666666666666666666666666666666666666665;
        a *= x;
        a += 0.5;
        a *= x;
        a += 1.0;
        a *= x;
        a += 1.0;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void vecexp_taylor9(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 1. / 362880.;
        a *= x;
        a += 2.4801587301587301587301587301587301587301587301587e-5;
        a *= x;
        a += 1.98412698412698412698412698412698412698412698412698e-4;
        a *= x;
        a += 1.38888888888888888888888888888888888888888888888889e-3;
        a *= x;
        a += 8.3333333333333333333333333333333333333333333333333e-3;
        a *= x;
        a += 4.1666666666666666666666666666666666666666666666666e-2;
        a *= x;
        a += 0.166666666666666666666666666666666666666666666666665;
        a *= x;
        a += 0.5;
        a *= x;
        a += 1.0;
        a *= x;
        a += 1.0;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void vecexp_taylor11(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 1. / 39916800.;
        a *= x;
        a += 2.7557319223985890652557319223985890652557319223986e-7;
        a *= x;
        a += 2.75573192239858906525573192239858906525573192239859e-6;
        a *= x;
        a += 2.4801587301587301587301587301587301587301587301587e-5;
        a *= x;
        a += 1.98412698412698412698412698412698412698412698412698e-4;
        a *= x;
        a += 1.38888888888888888888888888888888888888888888888889e-3;
        a *= x;
        a += 8.3333333333333333333333333333333333333333333333333e-3;
        a *= x;
        a += 4.1666666666666666666666666666666666666666666666666e-2;
        a *= x;
        a += 0.166666666666666666666666666666666666666666666666665;
        a *= x;
        a += 0.5;
        a *= x;
        a += 1.0;
        a *= x;
        a += 1.0;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}

void vecexp_taylor13(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float a, px, x = values[i];
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;

        /* Compute e^x using a polynomial approximation. */
        a = 1. / 6227020800;
        a *= x;
        a += 2.08767569878680989792100903212014323125434236545349e-9;
        a *= x;
        a += 2.50521083854417187750521083854417187750521083854419e-8;
        a *= x;
        a += 2.7557319223985890652557319223985890652557319223986e-7;
        a *= x;
        a += 2.75573192239858906525573192239858906525573192239859e-6;
        a *= x;
        a += 2.4801587301587301587301587301587301587301587301587e-5;
        a *= x;
        a += 1.98412698412698412698412698412698412698412698412698e-4;
        a *= x;
        a += 1.38888888888888888888888888888888888888888888888889e-3;
        a *= x;
        a += 8.3333333333333333333333333333333333333333333333333e-3;
        a *= x;
        a += 4.1666666666666666666666666666666666666666666666666e-2;
        a *= x;
        a += 0.166666666666666666666666666666666666666666666666665;
        a *= x;
        a += 0.5;
        a *= x;
        a += 1.0;
        a *= x;
        a += 1.0;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = a * u.d;
    }
}



void vecexp_cephes(float *values, int num)
{
    int i;
    for (i = 0;i < num;++i) {
        int n;
        float x = values[i];
        float a, xx, px, qx;
        ieee754 u;

        /* n = round(x / log 2) */
        a = LOG2E * x + 0.5;
        n = (int)a;
        n -= (a < 0);

        /* x -= n * log2 */
        px = (float)n;
        x -= px * C1;
        x -= px * C2;
        xx = x * x;

        /* px = x * P(x**2). */
        px = 1.26177193074810590878E-4;
        px *= xx;
        px += 3.02994407707441961300E-2;
        px *= xx;
        px += 9.99999999999999999910E-1;
        px *= x;

        /* Evaluate Q(x**2). */
        qx = 3.00198505138664455042E-6;
        qx *= xx;
        qx += 2.52448340349684104192E-3;
        qx *= xx;
        qx += 2.27265548208155028766E-1;
        qx *= xx;
        qx += 2.00000000000000000009E0;

        /* e**x = 1 + 2x P(x**2)/( Q(x**2) - P(x**2) ) */
        x = px / (qx - px);
        x = 1.0 + 2.0 * x;

        /* Build 2^n in float. */
        u.d = 0;
        n += 1023;
        u.s[3] = (unsigned short)((n << 4) & 0x7FF0);

        values[i] = x * u.d;
    }
}



void vecexp_libc(float *values, int n)
{
    int i;
    for (i = 0;i < n;++i) {
        values[i] = exp(values[i]);
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
            printf("%f %f\n", ex, exf);

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

int main(int argc, char *argv[])
{
    int n;
    float *values = NULL;
    performance_t *p = NULL;

    performance_t perf[] = {
        {"libc", vecexp_libc, 0., 0., 0, NULL},
        {"Cephes", vecexp_cephes, 0., 0., 0, NULL},
        {"Taylor 5th", vecexp_taylor5, 0., 0., 0, NULL},
        {"Taylor 7th", vecexp_taylor7, 0., 0., 0, NULL},
        {"Taylor 9th", vecexp_taylor9, 0., 0., 0, NULL},
        {"Taylor 11th", vecexp_taylor11, 0., 0., 0, NULL},
        {"Taylor 13th", vecexp_taylor13, 0., 0., 0, NULL},
        {"Remez 5th [-0.5,+0.5]", vecexp_remez5_05_05, 0., 0., 0, NULL},
        {"Remez 7th [-0.5,+0.5]", vecexp_remez7_05_05, 0., 0., 0, NULL},
        {"Remez 9th [-0.5,+0.5]", vecexp_remez9_05_05, 0., 0., 0, NULL},
        {"Remez 11th [-0.5,+0.5]", vecexp_remez11_05_05, 0., 0., 0, NULL},
        {"Remez 13th [-0.5,+0.5]", vecexp_remez13_05_05, 0., 0., 0, NULL},
        {"Remez 5th [0,log2]", remez5_0_log2, 0., 0., 0, NULL},
        {"Remez 7th [0,log2]", remez7_0_log2, 0., 0., 0, NULL},
        {"Remez 9th [0,log2]", remez9_0_log2, 0., 0., 0, NULL},
        {"Remez 11th [0,log2]", remez11_0_log2, 0., 0., 0, NULL},
        {"Remez 13th [0,log2]", remez13_0_log2, 0., 0., 0, NULL},
        {NULL, NULL, 0., 0., 0},
    };

    values = read_source(stdin, &n);
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
