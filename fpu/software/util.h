#ifndef _UTIL_H_
#define _UTIL_H_

typedef long long ll;
typedef unsigned long long ull;
typedef unsigned int ui;

#define swap(a,b) { int temp = a; a = b; b = temp; }

#define DEBUG 0
#define DOTS 0
#define TESTCASE 0
#define D(x) { if (DEBUG) { x ; } }

#define MANTISSA(x) (0x800000 + (x & 0x7fffff))
#define EXP(x) ((int)((x & 0x7f800000) >> 23)-127)
#define MAN_TO_FLOAT(x) ((127 << 23) + ((x) & 0x7fffff))
#define F(x) ((1 << x) - 1)
#define MANTISSA_ONLY(x) ((((x) & F(23)) + (127 << 23)))

void print_float(ui x);
unsigned int rand_float();
unsigned int place(ll x);

union IntAndFloat {
    unsigned int ival;
    float fval;
};

typedef IntAndFloat fi;


#endif /* _UTIL_H_ */
