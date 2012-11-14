#ifndef _FPU_H_
#define _FPU_H_

#include <stdint.h>
#include <float.h>
#include <math.h>

typedef union{	uint32_t i; float f;} conv;


#ifdef __cplusplus
extern "C" {
#endif
void load_tables(const char * dirpath);
uint32_t myfadd(uint32_t rs, uint32_t rt);
uint32_t myfsub(uint32_t rs, uint32_t rt);
uint32_t myfmul(uint32_t rs, uint32_t rt);
uint32_t myfdiv(uint32_t rs, uint32_t rt);
uint32_t myfinv(uint32_t rs);
uint32_t myfsqrt(uint32_t rs);
uint32_t myfabs(uint32_t rs);
uint32_t myfneg(uint32_t rs);
uint32_t myfloor(uint32_t rs);
uint32_t myfsin(uint32_t rs);
uint32_t myfcos(uint32_t rs);
uint32_t myftan(uint32_t rs);
uint32_t myfatan(uint32_t rs);
#ifdef __cplusplus
}
#endif

#endif /* _FPU_H_ */
