#ifndef _TABLE_H_
#define _TABLE_H_

#include <stdio.h>
#include <errno.h>

#define MAX_KEY 1024

extern unsigned int const_table[MAX_KEY];
extern unsigned int inc_table[MAX_KEY];

void initialize_tables();
int read_tables();
int write_tables();
int join_tables(const char * filename);

unsigned int table_const(unsigned int key);
unsigned int table_inc(unsigned int key);

#endif /* _TABLE_H_ */
