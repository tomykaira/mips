#include <cstdio>
#include "table.h"
#include "util.h"

// to pass compilation
int table_const(int key) {return 0;}
int table_inc(int key) {return 0;}

int main(int argc, char *argv[])
{
  read_tables();

  return join_tables("finv_join.dat");
}
