#include <cstdio>
#include "table.h"
#include "util.h"

// to pass compilation
ui table_const(ui key) {return 0;}
ui table_inc(ui key) {return 0;}

int main(int argc, char *argv[])
{
  read_tables();

  return join_tables("fsqrt_join.dat");
}
