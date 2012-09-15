#include <cassert>

#include "table.h"
#include "util.h"

unsigned int const_table[MAX_KEY];
unsigned int inc_table[MAX_KEY];

void initialize_tables() {
  for (ui i = 0; i<MAX_KEY; i++) {
    const_table[i] = table_const(i);
    inc_table[i] = table_inc(i);
  } 
}

int read_tables() {
  FILE * fp = fopen("const.dat", "r");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      fscanf(fp, "%x\n", &const_table[i]);
    }
    if (fclose(fp) != 0) {
      perror("fclose const.dat");
      return 1;
    }
  } else {
    perror("fopen const.dat");
    return 1;
  }

  fp = fopen("inc.dat", "r");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      fscanf(fp, "%x\n", &inc_table[i]);
    }
    if (fclose(fp) != 0) {
      perror("fclose inc.dat");
      return 1;
    }
  } else {
    perror("fopen inc.dat");
    return 1;
  }

  return 0;
}

int write_tables() {
  FILE * fp = fopen("const.dat", "w");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      fprintf(fp, "%x\n", const_table[i]);
    }
    if (fclose(fp) != 0) {
      perror("fclose const.dat");
      return 1;
    }
  } else {
    perror("fopen const.dat");
    return 1;
  }

  fp = fopen("inc.dat", "w");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      fprintf(fp, "%x\n", inc_table[i]);
    }
    if (fclose(fp) != 0) {
      perror("fclose inc.dat");
      return 1;
    }
  } else {
    perror("fopen inc.dat");
    return 1;
  }

  return 0;
}

int join_tables(const char * filename) {
  FILE * fp = fopen(filename, "w");
  if (fp) {
    for (int i = 0; i<MAX_KEY; i++) {
      assert((const_table[i] & F(23)) == const_table[i]);
      assert((inc_table[i] & F(13)) == inc_table[i]);
      fprintf(fp, "%llx\n", (((ull)const_table[i] & F(23ll)) << 13ll) + (inc_table[i] & F(13)));
    }
    if (fclose(fp) != 0) {
      perror("fclose finv_join.dat");
      return 1;
    }
  } else {
    perror("fopen finv_join.dat");
    return 1;
  }

  return 0;
}
