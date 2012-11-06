#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(void) {
  FILE *fp;
  srand((unsigned) time(NULL));
  unsigned int ex= rand()%254+1;    
  unsigned int a= ex << 23;
  fp = fopen("mytest.vec","w");
  while (1) {
    fprintf(fp, "%08x\n", a);
    if (a >= (ex << 23)+0x007fffff) break;
    else{
      a++;
    }
  }
  fclose(fp);
  return 0;
}
