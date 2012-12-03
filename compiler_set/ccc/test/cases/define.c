/*
ASSERT r3 = 3165
*/
#define COLS 80
#define ROWS 30
#define C(y, x) ((y)*COLS + (x))

int main(int argc)
{
  print_int(C(COLS/3, ROWS + 2));

  return 0;
}
