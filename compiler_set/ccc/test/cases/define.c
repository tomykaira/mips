#define COLS 80
#define ROWS 30
#define C(y, x) ((y)*COLS + (x))

int main(int argc)
{
  return C(COLS/3, ROWS + 2);
}
