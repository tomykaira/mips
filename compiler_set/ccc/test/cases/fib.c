/*
ASSERT r3 = 55
*/
int read_int();
void print_int(int n);

int fib (int n) {
  return fib(n - 1) + fib(n - 2);
}

int main(int argc) {
  int n = 0;
  n = 10;
  n = fib(n);
  print_int(n);

  return 0;
}
