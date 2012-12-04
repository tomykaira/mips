/*
ASSERT r3 = 55
*/
int read_int();
void print_int(int n);

int fib (int n) {
  if (n == 0) return 1;
  if (n == 1) return 1;
  return fib(n - 1) + fib(n - 2);
}

int main() {
  int n = 0;
  n = 2;
  n = fib(n);
  print_int(n);

  return 0;
}
