int read_int();
void print_int(int n);

int fib (int n) {
  return fib(n - 1) + fib(n - 2);
}

void main(int argc) {
  int n = 0;
  n = read_int();
  n = fib(n);
  print_int(n);
}
