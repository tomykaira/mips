int x = 0;
int y = 10;
int z = 0;
void print_int(int n);

int callee() {
  x = x + 1;
  return x;
}

int callee2() {
  z = x + y;
  return z;
}

int main() {
  callee();
  callee2();
  callee();
  callee2();
  callee();
  print_int(x);
  print_int(y);
  print_int(z);
  return 0;
}
