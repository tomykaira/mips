int f(int x) {
  return (x + 2) << 3;
}

int main() {
  int x = f(5);
  int y = f(2);
  print_int(x);
  print_int(y);
  return 0;
}
