int a = 10;
int b = 3;
int c = 92;
int d = 88;
int e = 24;
int f = 11;

int main() {
  int x = 0;
  int y = 0;
  int z = 0;

  x = a + b + c + d + e + f;
  y = a + b - c + d - e + f;
  z = a*b*c + d*e*f;

  print_int(x);
  print_int(y);
  print_int(z);
  return 0;
}
