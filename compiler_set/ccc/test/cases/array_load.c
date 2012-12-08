int a[10];

int main() {
  int x = 0;
  int y = 0;
  int z = 0;
  int ctr = 0;

  while (ctr < 10) {
    a[ctr] = ctr;
    ctr += 1;
  }

  x = a[0] + a[a[1] + a[2]];
  y = a[a[1] + a[1] + a[2] + a[4]] + a[a[3] + a[5]];
  z = a[a[a[1] + a[1]] + a[a[2] + a[2]]]*2;

  print_int(x);
  print_int(y);
  print_int(z);
  return 0;
}
