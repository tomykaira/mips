int main() {
  int x = 0;

  x = 8;
  goto print;
  goto end;
  print_int(0);
 print:
  print_int(x);
 end:
  return 0;
}
