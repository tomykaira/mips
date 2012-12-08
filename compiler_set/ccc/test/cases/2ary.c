int callee(int x, int y) {
  return x + y;
}

int main()
{
  print_int(callee(10, 20));
  return 0;
}
