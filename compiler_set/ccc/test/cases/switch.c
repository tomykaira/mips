int callee(int argc)
{
  switch (argc) {
  case 0:
    return 5;
  case 1:
    return 8;
  case 2:
    return 9;
  default:
    return 92;
  }
}

int main() {
  print_int(callee(0));
  print_int(callee(1));
  print_int(callee(2));
  print_int(callee(3));

  return 0;
}
