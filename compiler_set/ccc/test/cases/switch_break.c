int callee(int argc)
{
  int f = 0;
  switch (argc) {
  case 0:
    {
      f = 5;
      break;
    }
  case 1:
    {
      f = 8;
      break;
    }
  case 2:
    {
      f = 11;
      break;
    }
  default:
    {
      f = 92;
      break;
    }
  }
  print_int(f);
  return 0;
}

int main() {
  callee(0);
  callee(1);
  callee(2);
  callee(3);
  return 0;
}
