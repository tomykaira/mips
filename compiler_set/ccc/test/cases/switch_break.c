/*
INPUT r3 = 0
ASSERT r3 = 5
INPUT r3 = 1
ASSERT r3 = 8
INPUT r3 = 2
ASSERT r3 = 11
INPUT r3 = 3
ASSERT r3 = 92
*/

int main(int argc)
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
