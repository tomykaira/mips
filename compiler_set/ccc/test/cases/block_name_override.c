/*
ASSERT r3 = 4
*/
int main(int argc)
{
  int x = 0;
  x = 1;
  {
    int x = 3;
    x = x + 1;
  }

  print_int(x);

  return 0;
}
