/*
ASSERT r3 = 5
*/

int f(int v) {
  return v + 5;
}

int main(int argc)
{
  int result = 0;
  int ret = 0;
  ret = f(result);
  print_int(ret);
  return 0;
}
