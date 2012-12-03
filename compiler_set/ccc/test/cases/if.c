/*
ASSERT r3 = 20
*/

int main()
{
  int x = 0;
  int y = 1;
  int ans = 0;
  if (x == 0) {
    ans += 5;
  } else {
    ans +=  3;
  }
  if (y == 0) {
    ans += 9;
  } else {
    ans += 15;
  }
  print_int(ans);
  return 0;
}
