int main(int argc)
{
  int x = 0;
  x = 1;
  {
    int x = 3;
    x = x + 1;
  }
  return x;
}
