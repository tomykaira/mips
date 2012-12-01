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
  return f;
}
