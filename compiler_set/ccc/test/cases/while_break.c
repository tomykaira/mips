int main()
{
  int i = 0;
  while (i < 100) {
    i = i + 1;
    if (i > 50) {
      break;
    }
  }
  print_int(i);
  return 0;
}
