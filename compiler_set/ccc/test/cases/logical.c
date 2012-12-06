void and() {
  int i = 0;
  int j = 0;

  i = 10;
  j = 20;
  if (i == 10 && j == 20) {
    i += 1;
  } else {
    i = 0;
    j = 0;
  }
  if (j == i + 9 && j == 20) {
    j += 1;
  } else {
    i = 0;
    j = 0;
  }
  if (i == 1 && j == 21) {
    i = 0;
    j = 0;
  } else {
    j += 1;
  }
  if (i == 11 && j == 0) {
    i = 0;
    j = 0;
  } else {
    j += 1;
  }
  print_int(i);
  print_int(j);
}

void or() {
  int i = 0;
  int j = 0;

  i = 10;
  j = 20;
  if (i == 10 || j == 21) {
    i += 1;
  } else {
    i = 0;
    j = 0;
  }
  if (i == 10 || j == 20) {
    i += 1;
  } else {
    i = 0;
    j = 0;
  }
  if (j == 0 || i == 0) {
    i = 1;
    j = 1;
  }
  print_int(i);
  print_int(j);
}

int main()
{
  and();
  or();
  return 0;
}
