void if_eq () {
  int x = 0;
  int y = 1;
  int ans = 0;

  if (x == 0) {
    ans += 5;
  }
  if (y == 0) {
    ans += 9;
  }
  print_int(ans);

  ans = 0;
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
}

void if_lt () {
  int x = 0;
  int y = 1;
  int ans = 0;

  if (x < 1) {
    ans += 5;
  }
  if (y < 1) {
    ans += 9;
  }
  print_int(ans);

  ans = 0;
  if (x < 1) {
    ans += 5;
  } else {
    ans +=  3;
  }
  if (y < 1) {
    ans += 9;
  } else {
    ans += 15;
  }
  print_int(ans);
}

void if_le () {
  int x = 0;
  int y = 1;
  int ans = 0;

  if (x <= 0) {
    ans += 5;
  }
  if (y <= 0) {
    ans += 9;
  }
  print_int(ans);

  ans = 0;
  if (x <= 0) {
    ans += 5;
  } else {
    ans +=  3;
  }
  if (y <= 0) {
    ans += 9;
  } else {
    ans += 15;
  }
  print_int(ans);
}

void if_gt () {
  int x = 1;
  int y = 0;
  int ans = 0;

  if (x > 0) {
    ans += 5;
  }
  if (y > 0) {
    ans += 9;
  }
  print_int(ans);

  ans = 0;
  if (x > 0) {
    ans += 5;
  } else {
    ans +=  3;
  }
  if (y > 0) {
    ans += 9;
  } else {
    ans += 15;
  }
  print_int(ans);
}

void if_ge () {
  int x = 1;
  int y = 0;
  int ans = 0;

  if (x >= 1) {
    ans += 5;
  }
  if (y >= 1) {
    ans += 9;
  }
  print_int(ans);

  ans = 0;
  if (x >= 1) {
    ans += 5;
  } else {
    ans +=  3;
  }
  if (y >= 1) {
    ans += 9;
  } else {
    ans += 15;
  }
  print_int(ans);
}

int main()
{
  if_eq();
  if_lt();
  if_le();
  if_gt();
  if_ge();
  return 0;
}
