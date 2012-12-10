int div_binary_search(int a, int b, int left, int right) {
  int mid = (left + right) >> 1;
  int x = mid*b;

  if (right - left <= 1) {
    return left;
  } else {
    if (x < a) {
      return div_binary_search(a, b, mid, right);
    } else if (x == a) {
      return mid;
    } else {
      return div_binary_search(a, b, left, mid);
    }
  }
}

int print_int (int x, char * output) {
  int size = 0;
  int tx = 0;
  int dx = 0;
  int flg = 0;
  int length = 0;
  if (x<0) {
    output[length] = '-';
    length += 1;
    x = -x;
  }

  tx = div_binary_search(x, 100000000, 0, 3);
  dx = tx*100000000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    output[length] = '0' + tx;
    length += 1;
    flg = 1;
  }

  tx = div_binary_search(x, 10000000, 0, 10);
  dx = tx*10000000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    output[length] = '0' + tx;
    length += 1;
    flg = 1;
  }

  tx = div_binary_search(x, 1000000, 0, 10);
  dx = tx*1000000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    output[length] = '0' + tx;
    length += 1;
    flg = 1;
  }

  tx = div_binary_search(x, 100000, 0, 10);
  dx = tx*100000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    output[length] = '0' + tx;
    length += 1;
    flg = 1;
  }

  tx = div_binary_search(x, 10000, 0, 10);
  dx = tx*10000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    output[length] = '0' + tx;
    length += 1;
    flg = 1;
  }

  tx = div_binary_search(x, 1000, 0, 10);
  dx = tx*1000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    output[length] = '0' + tx;
    length += 1;
    flg = 1;
  }

  tx = div_binary_search(x, 100, 0, 10);
  dx = tx*100;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    output[length] = '0' + tx;
    length += 1;
    flg = 1;
  }

  tx = div_binary_search(x, 10, 0, 10);
  dx = tx*10;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    output[length] = '0' + tx;
    length += 1;
    flg = 1;
  }

  output[length] = '0' + x;
  length += 1;
  return length;
}
