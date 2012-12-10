char other_array[1000];
char array[1024] = "prepared";

void add_www(char * arr) {
  int i = 0;
  while (i < 1024 && arr[i] != 0) {
    i += 1;
  }
  while (i < 100) {
    arr[i] = 'w';
    i += 1;
  }
}

int main() {
  add_www(array);
  send_rs(array, 20);
  return 0;
}
