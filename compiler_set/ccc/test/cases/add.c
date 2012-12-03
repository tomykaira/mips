int callee () {
  return 1 + 2;
}

int main() {
  print_int(callee());
  return 0;
}
