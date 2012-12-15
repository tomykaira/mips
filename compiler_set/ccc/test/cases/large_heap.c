int large_array[0x8000];
int can_bug = 0;

int main() {
  can_bug = 5;
  print_int(can_bug);
  return 0;
}
