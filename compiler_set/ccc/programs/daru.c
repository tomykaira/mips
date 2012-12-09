char output[1024];
int output_length = 0;

void main() {
  int addr = 0x14000;
  int end = 0x14008;

  while ( addr < end ) {
    int rs = read_sd(addr);
    output[output_length] = rs;
    output_length += 1;
    addr += 1;
  }

  output[output_length] = '\n';
  output_length += 1;

  send_rs(output, output_length);
  return;
}
