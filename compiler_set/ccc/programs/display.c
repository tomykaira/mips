#define COLS 80 /* replace before compile */
#define ROWS 30
#define C(y, x) ((y)*COLS + (x))

char read_key();
void move_memory(char * array, int offset, int size);
void send_display(char * array);

int current_line = 0;
int current_column = 0;
char buffer[2400];

void format_line() {
  buffer[C(current_line, 0)] = '>';
  current_column = 2;
}

void next_line() {
  if (current_line < ROWS-1) {
    current_line += 1;
    format_line();
  } else {
    move_memory(buffer, -COLS, 2400 - COLS);
    format_line();
  }
}

void add_key_input() {
  char input = 0;

  input = read_key(); /* blocking */
  if (input == '\n') {
    next_line();
  } else {
    buffer[C(current_line, current_column)] = input;
    current_column += 1;
  }
}

void main(int argc)
{
  while (1) {
    add_key_input();
    send_display(buffer); /* give pointer to assembly function */
  }
}
