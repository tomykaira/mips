#define COLS 80 /* replace before compile */
#define ROWS 30
#define C(y, x) ((y)*COLS + (x))

char read_key();
void move_memory(char * array, int offset, int size);
void send_display(char * array);
void load_program();

int current_line = 0;
int current_column = 0;
char buffer[2400];

void put_char(char i) {
  buffer[C(current_line, current_column)] = i;
  current_column += 1;
}

void format_line() {
  put_char('>');
  put_char(' ');
}

void clear_line(int line) {
  int i = 0;
  int end = 0;
  i = C(line, 0);
  end = C(line, ROWS);
  while (i < end) {
    buffer[i] = 0;
    i += 1;
  }
}

void next_line() {
  if (current_line < ROWS-1) {
    current_line += 1;
  } else {
    move_memory(buffer, -COLS, 2400 - COLS);
    clear_line(ROWS-1);
  }
  current_column = 0;
}

void execute_command() {
  int line_start = 0;
  int line_end = 0;
  line_start = C(current_line, 2);
  line_end = C(current_line, current_column);
  next_line();
  if (buffer[line_start] == 'e'
      && buffer[line_start + 1] == 'c'
      && buffer[line_start + 2] == 'h'
      && buffer[line_start + 3] == 'o'
      && buffer[line_start + 4] == ' ') {
    line_start += 5;
    while (line_start < line_end) {
      put_char(buffer[line_start]);
      line_start += 1;
    }
  }

  if (buffer[line_start] == 'e'
      && buffer[line_start + 1] == 'x'
      && buffer[line_start + 2] == 'i'
      && buffer[line_start + 3] == 't') {
    put_char('b');
    put_char('y');
    put_char('e');
    send_display(buffer);
    halt();
  }

  if (buffer[line_start] == 'l'
      && buffer[line_start + 1] == 'o'
      && buffer[line_start + 2] == 'a'
      && buffer[line_start + 3] == 'd') {
    put_char('w');
    put_char('a');
    put_char('i');
    put_char('t');
    put_char('i');
    put_char('n');
    put_char('g');
    put_char('.');
    put_char('.');
    put_char('.');
    send_display(buffer);
    load_program();
  }
}

void add_key_input() {
  char input = 0;

  input = read_key(); /* blocking */
  if (input == '\n') {
    execute_command();
    next_line();
    format_line();
  } else if (input == 0x7f) {//  backspace
    if (current_column > 2) {
      current_column -= 1;
      buffer[C(current_line, current_column)] = 0;
    }
  } else if (input != 0) {
    put_char(input);
  }
}

void main(int argc)
{
  format_line();
  while (1) {
    add_key_input();
    send_display(buffer); /* give pointer to assembly function */
  }
}
