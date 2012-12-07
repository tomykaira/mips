#define COLS 80 /* replace before compile */
#define ROWS 30
#define EOF '!'
#define C(y, x) (((y) << 6) + ((y) << 4) + (x))

char read_key();
void move_memory(char * array, int offset, int size);
void send_display(char * array);
void load_program();

int current_line = 0;
int current_column = 0;
int insert_mode = 0;

char notification[80];
char buffer[2400];
char text_buffer[2400];

void update_notification_line() {
  int i = 0;
  int buffer_last_line = 0;

  buffer_last_line = C(ROWS - 1, 0);
  while (i<COLS) {
    buffer[buffer_last_line + i] = notification[i];
    i += 1;
  }
}

void insert_character(char input) {
  int first = 0;
  int ptr = 0;

  move_memory(buffer + C(current_line, current_column), 1, COLS - current_column - 1);
  buffer[C(current_line, current_column)] = input;
  current_column += 1;
}

int break_line() {
  current_line += 1;
  current_column = 0;
  move_memory(buffer + C(current_line, 0), 80, C(ROWS - current_line - 1, 0));
}

void write() {
  int text_pointer = 0;
  int line = 0;
  int column = 0;

  while ( line < ROWS ) {
    int c = 0;
    c = buffer[C(line, column)];
    if (c == EOF) {
      break;
    } else if (c != 0) {
      text_buffer[text_pointer] = c;
      text_pointer += 1;
    }
    column += 1;
    if (column >= COLS) {
      column = 0;
      line += 1;
      text_buffer[text_pointer] = '\n';
      text_pointer += 1;
    }
  }
  send_rs(text_buffer, text_pointer);
}

// int direction: 0: up, 1: right,  2: down, 3: left
// return: 1 when quit
int interpret_command(char input) {
  switch (input) {
  case 'i':
    insert_mode = 1;
    // set_string(notifications, "INPUT");
    break;
  case 'q':
    return 1;
  case 'w':
    write();
    break;
  case 'j':
    if (current_line > 0) {
      current_line += 1;
    }
    break;
  case 'k':
    if (current_line < ROWS) {
      current_line -= 1;
    }
    break;
  case 'h':
    if (current_column > 0) {
      current_column -= 1;
    }
    break;
  case 'l':
    if (current_column < COLS) {
      current_column += 1;
    }
    break;
  case 'x':
    {
      int end = 0;
      int ptr = 0;

      end = -1;
      ptr = C(current_line, current_column);

      move_memory(buffer + C(current_line, current_column), -1, COLS - current_column - 1);
      buffer[C(current_line, COLS-1)] = 0;
    }
    break;
  }
  return 0;
}

void main(int argc)
{
  char input = 0;

  buffer[0] = EOF;

  while (1) {
    input = read_key(); /* blocking */
    if (insert_mode == 1) {
      switch (input) {
      case '\n':
        break_line();
        break;
      case 0x7f:
        break;
      case 27: // esc
        insert_mode = 0;
        // set_string(notifications, "COMMAND");
        break;
      default:
        if (input != 0) {
          insert_character(input);
        }
        break;
      }
    } else {
      if (interpret_command(input) == 1) {
        return 0;
      }
    }
    // update_notification_line();
    send_display(buffer); /* give pointer to assembly function */
  }
}
