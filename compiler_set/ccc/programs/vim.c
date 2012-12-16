#define COLS 80 /* replace before compile */
#define ROWS 30
#define EOF '*'
#define EOL '\n'
#define C(y, x) (((y) << 6) + ((y) << 4) + (x))

char read_key();
void move_memory(char * array, int offset, int size);
void send_display(char * array);
void load_program();

int current_line = 0;
int current_column = 0;
int insert_mode = 0;

int directory_id = 0;
int file_id = 0;
char filename[12];

char notification[80];
char buffer[2400];
char text_buffer[2400];
char file_content[0x1000];
char token[255];

void update_notification_line() {
  int i = 0;
  int buffer_last_line = C(ROWS - 1, 0);

  while (i<COLS) {
    buffer[buffer_last_line + i] = notification[i];
    i += 1;
  }
}

// fail if the line is too long
void insert_character(char input) {
  if (buffer[C(current_line, 79)] != 0)
    return;
  move_memory(buffer + C(current_line, current_column), 1, COLS - current_column - 1);
  buffer[C(current_line, current_column)] = input;
  current_column += 1;
}

void break_line() {
  move_memory(buffer + C(current_line + 1, 0), 80, C(ROWS - current_line, 0));
  move_and_clear_memory(buffer + C(current_line, current_column), COLS - current_column, COLS - current_column);
  current_line += 1;
  current_column = 0;
}

// parent_directory_id, entry_id, cluster_id
int resolve_result[3];

void read() {
  int cluster_id = 0;
  int new_cluster_id = 0;
  int argument_pointer = 0;
  int prev_pointer = 0;
  int empty_index = 0;
  int entry_id = 0;
  int file_size = 0;

  while (argument[argument_pointer] != 0) {
    prev_pointer = argument_pointer;
    argument_pointer += basename(argument + argument_pointer, filename);
    argument_pointer += 1;  // skip "/"
  }

  prev_pointer -= 1;
  while (prev_pointer < argument_pointer) {
    argument[prev_pointer] = 0;
    prev_pointer += 1;
  }

  if (resolve_argument_path(argument[ARGUMENT_HEAP_SIZE-1], argument, resolve_result) == -1) {
    copy_string(argument, file_not_found_error_message);
    return;
  }
  directory_id = resolve_result[2];

  entry_id = find_entry_by_name(directory_id, filename);

  if (entry_id == ENTRY_NOT_FOUND_ID) {
    buffer[0] = EOF;
    file_id = 0;
  } else {
    file_id = get_cluster_id(directory_id, entry_id);
    file_size = get_file_size(directory_id, entry_id);
    read_file(file_id, file_size, file_content);

    {
      int line = 0;
      int column = 0;
      int ptr = 0;
      int got_newline = 0;

      initialize_array(buffer, 2400, 0);

      while (ptr < file_size) {
        buffer[C(line, column)] = file_content[ptr];
        if (file_content[ptr] == '\n') {
          line += 1;
          column = 0;
        } else {
          column += 1;
        }
        ptr += 1;
      }

      buffer[C(line, column)] = EOF;
    }
  }
}

void write() {
  int length = 0;
  int line = 0;
  int column = 0;

  while ( line < ROWS ) {
    int c = 0;
    c = buffer[C(line, column)];
    if (c == EOF) {
      break;
    } else if (c != 0) {
      text_buffer[length] = c;
      length += 1;
    }
    column += 1;
    if (column >= COLS) {
      column = 0;
      line += 1;
    }
  }


  if (file_id) {
    write_file(file_id, text_buffer, length);
    update_file_size(directory_id, file_id, length);
  } else {
    int empty_index = find_empty_directory_index(directory_id);

    if (empty_index == -1) {
      copy_string(argument, no_empty_index_error_message);
      return;
    }
    file_id = create_fat_entry();
    if (file_id == -1) {
      copy_string(argument, no_fat_entry_error_message);
      return;
    }
    write_file(file_id, text_buffer, length);
    create_file_entry(directory_id, empty_index, 0, file_id, length, filename);
  }

  send_rs(text_buffer, length);
}

void goto_last_column() {
  int last = 0;

  if (current_column > 0) {
    while (buffer[C(current_line, current_column-1)] == 0) {
      current_column -= 1;
    }
    last = buffer[C(current_line, current_column-1)];
    if (last == EOL || last == EOF) {
      current_column -= 1; // skip EOL
    }
  }
}

// int direction: 0: up, 1: right,  2: down, 3: left
// return: 1 when quit
int interpret_command(char input) {
  switch (input) {
  case 'i':
    insert_mode = 1;
    break;
  case 'a':
    insert_mode = 1;
    break;
  case 'o':
    move_memory(buffer + C(current_line + 1, 0), 80, C(ROWS - current_line, 0));
    move_memory(buffer + C(current_line + 1, 0), 80, C(ROWS - current_line, 0));
    current_line += 1;
    current_column = 0;
    initialize_array(buffer + C(current_line, 0), C(current_line, COLS-1) - C(current_line, 0), 0);
    buffer[C(current_line, 0)] = EOL;
    insert_mode = 1;
    break;
  case 'q':
    return 1;
  case 'w':
    write();
    break;
  case 'j':
    if (current_line < ROWS && buffer[C(current_line + 1, 0)] != 0) {
      current_line += 1;
      goto_last_column();
    }
    break;
  case 'k':
    if (current_line > 0) {
      current_line -= 1;
      goto_last_column();
    }
    break;
  case 'h':
    if (current_column > 0) {
      current_column -= 1;
    }
    break;
  case 'l':
    if (current_column < COLS && buffer[C(current_line, current_column + 1)] != 0) {
      current_column += 1;
    }
    break;
  case 'x':
    {
      int current_char = buffer[C(current_line, current_column)];
      if (current_char != EOF && current_char != EOL) {
        int end = -1;
        int ptr = C(current_line, current_column);

        move_memory(buffer + C(current_line, current_column), -1, COLS - current_column - 1);
        buffer[C(current_line, COLS-1)] = 0;
      }
    }
    break;
  }
  return 0;
}

void main(int argc)
{
  char input = 0;

  read();

  while (1) {
    input = read_key(); /* blocking */
    if (insert_mode == 1) {
      switch (input) {
      case '\n':
        insert_character(input);
        break_line();
        break;
      case 0x7f:
        if (current_column > 0) {
          int current_char = 0;
          current_column -= 1;
          current_char = buffer[C(current_line, current_column)];
          if (current_column >= 0 && current_char != EOF && current_char != EOL) {
            move_memory(buffer + C(current_line, current_column), -1, COLS - current_column - 1);
            buffer[C(current_line, COLS-1)] = 0;
          }
        }
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
    display(C(current_line, current_column), '_');
  }
}
