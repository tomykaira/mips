#define COLS 80 /* replace before compile */
#define ROWS 30
#define C(y, x) (((y) << 6) + ((y) << 4) + (x))

char read_key();
void move_memory(char * array, int offset, int size);
void send_display(char * array);
void load_program();

char bin_token[8] = "BIN";
int bin_cluster_id = 0;

int current_directory_id = 0;

int current_line = 1;
int current_column = 0;
char buffer[2400] = "Welcome to FutureGadgetLab! by. Kyoma Hououin";
char program_name[80];
char argument[128];
char file_content[0x10000];

void put_char(char i) {
  if (i == '\n') {
    next_line();
  } else {
    buffer[C(current_line, current_column)] = i;
    current_column += 1;
  }
}

void format_line() {
  put_char('>');
  put_char(' ');
}

void clear_line(int line) {
  int i = 0;
  int end = 0;
  i = C(line, 0);
  end = C(line, COLS);
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

void execute_bin(char * program, char * argument) {
  int entry_id     = find_entry_by_name(bin_cluster_id, program);
  int cluster_id   = get_cluster_id(bin_cluster_id, entry_id);
  int program_size = get_file_size(bin_cluster_id, entry_id);

  read_file(cluster_id, program_size, file_content);
  execute(file_content, program_size, argument);
}

void print_return_argument(char * result) {
  int ptr = 0;
  while (ptr < ARGUMENT_HEAP_SIZE && result[ptr] != 0) {
    put_char(result[ptr]);
    ptr += 1;
  }
}

// update current_directory_id environment variable
int resolve_result[3];
void change_directory(char * path, int length) {
  resolve_argument_path(current_directory_id, path, resolve_result);
  current_directory_id = resolve_result[2];
}

char command[80];
char c_exit[80] = "exit";
char c_cd[80] = "cd ";
void process_command() {
  int command_ptr  = 0;

  initialize_array(command, 80, 0);
  copy_string(command, buffer + C(current_line, 2));
  initialize_array(argument, ARGUMENT_HEAP_SIZE, 0);

  send_rs(command, 10);
  next_line();

  if (str_equal(c_exit, command, 4)) {
    put_char('b');
    put_char('y');
    put_char('e');
    send_display(buffer);
    halt();

  } else if (str_equal(c_cd, command, 3)) {
    int length = copy_string(argument, command + 3);
    change_directory(argument, length);

  } else if (command[0] == 0) {
    return;

  } else {
    int ptr = 0;
    while (command[command_ptr] != ' '
           && command[command_ptr] != 0
           && command_ptr < COLS) {
      int byte = command[command_ptr];
      if (byte >= 'a' && byte <= 'z') {
        byte -= 0x20;
      }
      program_name[ptr] = byte;
      command_ptr += 1;
      ptr += 1;
    }
    program_name[ptr] = 0;

    while (command[command_ptr] == ' ' && command_ptr < COLS) {
      command_ptr += 1;
    }

    ptr = 0;
    while (command[command_ptr] != ' '
           && command[command_ptr] != 0
           && command_ptr < COLS) {
      argument[ptr] = command[command_ptr];
      command_ptr += 1;
      ptr += 1;
    }
    argument[ptr] = 0;
    argument[ARGUMENT_HEAP_SIZE-1] = current_directory_id;
    execute_bin(program_name, argument);

    send_rs(argument, 10);
    print_return_argument(argument);
  }
}

void add_key_input() {
  char input = 0;

  input = read_key(); /* blocking */
  if (input == '\n') {
    process_command();
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

void get_bin_cluster_id() {
  int root_entry = 0;
  int entry_id = find_entry_by_name(root_entry, bin_token);
  bin_cluster_id = get_cluster_id(root_entry, entry_id);
}

void main(int argc)
{
  get_bin_cluster_id();
  format_line();
  while (1) {
    add_key_input();
    send_display(buffer); /* give pointer to assembly function */
  }
}
