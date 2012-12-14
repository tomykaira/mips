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

int current_line = 0;
int current_column = 0;
char buffer[2400];
char program_name[80];
char argument[128];
char file_content[0x1000];

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
char token[11];
void change_directory(char * path, int length) {
  int pointer = 0;
  int entry_id = 0;
  int cluster_id = 0;

  if (path[0] == '/') {
    current_directory_id = 0;
    pointer = 1;
  }

  while (1) {
    pointer += basename(path + pointer, token);
    pointer += 1;

    entry_id     = find_entry_by_name(cluster_id, token);
    cluster_id   = get_cluster_id(cluster_id, entry_id);

    if (path[pointer] != '/') {
      break;
    }
  }

  current_directory_id = cluster_id;
}

void process_command() {
  int line_start = 0;
  int line_end = 0;
  int ptr = 0;
  line_start = C(current_line, 2);
  line_end = C(current_line, current_column);
  next_line();

  if (buffer[line_start] == 'e'
      && buffer[line_start + 1] == 'x'
      && buffer[line_start + 2] == 'i'
      && buffer[line_start + 3] == 't') {
    put_char('b');
    put_char('y');
    put_char('e');
    send_display(buffer);
    halt();

  } else if (buffer[line_start] == 'c'
             && buffer[line_start + 1] == 'd'
             && buffer[line_start + 2] == ' ') {
    line_start += 3;
    while (buffer[line_start] != 0 && line_start < line_end) {
      argument[ptr] = buffer[line_start];
      line_start += 1;
      ptr += 1;
    }
    debug(current_directory_id);
    change_directory(argument, ptr);
    debug(current_directory_id);

  } else if (buffer[line_start] == 0) {
    return;
  } else {
    ptr = 0;
    while (buffer[line_start] != ' '
           && buffer[line_start] != 0
           && line_start < line_end) {
      int byte = buffer[line_start];
      if (byte >= 'a' && byte <= 'z') {
        byte -= 0x20;
      }
      program_name[ptr] = byte;
      line_start += 1;
      ptr += 1;
    }
    program_name[ptr] = 0;

    while (buffer[line_start] == ' ' && line_start < line_end) {
      line_start += 1;
    }

    ptr = 0;
    initialize_array(argument, 128, 0);
    while (buffer[line_start] != ' '
           && buffer[line_start] != 0
           && line_start < line_end) {
      argument[ptr] = buffer[line_start];
      line_start += 1;
      ptr += 1;
    }
    argument[ptr] = 0;

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
