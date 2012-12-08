#define ATOM_LENGTH 32
#define ATOM_COUNT 100
#define CONS(x, y) (((x) << 16) + (y))
#define CAR(c) (expression[c] >> 16)
#define CDR(c) ((expression[c] << 16) >> 16)
#define ATOM(x) (expression[x] < 0xffff)
#define GEN_ATOM(x) (x)
#define NTH_ATOM(x) ((x) << 6)

int str_equal(char * str1, char * str2, int length);
int copy_string(char *dest, char * src);
int copy_n_string(char *dest, char * src, int length);

char id_map[3200];
char input[1024];
int input_pointer = 0;
int expression[1024];

int exp_counter = 0;
int atom_counter = 0;

char output[1024];
int output_pointer = 0;

int exp_id() {
  exp_counter += 1;
  return exp_counter;
}

int atom_id() {
  atom_counter += 1;
  return atom_counter;
}

void read_input() {
  int i = 0;
  int byte = 0;
  while (i < 1024) {
    byte = inputb();
    if (byte == '\n') {
      break;
    }
    input[i] = byte;
    i += 1;
  }
}

void reconstruct(int exp_id) {
  if (ATOM(exp_id)) {
    output_pointer += copy_string(output + output_pointer, id_map + NTH_ATOM(expression[exp_id]));
  } else {
    output[output_pointer] = '(';
    output_pointer += 1;

    reconstruct(CAR(exp_id));

    output[output_pointer] = ' ';
    output[output_pointer + 1] = '.';
    output[output_pointer + 2] = ' ';
    output_pointer += 3;

    reconstruct(CDR(exp_id));

    output[output_pointer] = ')';
    output_pointer += 1;
  }
}

int evaluate(int exp_id) {
  if (ATOM(exp_id)) {
    error(exp_id);
  } else {
    switch (expression[CAR(exp_id)]) {
    case 8:
      output_pointer = 0;
      reconstruct(CDR(exp_id));
      send_rs(output, output_pointer);
      break;
    default:
      error(exp_id + 1);
      break;
    }
  }
}

void skip_space() {
  while (input[input_pointer] == ' ') {
    input_pointer += 1;
  }
}

void parse_input(int id) {
  int exp_start = input_pointer;

  if (input[exp_start] == '(') {
    // read S expression
    int left = exp_id();
    int right = exp_id();
    expression[id] = CONS(left, right);
    input_pointer += 1;
    skip_space();
    parse_input(left);
    skip_space();
    input_pointer += 1; // .
    skip_space();
    parse_input(right);
    skip_space();
    input_pointer += 1; // )
    skip_space();

  } else {
    // read ATOM
    int length = 0;
    int atom_pointer = 1;
    int new_id = atom_id();

    while (input[input_pointer] != ' '
           && input[input_pointer] != ')') {
      length += 1;
      input_pointer += 1;
    }

    while (atom_pointer <= atom_counter) {
      if (str_equal(id_map + NTH_ATOM(atom_pointer), input + exp_start, length)) {
        expression[id] = GEN_ATOM(atom_pointer);
        return;
      }
      atom_pointer += 1;
    }

    // undefined atom
    copy_n_string(id_map + NTH_ATOM(new_id), input + exp_start, length);
    expression[id] = GEN_ATOM(new_id);
  }
}

int main() {
  // predefined functions
  id_map[NTH_ATOM(1) + 0] = 'c';
  id_map[NTH_ATOM(1) + 1] = 'a';
  id_map[NTH_ATOM(1) + 2] = 'r';
  id_map[NTH_ATOM(2) + 0] = 'c';
  id_map[NTH_ATOM(2) + 1] = 'd';
  id_map[NTH_ATOM(2) + 2] = 'r';
  id_map[NTH_ATOM(3) + 0] = 'c';
  id_map[NTH_ATOM(3) + 1] = 'o';
  id_map[NTH_ATOM(3) + 2] = 'n';
  id_map[NTH_ATOM(3) + 3] = 's';
  id_map[NTH_ATOM(4) + 0] = 'q';
  id_map[NTH_ATOM(4) + 1] = 'u';
  id_map[NTH_ATOM(4) + 2] = 'o';
  id_map[NTH_ATOM(4) + 3] = 't';
  id_map[NTH_ATOM(4) + 4] = 'e';
  id_map[NTH_ATOM(5) + 0] = 'e';
  id_map[NTH_ATOM(5) + 1] = 'q';
  id_map[NTH_ATOM(6) + 0] = 'a';
  id_map[NTH_ATOM(6) + 1] = 't';
  id_map[NTH_ATOM(6) + 2] = 'o';
  id_map[NTH_ATOM(6) + 3] = 'm';
  id_map[NTH_ATOM(7) + 0] = 'c';
  id_map[NTH_ATOM(7) + 1] = 'o';
  id_map[NTH_ATOM(7) + 2] = 'n';
  id_map[NTH_ATOM(7) + 3] = 'd';
  id_map[NTH_ATOM(8) + 0] = 'p';
  id_map[NTH_ATOM(8) + 1] = 'r';
  id_map[NTH_ATOM(8) + 2] = 'i';
  id_map[NTH_ATOM(8) + 3] = 'n';
  id_map[NTH_ATOM(8) + 4] = 't';

  atom_counter = 8;

  read_input();

  input_pointer = 0;
  parse_input(0);

  evaluate(0);
}
