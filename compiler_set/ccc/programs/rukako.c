#define ATOM_LENGTH 32
#define ATOM_COUNT 256
#define CONS(x, y) (((x) << 16) + (y))
#define CAR(c) (expression[c] >> 16)
#define CDR(c) ((expression[c] << 16) >> 16)
#define CADR(c) (CAR((CDR(c))))
#define CDAR(c) (CDR((CAR(c))))
#define CDDR(c) (CDR((CDR(c))))
#define CAAR(c) (CAR((CAR(c))))
#define CAADR(c) (CAR(CAR(CDR(c))))
#define CADDR(c) (CAR(CDR(CDR(c))))
#define CADAR(c) (CAR(CDR(CAR(c))))
#define ATOM(x) (expression[x] < 0xffff)
#define GEN_ATOM(x) (x)
#define NTH_ATOM(x) ((x) << 6)
#define NILP(x) (x == 0)
#define LISTP(x) (!ATOM(x) || expression[x] == L_NIL)
#define SET_BIT(x) ((x) + (1 << 31))
#define REMOVE_BIT(x) ((x) - (1 << 31))

#define L_NIL      0
#define L_CAR      1
#define L_CDR      2
#define L_CONS     3
#define L_QUOTE    4
#define L_EQ       5
#define L_ATOM     6
#define L_COND     7
#define L_PRINT    8
#define L_T        9
#define L_LAMBDA  10
#define L_LABEL   11
#define L_APPLY   12

#define TOO_MANY_INPUT        0xf01
#define TOO_MANY_EXP          0xf02
#define TOO_MANY_ATOM         0xf03
#define INVALID_PARAM         0xf04
#define TOO_DEEP_CALL         0xf05
#define NEGATIVE_CALL_DEPTH   0xf06
#define LIST_EXPECTED         0xf07
#define NOT_LAMBDA            0xf08
#define INVALID_LABEL_NAME    0xf09
#define FUNCTION_NOT_FOUND    0xf0a
#define R_PAREN_NOT_FOUND     0xf0b

#define DEBUG 0

int str_equal(char * str1, char * str2, int length);
int copy_string(char *dest, char * src);
int copy_n_string(char *dest, char * src, int length);

int evaluate_cond(int exp_id);
void parse_input(int id);
int evaluate(int exp_id);

int input_pointer = 0;
int exp_counter = 0;
int atom_counter = 0;
int output_pointer = 0;
int indent = 0;
int call_depth = 0;

char id_map[8192];
char input[4096];
int env[4096]; // id -> id
int expression[32768];

char output[1024];

int exp_id() {
  if (exp_counter >= 32768) {
    error(TOO_MANY_EXP);
  }
  exp_counter += 1;
  return exp_counter;
}

int atom_id() {
  if (atom_counter >= 256) {
    error(TOO_MANY_ATOM);
  }
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
  if (i >= 1024) {
    error(TOO_MANY_INPUT);
  }
}

void reconstruct_list(int exp_id) {
  if (expression[exp_id] == L_NIL) {
    return;
  }
  output[output_pointer] = ' ';
  output_pointer += 1;
  reconstruct(CAR(exp_id));
  if (LISTP(CDR(exp_id))) {
    reconstruct_list(CDR(exp_id));
  } else {
    output[output_pointer] = ' ';
    output[output_pointer + 1] = '.';
    output[output_pointer + 2] = ' ';
    output_pointer += 3;

    reconstruct(CDR(exp_id));
  }
}

void reconstruct(int exp_id) {
  if (ATOM(exp_id)) {
    output_pointer += copy_string(output + output_pointer, id_map + NTH_ATOM(expression[exp_id]));
  } else {
    output[output_pointer] = '(';
    output_pointer += 1;

    reconstruct(CAR(exp_id));

    if (LISTP(CDR(exp_id))) {
      reconstruct_list(CDR(exp_id));
    } else {
      output[output_pointer] = ' ';
      output[output_pointer + 1] = '.';
      output[output_pointer + 2] = ' ';
      output_pointer += 3;

      reconstruct(CDR(exp_id));
    }

    output[output_pointer] = ')';
    output_pointer += 1;
  }
}

void print(int top_id) {
  output_pointer = 0;
  while (output_pointer < (indent << 1)) {
    output[output_pointer] = ' ';
    output_pointer += 1;
  }
  reconstruct(top_id);
  output[output_pointer] = '\n';
  send_rs(output, output_pointer + 1);
}

int eval_args(int args) {
  while(!NILP(expression[args])) {
    evaluate(CAR(args));
    args = CDR(args);
  }
}

int update_environment(int params, int args) {
  while(!NILP(expression[params]) && !NILP(expression[args])) {
    if (ATOM(CAR(params))) {
      env[(call_depth << 8) + expression[CAR(params)]] = SET_BIT(expression[CAR(args)]);
    } else {
      print(params);
      error(INVALID_PARAM);
    }
    params = CDR(params);
    args = CDR(args);
  }
}

int move_exp(int exp_id) {
  int new_id = exp_id();
  if (ATOM(exp_id)) {
    expression[new_id] = expression[exp_id];
  } else {
    expression[new_id] = CONS(move_exp(CAR(exp_id)), move_exp(CDR(exp_id)));
  }
  return new_id;
}

int before_call() {
  call_depth += 1;
  if (call_depth >= 16) {
    error(TOO_DEEP_CALL);
  }
}

int after_call() {
  call_depth -= 1;
  if (call_depth < 0) {
    error(NEGATIVE_CALL_DEPTH);
  }
}

int find_env(int exp_id) {
  int level = call_depth;
  int found = L_NIL;

  while (level >= 0) {
    found = env[(level << 8) + expression[exp_id]];
    if (!NILP(found)){
      return found;
    }
    level -= 1;
  }
  return L_NIL;
}

int evaluate(int exp_id) {
  if (DEBUG) {
    print(exp_id);
    indent += 1;
  }

  if (ATOM(exp_id)) {
    int found = find_env(exp_id);
    if (!NILP(found)) {
      expression[exp_id] = REMOVE_BIT(found);
    }
  } else {
    switch (expression[CAR(exp_id)]) {
    // car
    case 1:
      evaluate(CADR(exp_id));
      if (ATOM(CADR(exp_id))) {
        error(LIST_EXPECTED);
      }
      expression[exp_id] = expression[CAADR(exp_id)];
      break;

    // cdr
    case 2:
      evaluate(CADR(exp_id));
      if (ATOM(CADR(exp_id))) {
        error(LIST_EXPECTED);
      }
      expression[exp_id] = expression[CDR(CADR(exp_id))];
      break;

    // cons
    case 3:
      evaluate(CADR(exp_id));
      evaluate(CADDR(exp_id));
      expression[exp_id] = CONS(CADR(exp_id), CADDR(exp_id));
      break;

    // quote
    case 4:
      expression[exp_id] = expression[CADR(exp_id)];
      break;

    // eq
    case 5:
      evaluate(CADR(exp_id));
      evaluate(CADDR(exp_id));
      if (expression[CADR(exp_id)] == expression[CADDR(exp_id)]) {
        expression[exp_id] = L_T;
      } else {
        expression[exp_id] = L_NIL;
      }
      break;

    // atom
    case 6:
      evaluate(CADR(exp_id));
      if (ATOM(CADR(exp_id))) {
        expression[exp_id] = L_T;
      } else {
        expression[exp_id] = L_NIL;
      }
      break;

    // cond
    case 7:
      evaluate_cond(CDR(exp_id));
      expression[exp_id] = expression[CDR(exp_id)];
      break;

    // print
    case 8:
      evaluate(CADR(exp_id));
      print(CADR(exp_id));
      expression[exp_id] = expression[CADR(exp_id)];
      break;

    // apply
    case 12:
      {
        int callee = CADR(exp_id);
        int args = CDDR(exp_id);

        eval_args(args);

        before_call();

        // if expression stack is not sufficient,
        // you can save and restore max id here
        if (expression[CAR(callee)] == L_LAMBDA) {
          int new_exp_id = move_exp(CADDR(callee));
          update_environment(CADR(callee), args);
          evaluate(new_exp_id);
          expression[exp_id] = expression[new_exp_id];

        } else if (expression[CAR(callee)] == L_LABEL) {
          int lambda_name = CADR(callee);
          int lambda = CADDR(callee);
          int new_exp_id = 0;

          if (ATOM(lambda_name)) {
            env[(call_depth << 8) + expression[lambda_name]] = SET_BIT(expression[lambda]);
          } else {
            error(INVALID_LABEL_NAME);
          }

          new_exp_id = move_exp(CADDR(lambda));
          update_environment(CADR(lambda), args);
          evaluate(new_exp_id);
          expression[exp_id] = expression[new_exp_id];

        } else {
          error(NOT_LAMBDA);
        }

        after_call();
      }
      break;

    default:
      {
        int found = find_env(CAR(exp_id));
        if (!NILP(found)) {
          int cdr = (REMOVE_BIT(found) << 16) >> 16;
          int new_exp_id = 0;
          int args = CDR(exp_id);

          eval_args(args);

          before_call();

          new_exp_id = move_exp(CADR(cdr));

          update_environment(CAR(cdr), args);
          evaluate(new_exp_id);
          expression[exp_id] = expression[new_exp_id];

          after_call();
        } else {
          print(exp_id);
          error(FUNCTION_NOT_FOUND);
        }
      }
      break;
    }
  }
  if (DEBUG) {
    indent -= 1;
    print(exp_id);
  }
}

int evaluate_cond(int exp_id) {
  if (NILP(expression[exp_id])) {
    return;
  } else {
    evaluate(CAAR(exp_id));
    if (!NILP(expression[CAAR(exp_id)])) {
      evaluate(CADAR(exp_id));
      expression[exp_id] = expression[CADAR(exp_id)];
    } else {
      evaluate_cond(CDR(exp_id));
      expression[exp_id] = expression[CDR(exp_id)];
    }
  }
}

void skip_space() {
  while (input[input_pointer] == ' ') {
    input_pointer += 1;
  }
}

void parse_list(int id) {
  int left = exp_id();
  int right = exp_id();
  if (input[input_pointer] == ')') {
    expression[right] = L_NIL;
  } else {
    expression[id] = CONS(left, right);
    parse_input(left);
    skip_space();
    parse_list(right);
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
    if (input[input_pointer] == '.') {
      input_pointer += 1;
      skip_space();
      parse_input(right);
      skip_space();
    } else {
      parse_list(right);
    }
    if (input[input_pointer] == ')') {
      input_pointer += 1;
    } else {
      error(R_PAREN_NOT_FOUND);
    }
    skip_space();

  } else {
    // read ATOM
    int length = 0;
    int atom_pointer = 0;
    int new_id = atom_id();

    while (input[input_pointer] != ' '
           && input[input_pointer] != ')') {
      length += 1;
      input_pointer += 1;
    }

    while (atom_pointer <= atom_counter) {
      if (str_equal(id_map + NTH_ATOM(atom_pointer), input + exp_start, length)
          && id_map[NTH_ATOM(atom_pointer) + length] == 0) {
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
  // nil
  id_map[NTH_ATOM(L_NIL) + 0] = 'n';
  id_map[NTH_ATOM(L_NIL) + 1] = 'i';
  id_map[NTH_ATOM(L_NIL) + 2] = 'l';

  // car
  id_map[NTH_ATOM(L_CAR) + 0] = 'c';
  id_map[NTH_ATOM(L_CAR) + 1] = 'a';
  id_map[NTH_ATOM(L_CAR) + 2] = 'r';

  // cdr
  id_map[NTH_ATOM(L_CDR) + 0] = 'c';
  id_map[NTH_ATOM(L_CDR) + 1] = 'd';
  id_map[NTH_ATOM(L_CDR) + 2] = 'r';

  // cons
  id_map[NTH_ATOM(L_CONS) + 0] = 'c';
  id_map[NTH_ATOM(L_CONS) + 1] = 'o';
  id_map[NTH_ATOM(L_CONS) + 2] = 'n';
  id_map[NTH_ATOM(L_CONS) + 3] = 's';

  // qoute
  id_map[NTH_ATOM(L_QUOTE) + 0] = 'q';
  id_map[NTH_ATOM(L_QUOTE) + 1] = 'u';
  id_map[NTH_ATOM(L_QUOTE) + 2] = 'o';
  id_map[NTH_ATOM(L_QUOTE) + 3] = 't';
  id_map[NTH_ATOM(L_QUOTE) + 4] = 'e';

  // eq
  id_map[NTH_ATOM(L_EQ) + 0] = 'e';
  id_map[NTH_ATOM(L_EQ) + 1] = 'q';

  // atom
  id_map[NTH_ATOM(L_ATOM) + 0] = 'a';
  id_map[NTH_ATOM(L_ATOM) + 1] = 't';
  id_map[NTH_ATOM(L_ATOM) + 2] = 'o';
  id_map[NTH_ATOM(L_ATOM) + 3] = 'm';

  // cond
  id_map[NTH_ATOM(L_COND) + 0] = 'c';
  id_map[NTH_ATOM(L_COND) + 1] = 'o';
  id_map[NTH_ATOM(L_COND) + 2] = 'n';
  id_map[NTH_ATOM(L_COND) + 3] = 'd';

  // print (my extension)
  id_map[NTH_ATOM(L_PRINT) + 0] = 'p';
  id_map[NTH_ATOM(L_PRINT) + 1] = 'r';
  id_map[NTH_ATOM(L_PRINT) + 2] = 'i';
  id_map[NTH_ATOM(L_PRINT) + 3] = 'n';
  id_map[NTH_ATOM(L_PRINT) + 4] = 't';

  // t
  id_map[NTH_ATOM(L_T) + 0] = 't';

  // lambda
  id_map[NTH_ATOM(L_LAMBDA) + 0] = 'l';
  id_map[NTH_ATOM(L_LAMBDA) + 1] = 'a';
  id_map[NTH_ATOM(L_LAMBDA) + 2] = 'm';
  id_map[NTH_ATOM(L_LAMBDA) + 3] = 'b';
  id_map[NTH_ATOM(L_LAMBDA) + 4] = 'd';
  id_map[NTH_ATOM(L_LAMBDA) + 5] = 'a';

  // label
  id_map[NTH_ATOM(L_LABEL) + 0] = 'l';
  id_map[NTH_ATOM(L_LABEL) + 1] = 'a';
  id_map[NTH_ATOM(L_LABEL) + 2] = 'b';
  id_map[NTH_ATOM(L_LABEL) + 3] = 'e';
  id_map[NTH_ATOM(L_LABEL) + 4] = 'l';

  // apply
  id_map[NTH_ATOM(L_APPLY) + 0] = 'a';
  id_map[NTH_ATOM(L_APPLY) + 1] = 'p';
  id_map[NTH_ATOM(L_APPLY) + 2] = 'p';
  id_map[NTH_ATOM(L_APPLY) + 3] = 'l';
  id_map[NTH_ATOM(L_APPLY) + 4] = 'y';

  atom_counter = L_APPLY;

  read_input();

  input_pointer = 0;
  parse_input(0);

  evaluate(0);
}
