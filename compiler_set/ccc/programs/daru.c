#define FAT_TABLE1 0x0c400
#define FAT_TABLE2 0x10200
#define TABLE_SIZE 0x3e00
#define RDE 0x14000
#define DIRECTORY_ENTRY_SIZE 0x200 // 512
#define DIRECTORY_ENTRY_LINE 0x20 // 32
#define USER_DATA 0x18000

#define TOO_LARGE_FILE 0xf01

#define E(id, offset) (((id) << 5) + (offset))
#define D(base, id, offset) ((base) + ((id) << 5) + (offset))
#define DIRECTORY_BIT(attribute) (((attribute) << 27) >> 26)

int output_length = 0;
int file_length = 0;

char output[1024];
char file[0x4000];

int directory_entries[0x4000];

void put_char(char c) {
  output[output_length] = c;
  output_length += 1;
}

int div_binary_search(int a, int b, int left, int right) {
  int mid = (left + right) >> 1;
  int x = mid*b;

  if (right - left <= 1) {
    return left;
  } else {
    if (x < a) {
      return div_binary_search(a, b, mid, right);
    } else if (x == a) {
      return mid;
    } else {
      return div_binary_search(a, b, left, mid);
    }
  }
}

void print_int (int x) {
  int tx = 0;
  int dx = 0;
  int flg = 0;
  if (x<0) {
    put_char('-');
    x = -x;
  }

  tx = div_binary_search(x, 100000000, 0, 3);
  dx = tx*100000000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    put_char('0' + tx);
    flg = 1;
  }

  tx = div_binary_search(x, 10000000, 0, 10);
  dx = tx*10000000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    put_char('0' + tx);
    flg = 1;
  }

  tx = div_binary_search(x, 1000000, 0, 10);
  dx = tx*1000000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    put_char('0' + tx);
    flg = 1;
  }

  tx = div_binary_search(x, 100000, 0, 10);
  dx = tx*100000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    put_char('0' + tx);
    flg = 1;
  }

  tx = div_binary_search(x, 10000, 0, 10);
  dx = tx*10000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    put_char('0' + tx);
    flg = 1;
  }

  tx = div_binary_search(x, 1000, 0, 10);
  dx = tx*1000;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    put_char('0' + tx);
    flg = 1;
  }

  tx = div_binary_search(x, 100, 0, 10);
  dx = tx*100;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    put_char('0' + tx);
    flg = 1;
  }

  tx = div_binary_search(x, 10, 0, 10);
  dx = tx*10;
  x = x - dx;
  if (tx <= 0)
    flg = 0;
  else {
    put_char('0' + tx);
    flg = 1;
  }

  put_char('0' + x);
}

int read_directory_entry(int rde) {
  int disk_entry_id = 0;
  int logical_entry_id = 0;
  int ptr = 0;
  while ( disk_entry_id < DIRECTORY_ENTRY_SIZE ) {
    int first = read_sd(D(rde, disk_entry_id, 0));
    if (first != 0 && first != 0x05 && first != 0xe5) {
      directory_entries[E(logical_entry_id, 0)] = first;
      ptr = 1;
      while ( ptr < DIRECTORY_ENTRY_LINE ) {
        directory_entries[E(logical_entry_id, ptr)] = read_sd(D(rde, disk_entry_id, ptr));
        ptr += 1;
      }
      logical_entry_id += 1;
    }
    disk_entry_id += 1;
  }
  return logical_entry_id;
}

int list_directory(int count) {
  int i = 0;
  int cluster = 0;
  int size = 0;

  output_length = 0;
  while (i < count) {
    int file_name = 0;
    int ext_name = 0;
    while (directory_entries[E(i, file_name)] != 0x20 && file_name < 8) {
      file_name += 1;
    }
    while (directory_entries[E(i, ext_name + 8)] != 0x20 && ext_name < 3) {
      ext_name += 1;
    }

    copy_n_string(output + output_length, directory_entries + E(i, 0), file_name);
    output_length += file_name;
    if (ext_name > 0) {
      put_char('.');

      copy_n_string(output + output_length, directory_entries + E(i, 8), ext_name);
      output_length += ext_name;
    }
    put_char(' ');

    if (DIRECTORY_BIT(directory_entries[E(i, 11)])) {
      put_char('D');
    } else {
      put_char('F');
    }
    put_char(' ');

    cluster = (directory_entries[E(i, 27)] << 8) + directory_entries[E(i, 26)];
    print_int(cluster);
    put_char(' ');

    size = (directory_entries[E(i, 31)] << 24)
      + (directory_entries[E(i, 30)] << 16)
      + (directory_entries[E(i, 29)] << 8)
      + directory_entries[E(i, 28)];
    print_int(size);

    put_char('\n');

    i += 1;
  }
}

int read_file(int address, int size) {
  int i = 0;
  if (size > 0x4000) {
    error(TOO_LARGE_FILE);
  }
  while (i < size) {
    file[i] = read_sd(address + i);
    i += 1;
  }
  file_length = size;
  return size;
}

void main() {
  int entry_count = 0;

  entry_count = read_directory_entry(RDE);
  list_directory(entry_count);
  send_rs(output, output_length);

  entry_count = read_directory_entry(0x2d34000);
  list_directory(entry_count);
  send_rs(output, output_length);

  read_file(0x2d38000, 0x09);
  copy_n_string(output, file, file_length);
  output_length = file_length;
  send_rs(output, output_length);
  return;
}
