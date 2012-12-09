#define FAT_TABLE1 0x0c400
#define FAT_TABLE2 0x10200
#define TABLE_SIZE 0x3e00
#define RDE 0x14000
#define DIRECTORY_ENTRY_SIZE 0x200 // 512
#define DIRECTORY_ENTRY_LINE 0x20 // 32
#define USER_DATA 0x18000
#define CLUSTER_SIZE 0x4000

#define TOO_LARGE_FILE 0xf01
#define NO_EMPTY_TABLE 0xf02
#define NO_EMPTY_DIRECTORY_ENTRY_POSITION 0xf03
#define CLUSTER_NOT_FOUND 0xf04

#define E(id, offset) (((id) << 5) + (offset))
#define D(base, id, offset) ((base) + ((id) << 5) + (offset))
#define DIRECTORY_BIT(attribute) (((attribute) << 27) >> 26)
#define FAT_ENTRY(address) ((read_sd(address + 1) << 8) + read_sd(address))
#define B(cluster_id) ((((cluster_id) - 2) << 14) + 0x18000)

int output_length = 0;
int file_length = 0;

char directory_name[8];
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

int find_empty_directory_index(int rde) {
  int index = 0;
  while (index < DIRECTORY_ENTRY_SIZE) {
    int first = read_sd(D(rde, index, 0));
    if (first == 0) {
      return index;
    }
    index += 1;
  }
  error(NO_EMPTY_DIRECTORY_ENTRY_POSITION);
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
  if (size > CLUSTER_SIZE) {
    error(TOO_LARGE_FILE);
  }
  while (i < size) {
    file[i] = read_sd(address + i);
    i += 1;
  }
  file_length = size;
  return size;
}

// find first empty cluster and mark it as the last cluster (0xffff)
int create_fat_entry() {
  int index = 0;
  int entry = 0;
  while (index < TABLE_SIZE) {
    int address = FAT_TABLE1 + (index << 1);
    entry = FAT_ENTRY(address);
    if (entry == 0) {
      write_sd(address, 0xff);
      write_sd(address + 1, 0xff);
      write_sd(FAT_TABLE2 + (index << 1), 0xff);
      write_sd(FAT_TABLE2 + (index << 1) + 1, 0xff);
      return index;
    }
    read_sd(FAT_TABLE1 + index);
    index += 1;
  }
  error(NO_EMPTY_TABLE);
}

char filename[8];
char extname[3];

void write_filename(int address) {
  int pointer = 0;
  int got_null = 0;
  int data = 0;
  while (pointer < 8) {
    if (filename[pointer] == 0) {
      got_null = 1;
    }
    if (got_null) {
      data = 0x20;
    } else {
      data = filename[pointer];
    }
    write_sd(address + pointer, data);
    pointer += 1;
  }
}

void write_extname(int address) {
  int pointer = 0;
  int got_null = 0;
  int data = 0;
  while (pointer < 3) {
    if (extname[pointer] == 0) {
      got_null = 1;
    }
    if (got_null) {
      data = 0x20;
    } else {
      data = extname[pointer];
    }
    write_sd(address + pointer, data);
    pointer += 1;
  }
}

void create_file_entry(int start_address, int is_dir, int cluster_id, int size) {
  int data = 0;
  int i = 0;

  write_filename(start_address);
  write_extname(start_address + 8);

  if (is_dir) {
    data = 0x10; // directory
  } else {
    data = 0x20; // archive
  }
  write_sd(start_address + 11, data);

  i = 0;
  while (i < 14) {
    write_sd(start_address + 12 + i, 0);
    i += 1;
  }

  write_sd(start_address + 26, cluster_id);
  write_sd(start_address + 27, cluster_id >> 8);

  write_sd(start_address + 28, size);
  write_sd(start_address + 29, size >> 8);
  write_sd(start_address + 30, size >> 16);
  write_sd(start_address + 31, size >> 24);
}

void create_empty_directory(int cluster_id, int parent_directory) {
  int start = B(cluster_id);
  int pointer = 0;
  int i = 0;

  // empty entries should be 0
  while (i < CLUSTER_SIZE) {
    write_sd(start + i, 0);
    i += 1;
  }

  filename[0] = '.';
  {
    int i = 1;
    while (i < 8) {
      filename[i] = 0;
      i += 1;
    }
  }
  create_file_entry(start, 1, cluster_id, 0);

  filename[1] = '.';
  create_file_entry(start + 0x20, 1, parent_directory, 0);
}

// read from file[0x4000]
void write_file(int cluster_id, int length) {
  int start = B(cluster_id);
  int i = 0;

  i = 0;
  while (i < length) {
    write_sd(start + i, file[i]);
    i += 1;
  }
}

int find_directory_entry_index(int cluster_id, int file_cluster_id) {
  int index = 0;
  int rde = B(cluster_id);
  while (index < DIRECTORY_ENTRY_SIZE) {
    int address = D(rde, index, 0);
    int cluster_id = (read_sd(address + 27) << 8) + read_sd(address + 26);
    if (read_sd(address + 0) != 0x00
        && read_sd(address + 0) != 0x05
        && read_sd(address + 0) != 0xe5
        && cluster_id == file_cluster_id) {
      return index;
    }
    index += 1;
  }
  error(CLUSTER_NOT_FOUND);
}

void update_file_size(int cluster_id, int file_cluster_id, int new_size) {
  int rde = B(cluster_id);
  int index = find_directory_entry_index(cluster_id, file_cluster_id);
  int address = D(rde, index, 0);

  write_sd(address + 28, new_size);
  write_sd(address + 29, new_size >> 8);
  write_sd(address + 30, new_size >> 16);
  write_sd(address + 31, new_size >> 24);
}

void delete_file(int cluster_id, int file_cluster_id) {
  int rde = B(cluster_id);
  int index = find_directory_entry_index(cluster_id, file_cluster_id);
  int address = D(rde, index, 0);

  write_sd(address, 0xe5);
}

void main() {
  int entry_count = 0;
  int cluster_id = 0;
  int empty_index = 0;

  // create directory
  cluster_id = create_fat_entry();
  create_empty_directory(cluster_id, 0); // parent = RDE
  empty_index = find_empty_directory_index(RDE);
  filename[0] = 'N';
  filename[1] = 'E';
  filename[2] = 'W';
  filename[3] = 0;
  extname[0] = 0;
  create_file_entry(RDE + (empty_index << 5), 1, cluster_id, 0);

  // create file
  // create copied0.txt under the root
  cluster_id = create_fat_entry();
  read_file(0x2d38000, 0x09);
  write_file(cluster_id, file_length);
  empty_index = find_empty_directory_index(RDE);
  filename[0] = 'C';
  filename[1] = 'O';
  filename[2] = 'P';
  filename[3] = 'I';
  filename[4] = 'E';
  filename[5] = 'D';
  filename[6] = '0';
  extname[0] = 'T';
  extname[1] = 'X';
  extname[2] = 'T';
  create_file_entry(RDE + (empty_index << 5), 0, cluster_id, file_length);

  // update file
  // add xxxxx to the last of hoge/test
  cluster_id = 0x0b4a;
  read_file(0x2d38000, 0x09);
  file[9] = 'x';
  file[10] = 'x';
  file[11] = 'x';
  file[12] = 'x';
  file[13] = 'x';
  file[14] = 'x';
  file[15] = 'x';
  file_length = 16;
  write_file(cluster_id, file_length);
  update_file_size(0xb49, cluster_id, file_length);

  // delete file
  // delete cebackup/backup.bk1
  cluster_id = 0x03;
  delete_file(0x02, cluster_id);

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
