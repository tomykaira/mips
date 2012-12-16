#define FAT_TABLE1 0x00101000
#define FAT_TABLE2 0x00120000
#define TABLE_SIZE 0x0001f000
#define RDE 0x0013f000
#define DIRECTORY_ENTRY_SIZE 0x80 // 512
#define DIRECTORY_ENTRY_LINE 0x20 // 32
#define USER_DATA 0x143000
#define CLUSTER_SIZE 0x1000

#define ENTRY_NOT_FOUND_ID 0xfff

#define TOO_LARGE_FILE 0xf01
#define NO_EMPTY_TABLE 0xf02
#define NO_EMPTY_DIRECTORY_ENTRY_POSITION 0xf03
#define CLUSTER_NOT_FOUND 0xf04
#define PATH_NOT_FOUND 0xf05
#define NO_SUCCESSOR_CLUSTER 0xf06

#define E(id, offset) (((id) << 5) + (offset))
#define D(base, id, offset) ((base) + ((id) << 5) + (offset))
#define DIRECTORY_BIT(attribute) (((attribute) << 27) >> 26)
#define FAT_ENTRY(address) ((read_sd(address + 1) << 8) + read_sd(address))

char file_not_found_error_message[32] = "File not found";
char no_fat_entry_error_message[32] = "Could not allocate FAT entry";
char no_empty_index_error_message[32] = "no empty index";


int B(int cluster_id) {
  if (cluster_id == 0) {
    return RDE;
  } else {
    return (((cluster_id) - 2) << 12) + USER_DATA;
  }
}

int output_length = 0;
int file_length = 0;

char directory_name[8];
char output[1024];
char filename[8];
char extname[3];

int directory_entries[0x1000];

// dummy, to delete
void put_char() {
  return;
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

int get_valid_entries(int cluster_id, int entries) {
  int address = B(cluster_id);
  int entry_count = 0;
  int disk_entry_id = 0;

  while ( disk_entry_id < DIRECTORY_ENTRY_SIZE ) {
    int byte = read_sd(D(address, disk_entry_id, 0));
    if (byte != 0 && byte != 0x05 && byte != 0xe5) {
      entries[entry_count] = disk_entry_id;
      entry_count += 1;
    }
    disk_entry_id += 1;
  }
  return entry_count;
}

////////////////////////////////////////////////////////////////
// Find directory entry
// find index in the given cluster_id

int find_empty_directory_index(int cluster_id) {
  int index = 0;
  int de = B(cluster_id);
  while (index < DIRECTORY_ENTRY_SIZE) {
    int first = read_sd(D(de, index, 0));
    if (first == 0) {
      return index;
    }
    index += 1;
  }
  error(NO_EMPTY_DIRECTORY_ENTRY_POSITION);
  return -1;
}

int find_entry_by_name(int cluster_id, char * token) {
  int disk_entry_id = 0;
  int logical_entry_id = 0;
  int ptr = 0;

  int i = 0;
  int token_pointer = 0;
  int got_null = 0;
  int de = B(cluster_id);
  int is_special_directory = 0;

  if ((token[0] == '.' && token[1] == 0) || (token[0] == '.' && token[1] == '.' && token[2] == 0)) {
    is_special_directory = 1;
  }

  while ( disk_entry_id < DIRECTORY_ENTRY_SIZE ) {
    int address = D(de, disk_entry_id, 0);
    int byte = read_sd(address);
    if (byte != 0 && byte != 0x05 && byte != 0xe5 && byte == token[0]) {
      int matching = 1;

      token_pointer = 1;
      ptr = 1;

      while ( ptr < 8 && matching ) {
        byte = read_sd(address + ptr);
        if (byte == token[token_pointer]) {
          token_pointer += 1;
          ptr += 1;
        } else if (byte == 0x20
                   && (token[token_pointer] == 0 ||
                       (token[token_pointer] == '.' && !is_special_directory))) {
          break;
        } else {
          matching = 0;
          break;
        }
      }
      if (matching) {
        if (token[token_pointer] == '.') {
          token_pointer += 1;
          ptr = 8;
          while (ptr < 11) {
            byte = read_sd(address + ptr);
            if (byte == token[token_pointer]) {
              token_pointer += 1;
              ptr += 1;
            } else if (byte == 0x20 && token[token_pointer] == 0) {
              break;
            } else {
              matching = 0;
              break;
            }
          }
          if (matching) {
            return disk_entry_id;
          }
        } else if (read_sd(address + 8) == 0x20) { // both has no extname
          return disk_entry_id;
        }
      }
    }
    disk_entry_id += 1;
  }
  error(CLUSTER_NOT_FOUND);
  return ENTRY_NOT_FOUND_ID;
}

int find_entry_by_cluster_id(int cluster_id, int file_cluster_id) {
  int index = 0;
  int de = B(cluster_id);
  while (index < DIRECTORY_ENTRY_SIZE) {
    int address = D(de, index, 0);
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
  return ENTRY_NOT_FOUND_ID;
}

////////////////////////////////////////////////////////////////
// Get entry data from directory entries

int get_cluster_id(int directory_cluster_id, int entry_id) {
  int address = D(B(directory_cluster_id), entry_id, 0);
  return (read_sd(address + 27) << 8) + read_sd(address + 26);
}

int get_file_size(int directory_cluster_id, int entry_id) {
  int address = D(B(directory_cluster_id), entry_id, 0);
  return (read_sd(address + 31) << 24)
    + (read_sd(address + 30) << 16)
    + (read_sd(address + 29) << 8)
    + read_sd(address + 28);
}

int get_entry_name(int directory_cluster_id, int entry_id, char * entry_name) {
  int address = D(B(directory_cluster_id), entry_id, 0);
  int length = 0;
  int ptr = 0;
  int byte = 0;

  while (ptr < 8) {
    byte = read_sd(address + ptr);
    if (byte == 0x20)
      break;
    entry_name[length] = byte;
    length += 1;
    ptr += 1;
  }
  byte = read_sd(address + 8);
  if (byte != 0x20) {
    entry_name[length] = '.';
    entry_name[length + 1] = byte;
    length += 2;
    ptr = 9;
    while (ptr < 11) {
      byte = read_sd(address + ptr);
      if (byte == 0x20)
        break;
      entry_name[length] = byte;
      length += 1;
      ptr += 1;
    }
  }
  entry_name[length] = 0;
  return length;
}

int get_is_directory(int directory_cluster_id, int entry_id) {
  int address = D(B(directory_cluster_id), entry_id, 0);

  return DIRECTORY_BIT(read_sd(address + 11));
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


int next_cluster(int cluster_id) {
  int address = FAT_TABLE1 + (cluster_id << 1);
  int entry = FAT_ENTRY(address);
  if (entry == 0xffff) {
    error(NO_SUCCESSOR_CLUSTER);
  } else {
    return entry;
  }
}

int read_file(int cluster_id, int size, char * content) {
  int i = 0;
  int address = B(cluster_id);

  while (i < size && i < CLUSTER_SIZE) {
    content[i] = read_sd(address + i);
    i += 1;
  }
  if (i < size) {
    read_file(next_cluster(cluster_id), size - CLUSTER_SIZE, content + CLUSTER_SIZE);
  }
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
  return -1;
}

int write_name(int address, int max_length, char * name) {
  int pointer = 0;
  int got_null = 0;
  int data = 0;
  int wrote_length = max_length;
  int is_special_directory = 0;

  if ((name[0] == '.' && name[1] == 0) || (name[0] == '.' && name[1] == '.' && name[2] == 0)) {
    is_special_directory = 1;
  }

  while (pointer < max_length) {
    if (name[pointer] == 0 || name[pointer] == 0x20
        || (!is_special_directory && name[pointer] == '.')) {
      got_null = 1;
      wrote_length = pointer;
    }
    if (got_null) {
      data = 0x20;
    } else {
      data = name[pointer];
    }
    write_sd(address + pointer, data);
    pointer += 1;
  }

  return wrote_length;
}


void create_file_entry(int directory_cluster_id, int index, int is_dir, int cluster_id, int size, char * name) {
  int data = 0;
  int i = 0;
  int start_address = D(B(directory_cluster_id), index, 0);
  int name_pointer = 0;

  name_pointer = write_name(start_address, 8, name);
  if (name[name_pointer] == '.') {
    write_name(start_address + 8, 3, name + name_pointer + 1);
  } else {
    write_name(start_address + 8, 3, name + name_pointer);
  }

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
  create_file_entry(cluster_id, 0, 1, cluster_id, 0, filename);

  filename[1] = '.';
  create_file_entry(cluster_id, 1, 1, parent_directory, 0, filename);
}

// read from file[0x1000]
void write_file(int cluster_id, char * file, int length) {
  int start = B(cluster_id);
  int i = 0;

  i = 0;
  while (i < length) {
    write_sd(start + i, file[i]);
    i += 1;
  }
}

void update_file_size(int cluster_id, int file_cluster_id, int new_size) {
  int rde = B(cluster_id);
  int index = find_entry_by_cluster_id(cluster_id, file_cluster_id);
  int address = D(rde, index, 0);

  if (index == ENTRY_NOT_FOUND_ID) {
    return;
  }

  write_sd(address + 28, new_size);
  write_sd(address + 29, new_size >> 8);
  write_sd(address + 30, new_size >> 16);
  write_sd(address + 31, new_size >> 24);
}

void delete_file(int cluster_id, int file_cluster_id) {
  int rde = B(cluster_id);
  int index = find_entry_by_cluster_id(cluster_id, file_cluster_id);
  int address = D(rde, index, 0);
  int fat_address = file_cluster_id << 1;

  if (index == ENTRY_NOT_FOUND_ID) {
    return;
  }

  write_sd(address, 0xe5);

  write_sd(FAT_TABLE1 + fat_address, 0);
  write_sd(FAT_TABLE1 + fat_address + 1, 0);
  write_sd(FAT_TABLE2 + fat_address, 0);
  write_sd(FAT_TABLE2 + fat_address + 1, 0);
}

int basename(char * from, char * to) {
  int i = 0;
  while (from[i] != 0 && from[i] != '/') {
    i += 1;
  }
  copy_n_string(to, from, i);
  to[i] = 0;
  return i;
}

// Resolve argument as path, put parent_directory_id, entry_id, cluster_id to result
char __token[1024];
int resolve_argument_path(int current_directory_id, char * argument, int * result) {
  int directory_id     = 0;
  int cluster_id       = 0;
  int argument_pointer = 0;
  int entry_id         = 0;

  if (argument[0] != '/') { // relative path
    cluster_id = current_directory_id;
  } else {
    argument_pointer += 1;
    cluster_id = 0;
  }

  while (argument[argument_pointer] != 0) {
    argument_pointer += basename(argument + argument_pointer, __token);
    argument_pointer += 1;
    directory_id = cluster_id;
    entry_id     = find_entry_by_name(cluster_id, __token);
    if (entry_id == ENTRY_NOT_FOUND_ID) {
      return -1;
    }
    cluster_id = get_cluster_id(cluster_id, entry_id);
  }

  result[0] = directory_id;
  result[1] = entry_id;
  result[2] = cluster_id;

  return 0;
}
