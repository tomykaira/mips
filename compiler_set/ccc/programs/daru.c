#define FAT_TABLE1 0x0c400
#define FAT_TABLE2 0x10200
#define TABLE_SIZE 0x3e00
#define RDE 0x14000
#define DIRECTORY_ENTRY_SIZE 0x200 // 512
#define DIRECTORY_ENTRY_LINE 0x20 // 32

#define E(id, offset) (((id) << 5) + (offset))
#define D(base, id, offset) ((base) + ((id) << 5) + (offset))

int output_length = 0;

char output[1024];

int directory_entries[0x4000];

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

void put_char(char c) {
  output[output_length] = c;
  output_length += 1;
}

int list_directory(int count) {
  int i = 0;

  output_length = 0;
  while (i < count) {
    int file_name = 0;
    int ext_name = 0;
    while (directory_entries[E(i, file_name)] != 0x20) {
      file_name += 1;
    }
    while (directory_entries[E(i, ext_name + 8)] != 0x20) {
      ext_name += 1;
    }

    copy_n_string(output + output_length, directory_entries + E(i, 0), file_name);
    output_length += file_name;
    if (ext_name > 0) {
      put_char('.');

      copy_n_string(output + output_length, directory_entries + E(i, 8), ext_name);
      output_length += ext_name;
    }
    put_char('\n');

    i += 1;
  }
}

void main() {
  int entry_count = 0;

  entry_count = read_directory_entry(RDE);

  list_directory(entry_count);

  send_rs(output, output_length);
  return;
}
