#define PATH_NOT_FOUND 0xf08

char token[1024];
char file_content[0x4000];
char valid_entry_ids[512];
char entry_line[32];

void main() {
  int cluster_id = 0;
  int new_cluster_id = 0;
  int argument_pointer = 0;
  int entry_id = 0;
  int empty_index = 0;
  int i = 0;

  if (argument[0] != '/') {
    error(PATH_NOT_FOUND);
  }

  argument_pointer += 1;
  argument_pointer += basename(argument + argument_pointer, token);
  while (argument[argument_pointer] == '/') {
    entry_id = find_entry_by_name(cluster_id, token);
    cluster_id = get_cluster_id(cluster_id, entry_id);
    argument_pointer += 1;
    argument_pointer += basename(argument + argument_pointer, token);
  }

  send_rs(token, 10);

  // create directory
  new_cluster_id = create_fat_entry();
  debug(new_cluster_id, cluster_id);
  create_empty_directory(new_cluster_id, cluster_id);
  empty_index = find_empty_directory_index(cluster_id);
  create_file_entry(cluster_id, empty_index, 1, new_cluster_id, 0, token);

  i = 0;
  while ( i < ARGUMENT_HEAP_SIZE ) {
    argument[i] = 0;
    i += 1;
  }
}
