char token[1024];
char file_content[0x1000];
char valid_entry_ids[512];
char entry_line[32];


void main() {
  int cluster_id = 0;
  int argument_pointer = 0;
  int entry_id = 0;
  int entry_count = 0;
  int i = 0;

  if (argument[0] != '/') {
    error(PATH_NOT_FOUND);
  }

  if (argument[0] == '/' && argument[1] == 0) {
    cluster_id = 0;
  } else {
    while (argument[argument_pointer] == '/') {
      argument_pointer += 1;
      argument_pointer += basename(argument + argument_pointer, token);
      entry_id = find_entry_by_name(cluster_id, token);
      cluster_id = get_cluster_id(cluster_id, entry_id);
    }
  }

  entry_count = get_valid_entries(cluster_id, valid_entry_ids);
  argument_pointer = 0;
  i = 0;
  while (i < entry_count) {
    argument_pointer += get_entry_name(cluster_id, valid_entry_ids[i], argument + argument_pointer);
    if (get_is_directory(cluster_id, valid_entry_ids[i])) {
      argument[argument_pointer] = '/';
      argument_pointer += 1;
    } else {
      int file_size = 0;
      argument[argument_pointer] = ' ';
      argument_pointer += 1;
      file_size = get_file_size(cluster_id, valid_entry_ids[i]);
      argument_pointer += int_to_string(file_size, argument + argument_pointer);
    }
    argument[argument_pointer] = '\n';
    argument_pointer += 1;
    i += 1;
  }
  return;
}
