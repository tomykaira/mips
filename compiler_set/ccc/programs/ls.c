char token[1024];
char file_content[0x1000];
char valid_entry_ids[0x80];
char entry_line[32];

// parent_directory_id, entry_id, cluster_id
int resolve_result[3];

void main() {
  int cluster_id = 0;
  int argument_pointer = 0;
  int entry_id = 0;
  int entry_count = 0;
  int i = 0;

  resolve_argument_path(argument[ARGUMENT_HEAP_SIZE-1], argument, resolve_result);
  cluster_id = resolve_result[2];

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
  argument[argument_pointer] = 0;
  return;
}
