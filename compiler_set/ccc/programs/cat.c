char file_content[0x1000];

// parent_directory_id, entry_id, cluster_id
int resolve_result[3];

void main() {
  int directory_id = 0;
  int entry_id = 0;
  int cluster_id = 0;
  int file_size = 0;

  if (resolve_argument_path(argument[ARGUMENT_HEAP_SIZE-1], argument, resolve_result) == -1) {
    copy_string(argument, file_not_found_error_message);
    return;
  }
  directory_id = resolve_result[0];
  entry_id     = resolve_result[1];
  cluster_id   = resolve_result[2];

  file_size = get_file_size(directory_id, entry_id);
  read_file(cluster_id, file_size, file_content);
  copy_n_string(argument, file_content, file_size);
  initialize_array(argument + file_size, ARGUMENT_HEAP_SIZE - file_size, 0);
  return;
}
