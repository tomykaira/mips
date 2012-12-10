// input:  /hoge/test
// output: find me!

char token[1024];
char file_content[0x4000];

void main() {
  int entry_count = 0;
  int cluster_id = 0;
  int directory_id = 0;
  int empty_index = 0;
  int argument_pointer = 0;
  int file_size = 0;
  int entry_id = 0;

  if (argument[0] != '/') {
    error(PATH_NOT_FOUND);
  }

  while (argument[argument_pointer] == '/') {
    argument_pointer += 1;
    argument_pointer += basename(argument + argument_pointer, token);
    directory_id = cluster_id;
    entry_id = find_entry_by_name(cluster_id, token);
    cluster_id = get_cluster_id(cluster_id, entry_id);
  }

  file_size = get_file_size(directory_id, entry_id);
  read_file(cluster_id, file_size, file_content);
  copy_n_string(argument, file_content, file_size);
  initialize_array(argument + file_size, ARGUMENT_HEAP_SIZE - file_size, 0);
  return;
}
