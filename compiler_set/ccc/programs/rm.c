char token[1024];
char file_content[0x1000];
char valid_entry_ids[512];
char entry_line[32];


void main() {
  int directory_id = 0;
  int cluster_id = 0;
  int argument_pointer = 0;
  int entry_id = 0;
  int entry_count = 0;
  int i = 0;

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

  delete_file(directory_id, cluster_id);
  return;
}
