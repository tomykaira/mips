// parent_directory_id, entry_id, cluster_id
int resolve_result[3];

void main() {
  int directory_id = 0;
  int cluster_id = 0;

  resolve_argument_path(argument[ARGUMENT_HEAP_SIZE-1], argument, resolve_result);
  directory_id = resolve_result[0];
  cluster_id   = resolve_result[2];

  delete_file(directory_id, cluster_id);
  return;
}
