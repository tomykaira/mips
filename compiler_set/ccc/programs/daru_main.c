// constant strings
int copied0[8] = "COPIED0";
int new[8] = "NEW";
int txt[3] = "TXT";
int xxx[8] = "xxxxxxxx";

void main() {
  int entry_count = 0;
  int cluster_id = 0;
  int empty_index = 0;

  // create directory
  cluster_id = create_fat_entry();
  create_empty_directory(cluster_id, 0); // parent = RDE
  empty_index = find_empty_directory_index(0);
  copy_n_string(new, 4);
  filename[3] = 0;
  extname[0] = 0;
  create_file_entry(RDE + (empty_index << 5), 1, cluster_id, 0);

  // create file
  // create copied0.txt under the root
  cluster_id = create_fat_entry();
  read_file(0x2d38000, 0x09);
  write_file(cluster_id, file_length);
  empty_index = find_empty_directory_index(0);
  copy_n_string(filename, copied0, 8);
  copy_n_string(extname, txt, 3);
  create_file_entry(RDE + (empty_index << 5), 0, cluster_id, file_length);

  // update file
  // add xxxxx to the last of hoge/test
  cluster_id = 0x0b4a;
  read_file(0x2d38000, 0x09);
  copy_n_string(file + 9, xxx, 8);
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
