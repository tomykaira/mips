/**
 * Ram.cpp
 *
 */
#include "Ram.hpp"
#include <ostream>
#include <sstream>
#include <string>

/**
 * Default constructor
 */
Ram::Ram() {
  ram_ = new uint32_t[RAM_SIZE];
  valid_ = new bool[BLOCK_COUNT];
  dirty_ = new bool[BLOCK_COUNT];
  tag_ = new uint32_t[BLOCK_COUNT];
  history_ = new uint32_t[BLOCK_COUNT];

  write_ = read_ = 0;
}

/**
 * Default destructor
 */
Ram::~Ram() {
  delete [] ram_;
  delete [] valid_;
  delete [] dirty_;
  delete [] tag_;
  delete [] history_;
}

inline void Ram::validate(uint32_t index) {
  if (index < 0) {
    throw(MemoryException(index, "address < 0"));
  }

  if (index >= RAM_SIZE) {
    throw(MemoryException(index, "address >= RAM_SIZE"));
  }
}

void Ram::update_history(int set_id, int latest_index) {
  int j;
  for (j = 0; j < ASSOCIATIVITY; ++j) {
    if (history_[(set_id << ASSOCIATIVITY_BITS) + j] == latest_index - (set_id << ASSOCIATIVITY_BITS))
      break;
  }
  for (--j; j >= 0; --j) {
    history_[(set_id << ASSOCIATIVITY_BITS) + j + 1] = history_[(set_id << ASSOCIATIVITY_BITS) + j];
  }
  history_[set_id << ASSOCIATIVITY_BITS] = latest_index;
}

int Ram::load_block(uint32_t address) {
  int tag = address >> (SET_BITS + BLOCK_BITS);
  int set_id = (address >> BLOCK_BITS) & ((1 << SET_BITS) - 1);
  int invalid_entry = -1;

  for (int i = 0; i < ASSOCIATIVITY; ++i) {
    int index = (set_id << ASSOCIATIVITY_BITS) + i;
    if (valid_[index] && tag_[index] == tag) {
      update_history(set_id, index);
      return index;
    }
    if (!valid_[index]) {
      invalid_entry = index;
    }
  }

  if (invalid_entry != -1) {
    ++read_;
    update_history(set_id, invalid_entry);
    tag_[invalid_entry] = tag;
    valid_[invalid_entry] = true;
    return invalid_entry;
  }

  int to_load = history_[(set_id << ASSOCIATIVITY_BITS) + ASSOCIATIVITY - 1];
  int replaced_index = (set_id << ASSOCIATIVITY_BITS) + to_load;

  if (dirty_[replaced_index]) {
    ++write_;
    dirty_[replaced_index] = false;
  }

  ++read_;
  tag_[replaced_index] = tag;
  valid_[replaced_index] = true;
  update_history(set_id, replaced_index);
  return replaced_index;
}

void Ram::mark_dirty(int block) {
  dirty_[block] = true;
}

void Ram::set(uint32_t address, uint32_t value) {
  validate(address);
  int block = load_block(address);
  ram_[address] = value;
  mark_dirty(block);
}

uint32_t Ram::get(uint32_t address) {
  validate(address);
  load_block(address);
  return ram_[address];
}

Ram::operator std::string() const {
  return "::Ram";
}

/**
 * stream output operator
 */
std::ostream& operator<<(std::ostream& lhs, const Ram& rhs) {
  lhs << std::string(rhs);
  return lhs;
}

MemoryException::operator std::string() const {
  std::stringstream ss;
  ss << "Memory access error: Address:" << address << " Cause:" << cause;
  return ss.str();
}

/// stream output operator
std::ostream& operator<<(std::ostream& lhs, const MemoryException& rhs) {
  lhs << std::string(rhs);
  return lhs;
}
