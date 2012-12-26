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
}

/**
 * Default destructor
 */
Ram::~Ram() {
  delete [] ram_;
}

inline void Ram::validate(uint32_t index) {
  if (index < 0) {
    throw(MemoryException(index, "address < 0"));
  }

  if (index >= RAM_SIZE) {
    throw(MemoryException(index, "address >= RAM_SIZE"));
  }
}

void Ram::set(uint32_t address, uint32_t value) {
  validate(address);
  ram_[address] = value;
}

uint32_t Ram::get(uint32_t address) {
  validate(address);
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
