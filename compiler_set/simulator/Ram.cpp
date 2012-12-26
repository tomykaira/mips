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
}

/**
 * Default destructor
 */
Ram::~Ram() {
}

void Ram::validate(unsigned int index, std::string description) {
  if (index < 0) {
    throw(MemoryException(index, "address < 0", description));
  }

  if (index >= RAM_SIZE) {
    throw(MemoryException(index, "address >= RAM_SIZE", description));
  }
}

uint32_t& Ram::operator[] (unsigned int index) {
  validate(index, "load / store access");
  return (ram_[index]);
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
  ss << "Memory access error: Address:" << address << " Description:" << description << " Cause:" << cause;
  return ss.str();
}

/// stream output operator
std::ostream& operator<<(std::ostream& lhs, const MemoryException& rhs) {
  lhs << std::string(rhs);
  return lhs;
}
