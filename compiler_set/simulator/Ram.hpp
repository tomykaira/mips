#ifndef RAM_H_
#define RAM_H_

#include <iosfwd>
#include <string>
#include <stdint.h>

#define RAM_SIZE (512*1024*1024/4)


class Ram {
  // RAM
  uint32_t * ram_;

public:
  /// Default constructor
  Ram();
  /// Destructor
  ~Ram();
  /// Printer
  operator std::string() const;

  // default heap register point.
  // To test heap base related bugs, change this to any positive value (ex. 32).
  int32_t default_hr() { return 0; }

  // should be lesser than RAM_SIZE
  int32_t default_fr() { return RAM_SIZE - 1; }

  uint32_t& operator[] (unsigned int index);

  void validate(unsigned int address, std::string description);

};

/// stream output operator
std::ostream& operator<<(std::ostream& lhs, const Ram& rhs);

class MemoryException {
public:
  const uint32_t    address;
  const std::string cause;
  const std::string description;

  MemoryException(uint32_t address, std::string cause, std::string description) :
    address(address),
    cause(cause),
    description(description)
  {
  }

  operator std::string() const;
};

/// stream output operator
std::ostream& operator<<(std::ostream& lhs, const MemoryException& rhs);


#endif /* RAM_H_ */
