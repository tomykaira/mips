#ifndef RAM_H_
#define RAM_H_

#include <iosfwd>
#include <string>
#include <stdint.h>

#define RAM_SIZE (512*1024*1024/4)

#define SRAM_BITS 22
#define SRAM_SIZE (4*1024*1024)

#define BLOCK_BITS 7
#define BLOCK_SIZE (512/4)  // words

#define ASSOCIATIVITY_BITS 1
#define ASSOCIATIVITY      2

#define SET_BITS (SRAM_BITS - BLOCK_BITS - ASSOCIATIVITY_BITS)
#define SET_SIZE (SRAM_SIZE / BLOCK_SIZE / ASSOCIATIVITY)

#define BLOCK_COUNT (SRAM_SIZE / BLOCK_SIZE)

class Ram {
  // RAM
  uint32_t * ram_;

  // valid bit
  bool * valid_;
  bool * dirty_;
  uint32_t * tag_;
  uint32_t * history_;

  int write_;
  int read_;

  void update_history(int set_id, int latest_index);
  void mark_dirty(int block);

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

  void set(uint32_t address, uint32_t value);
  uint32_t get(uint32_t address);

  void validate(uint32_t address);

  int load_block(uint32_t address);

  int getRead() {
    return read_;
  }

  int getWrite() {
    return write_;
  }

};

/// stream output operator
std::ostream& operator<<(std::ostream& lhs, const Ram& rhs);

class MemoryException {
public:
  const uint32_t    address;
  const std::string cause;

  MemoryException(uint32_t address, std::string cause) :
    address(address),
    cause(cause)
  {
  }

  operator std::string() const;
};

/// stream output operator
std::ostream& operator<<(std::ostream& lhs, const MemoryException& rhs);


#endif /* RAM_H_ */
