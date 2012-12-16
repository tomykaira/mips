/**
 * BoundedBuffer.cpp
 *
 */
#include "BoundedBuffer.hpp"
#include <ostream>
#include <vector>

using namespace std;


/**
 * Default constructor
 */
BoundedBuffer::BoundedBuffer(size_t size) {
	_content = vector<int>(size);
	_size = size;
	_pointer = 0;
	_valid_element_count = 0;
}

/**
 * Default destructor
 */
BoundedBuffer::~BoundedBuffer() {
}

/**
 * Add an element
 */
void BoundedBuffer::add(int element) {
	_content[_pointer] = element;
	_pointer = (_pointer + 1) % _size;
	_valid_element_count = max(_valid_element_count, _pointer);
}

/**
 * Get valid elements.  Ordered from old to new.
 */
vector<int> BoundedBuffer::valid_elements() const {
	vector<int> ret;
	for (int i = 0; i < _valid_element_count; i++) {
		ret.push_back(_content[(i + _pointer - _valid_element_count + _size) % _size]);
	}
	return ret;
}

/**
 * stream output operator
 */
std::ostream& operator<<(std::ostream& lhs, const BoundedBuffer& rhs) {
	vector<int> elements = rhs.valid_elements();
	vector<int>::iterator it = elements.begin();
	lhs << "BoundedBuffer{";
	while (it < elements.end()-1) {
		lhs << (*it) << ", ";
		it++;
	}
	lhs << (*it);
	lhs << "}";
	return lhs;
}
