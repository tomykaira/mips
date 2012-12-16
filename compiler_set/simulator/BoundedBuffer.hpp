#ifndef STD_BOUNDEDBUFFER_H_
#define STD_BOUNDEDBUFFER_H_

#include <iosfwd>
#include <vector>

using namespace std;

class BoundedBuffer {
	vector<int> _content;
	const size_t _size;
	int _pointer;
	int _valid_element_count;
public:
	/// Default constructor
	BoundedBuffer(size_t size);
	/// Destructor
	~BoundedBuffer();
	void add(int element);
	vector<int> valid_elements() const;
};

/// stream output operator
std::ostream& operator<<(std::ostream& lhs, const BoundedBuffer& rhs);

#endif /* STD_BOUNDEDBUFFER_H_ */
