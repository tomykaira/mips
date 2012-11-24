#ifndef _DISPLAY_H_
#define _DISPLAY_H_

#define COLUMNS 80
#define ROWS 30
#define DISPLAY_SIZE COLUMNS*ROWS

#include <stdint.h>


class Display {
 private:
	char buffer[DISPLAY_SIZE];

 public:
	Display();
	void set(uint32_t index, uint32_t ch);
	void preview();
};

#endif /* _DISPLAY_H_ */
