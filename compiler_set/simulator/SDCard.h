#ifndef _SDCARD_H_
#define _SDCARD_H_

#include <stdint.h>
#include <cstdio>
#include "simulator.h"

class SDCard {
 private:
	FILE * fp;

 public:
	SDCard(const char * filename);
	~SDCard();
	uint32_t read_at(uint32_t address);
	void write_at(uint32_t address, uint32_t data);
};

#endif /* _SDCARD_H_ */
