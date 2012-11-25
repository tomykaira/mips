#include "Display.h"

#include <iostream>
#include <cassert>

using namespace std;

Display::Display() {
	for (int i = 0; i < COLUMNS*ROWS; i++) {
		buffer[i] = 0;
	}
}

void Display::set(uint32_t index, uint32_t ch) {
	assert(index < DISPLAY_SIZE);
	buffer[index] = ch & 0x7f;
}

void Display::preview() {
	for (int x = 0; x < COLUMNS + 2; x++) {
		cout << "-";
	}
	cout << endl;
	for (int y = 0; y < ROWS; y++) {
		cout << "|";
		for (int x = 0; x < COLUMNS; x++) {
			if (buffer[y*COLUMNS + x] > 32) {
				cout << buffer[y*COLUMNS + x];
			} else {
				cout << " ";
			}
		}
		cout << "|" << endl;
	}
	for (int x = 0; x < COLUMNS + 2; x++) {
		cout << "-";
	}
	cout << endl;
	cout.flush();
}
