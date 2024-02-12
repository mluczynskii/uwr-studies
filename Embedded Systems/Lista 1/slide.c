#include <avr/io.h>
#include <util/delay.h> 

#define LED_DDR DDRD 
#define LED_PORT PORTD 

#define DELAY 100

void slide_right () {
  unsigned int curr = 0b1;
  for (int i = 0; i < 8; i++) {
    LED_PORT &= 0b0;
    LED_PORT |= curr;
    curr <<= 1;
    _delay_ms(DELAY);
  }
}

void slide_left () {
  unsigned int curr = 0b10000000;
  for (int i = 0; i < 8; i++) {
    LED_PORT &= 0b0;
    LED_PORT |= curr;
    curr >>= 1;
    _delay_ms(DELAY);
  }
}

int main () {
  LED_DDR |= ~0b0;
  while (1) {
    slide_right();
    slide_left();
  }
}


