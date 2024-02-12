// Mateusz Łuczyński 331826 L2.Z4
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <stdint.h>

#define LED_PORT PORTD
#define LED_DDR DDRD

#define DIGIT_1 PC1
#define DIGIT_2 PC2 
#define DIGIT_PORT PORTC 
#define DIGIT_DDR DDRC

#define DELAY 1 // higher value allows to observe the "blinking"

void setup() {
  LED_DDR |= ~0b0;
  LED_PORT &= 0b0;
  DIGIT_DDR |= _BV(DIGIT_1) | _BV(DIGIT_2);
  DIGIT_PORT &= 0b0;
}

const uint8_t digits[] = {
  ~0b0, // blank
  0b11000000, // 0 
  0b11111001, // 1
  0b10100100, // 2 
  0b10110000, // 3 
  0b10011001, // 4 
  0b10010010, // 5 
  0b00000010, // 6 
  0b11111000, // 7 
  0b10000000, // 8 
  0b00011000, // 9
};

void clear() {
  LED_PORT &= 0b0;
  DIGIT_PORT &= 0b0;
}

// assert: LED_PORT == 0b0, DIGIT_PORT == 0b0 
void display(int a, int b) {
  LED_PORT |= digits[a+1];
  DIGIT_PORT |= _BV(DIGIT_2);
  LED_PORT |= 0b10000000; // Remove the 'dot' on the first digit
  _delay_ms(DELAY);
  clear();

  LED_PORT |= digits[b+1];
  DIGIT_PORT |= _BV(DIGIT_1);
  _delay_ms(DELAY);
  clear();
}

int main() {
  setup();
  int a = -1, b = 0;
  while(1) {
    for(int i = 0; i < 1000/(DELAY*2); i++) display(a, b);
    b = b + 1;
    if (b > 9) { a++; b = 0; }

    if (a > 9) { a = -1; b = 0; }
    else if (a == 0) a++;
  }
}







