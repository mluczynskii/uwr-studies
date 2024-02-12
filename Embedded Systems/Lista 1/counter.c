// Mateusz Łuczyński 331826 L1Z3
#include <avr/sfr_defs.h>
#include <inttypes.h>
#include <util/delay.h>
#include <avr/io.h>

#define LED_DDR DDRD
#define LED_PORT PORTD

#define DELAY 1000 

// pin -> segment configuration:
// PD0 = F, PD1 = G, PD2 = D, PD3 = E 
// PD4 = DP, PD5 = C, PD6 = A, PD7 = B

const int8_t digits[] = {
  0b00010010, // 0
  0b01011111, // 1 
  0b00110001, // 2 
  0b00011001, // 3 
  0b01011100, // 4 
  0b10011000, // 5 
  0b10000000, // 6 
  0b00011111, // 7 
  0b00010000, // 8 
  0b00001100, // 9
};

// changes displayed digit to n
void change (int n) {
  LED_PORT |= ~0b0;
  LED_PORT &= digits[n];
}

int main () {
  LED_DDR |= ~0b0;
  int n = 0;
  while (1) {
    change(n);
    _delay_ms(DELAY); 
    n = (n + 1) % 10;
  }
}
