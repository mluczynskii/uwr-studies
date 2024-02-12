// Mateusz Łuczyński 331826 L2.Z3
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>

#define LED_PORT PORTD
#define LED_DDR DDRD

#define BTN_PIN PINC
#define BTN_PORT PORTC

#define RESET PC5
#define PREV PC3
#define NEXT PC4

#define LIMIT (1 << 8)

int debounce(int state) {
  int new_state = BTN_PIN & (_BV(RESET) | _BV(PREV) | _BV(NEXT));
  if (new_state != state) {
    _delay_ms(5);
    new_state = BTN_PIN & (_BV(RESET) | _BV(PREV) | _BV(NEXT));
  }
  return new_state;
}

void setup() {
  LED_DDR |= ~0b0;
  BTN_PORT |= _BV(RESET) | _BV(PREV) | _BV(NEXT);
}

int convert(int n) {
  return n ^ (n >> 1);
}

int main() {
  setup();
  int state = BTN_PIN & (_BV(RESET) | _BV(PREV) | _BV(NEXT)), n = 0;
  while(1) {
    LED_PORT &= 0b0;
    LED_PORT |= convert(n);
    int new_state = debounce(state);
    if (new_state != state) {
      state = new_state;
      if (state & _BV(RESET) ^ _BV(RESET)) n = 0;
      else if (state & _BV(NEXT) ^ _BV(NEXT)) n = n+1;
      else if (state & _BV(PREV) ^ _BV(PREV)) n = n-1;
      n = (n + LIMIT) % LIMIT;
    }
  }
}
