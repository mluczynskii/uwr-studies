// Mateusz Łuczyński 331826 L2.Z1
#define __AVR_ATmega328__

#include <util/delay.h>
#include <avr/io.h>

typedef long long int time_t;

// LED plugged in pin PD2
#define LED PD2
#define LED_PORT PORTD
#define LED_DDR DDRD

// Pushbutton plugged in pin PC5
#define BTN PC5 
#define BTN_PIN PINC 
#define BTN_PORT PORTC 

#define DELAY 3000 // ms

// Cycling buffer 
#define N 32
typedef struct {
  time_t tab[N];
  int first, last;
} buffer;

double pop(buffer* buf) {
  if (buf->first == buf->last) return -1; // Buffer is empty
  time_t time_stamp = buf->tab[buf->first];
  buf->first = (buf->first + 1) % N;
  return time_stamp;
}

void push(buffer* buf, time_t time_stamp) {
  buf->tab[buf->last] = time_stamp;
  buf->last = (buf->last + 1) % N;
  if (buf->last == buf->first) buf->first = (buf->first + 1) % N;
}

void setup() {
  LED_PORT &= 0b0;
  BTN_PORT |= _BV(BTN); // Enable pull-up resistor for the button
  LED_DDR |= _BV(LED); // Setup LED pin to output
}

time_t clock = 0;

int debounce(int state) {
  int new_state = (BTN_PIN & _BV(BTN));
  if (new_state != state) {
    _delay_ms(10);
    clock = clock + 10;
    new_state = (BTN_PIN & _BV(BTN));
  }
  return new_state;
}

int main() {
  setup();
  buffer buf = {.first = 0, .last = 0};
  time_t next_event = -1;
  int state = (BTN_PIN & _BV(BTN));
  while (1) {
    clock = clock + 1; // we use the assumption that all operations are atomic
                       // and _delay_ms dictates how long one iteration takes
    int new_state = debounce(state);
    if (state != new_state) { 
      push(&buf, clock + DELAY);
      state = new_state;
    } 
    if (next_event == -1) next_event = pop(&buf);
    else if (clock >= next_event) {
      LED_PORT ^= _BV(LED);
      next_event = -1; 
    }
    _delay_ms(1);
  }
}
