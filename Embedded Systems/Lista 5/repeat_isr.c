// Mateusz Łuczyński 331826 L5.Z1
#define __AVR_ATmega328__

#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/interrupt.h>

#define LED PD2
#define LED_PORT PORTD
#define LED_DDR DDRD

#define BTN PD3 
#define BTN_PORT PORTD
#define BTN_PIN PIND

// cycling buffer 
#define N 1024
uint8_t buffer[N];
volatile uint16_t idx = 0;

// f = 100Hz -> T = 10ms -> if we want 2s of delay then OFFSET = 200 
#define OFFSET 200

ISR(TIMER1_COMPA_vect) {
  if (buffer[idx]) LED_PORT |= _BV(LED); // check whether the LED should be on or off
  else LED_PORT &= ~_BV(LED);

  if (BTN_PIN & _BV(BTN)) buffer[(idx+OFFSET) % N] = 0; // update LED state in the future
  else buffer[(idx+OFFSET) % N] = 1;
  
  buffer[idx] = 0; // clear already visited timestamps
  idx = (idx + 1) % N;
}

void setup() {
  sei(); // enable global interrupts
  
  LED_DDR |= _BV(LED); // LED GPIO -> output
  BTN_PORT |= _BV(BTN); // enable pull-up for BTN
  
  TCCR1B |= _BV(WGM12); // CTC mode, TOP = OCR1A 
  TCCR1B |= _BV(CS11); // prescaler -> 8
  TIMSK1 |= _BV(OCIE1A); // enable OCR1A compare match interrupt 

  // f = F_CPU / (2 * N * (1 + OCR1A)) 
  // we want f = 100Hz (100 btn probes per second), so OCR1A = (F_CPU/16 - 100) / 100;
  OCR1A = 9999;
}

int main() {
  setup();
  set_sleep_mode(SLEEP_MODE_IDLE);
  while(1) { sleep_mode(); }
}
