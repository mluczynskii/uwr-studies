#define __AVR_ATtiny84__

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>

#define LED_DDR DDRB
#define LED_PORT PORTB 
#define LED PB2

#define BTN_PORT PORTB 
#define BTN_PIN PINB 
#define BTN PB1

void setup() {
  LED_DDR |= _BV(LED);
  BTN_PORT |= _BV(BTN);

  TCCR1B |= _BV(WGM12); // CTC, top -> OCR1A
  TCCR1B |= _BV(CS11); // prescaler -> 8
  TIMSK1 |= _BV(OCIE1A); // OCR1A compare interrupt ON
  OCR1A = 2499; 
  sei();
}

#define SIZE 200
uint8_t state[SIZE] = {0};
volatile uint8_t idx = 0;

ISR (TIM1_COMPA_vect) {
  if (BTN_PIN & _BV(BTN)) state[(idx+100)%SIZE] = 0;
  else state[(idx+100)%SIZE] = 1;

  if (state[idx]) LED_PORT |= _BV(LED);
  else LED_PORT &= ~_BV(LED);

  state[idx] = 0;
  idx = (idx+1)%SIZE;
}

int main() {
  setup();
  set_sleep_mode(SLEEP_MODE_IDLE);
  while(1) {sleep_mode();}
}