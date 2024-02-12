// Mateusz Łuczyński 331826 L5.Z4
#define __AVR_ATmega328__

#include <avr/io.h>
#include <stdio.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <util/delay.h>
#include <inttypes.h>
#include <stdbool.h>

#define BAUD 9600                        
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)

void uart_init() {
  UBRR0 = UBRR_VALUE;
  UCSR0B = _BV(RXEN0) | _BV(TXEN0);
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

int uart_transmit(char data, FILE *stream) {
  while(!(UCSR0A & _BV(UDRE0)));
  UDR0 = data;
  return 0;
}

int uart_receive(FILE *stream) {
  while (!(UCSR0A & _BV(RXC0)));
  return UDR0;
}

FILE uart_file;

void setup() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  
  sei(); // enable global interrupts
  
  TCCR1B |= _BV(ICES1); // capture trigger -> rising edge
  TCCR1B |= _BV(CS12) | _BV(CS10); // prescaler -> 1024 
  TIMSK1 |= _BV(ICIE1); // enable input capture interrupts 
}

bool flag = false;
uint16_t first, second;

ISR(TIMER1_CAPT_vect) {
  // diff = second-first = ticks between two rising edges (wave period in ticks)
  // 1 tick (in seconds) = 1/(F_CPU/prescaler) => wave period (in seconds) = diff/(F_CPU/prescaler) 
  // that gives -> wave frequency = (F_CPU/prescaler)/diff
  if (!flag) {
    first = ICR1;
    flag = true;
  }
  else {
    second = ICR1;
    uint32_t frequency = (F_CPU/1024) / (second-first); 
    printf("measured frequency is: %"PRIu32" Hz \r\n", frequency);
    flag = false;
  }
}

int main() {
  setup();
  set_sleep_mode(SLEEP_MODE_IDLE);
  while(1) { sleep_mode(); }
}
