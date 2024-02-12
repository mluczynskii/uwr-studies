// Mateusz Łuczyński 331826 L4.Z1
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <inttypes.h>

#define BAUD 9600                        
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)

void uart_init() {
  UBRR0 = UBRR_VALUE;
  UCSR0A = 0;
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

void setup() {
  TCCR1B = _BV(CS10); // prescaler -> 1, mode -> normal
}

FILE uart_file;

int main() {
  setup();
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  volatile int8_t x;

  uint16_t a = TCNT1; 
  x = x + x; 
  uint16_t b = TCNT1;
  printf("%"PRIu16"", b-a);
  /*
  TCNT1 = 0; c = c + d; c1 = TCNT1;
  TCNT1 = 0; c = c * d; c2 = TCNT1;
  TCNT1 = 0; c = c / d; c3 = TCNT1;
  printf("Operations on int16_t: addition = %hu, multiplication = %hu, division = %hu (cycles) \r\n", c1, c2, c3);

  TCNT1 = 0; e = e + f; c1 = TCNT1;
  TCNT1 = 0; e = e * f; c2 = TCNT1;
  TCNT1 = 0; e = e / f; c3 = TCNT1;
  printf("Operations on int32_t: addition = %hu, multiplication = %hu, division = %hu (cycles) \r\n", c1, c2, c3);

  TCNT1 = 0; g = g + h; c1 = TCNT1;
  TCNT1 = 0; g = g * h; c2 = TCNT1;
  TCNT1 = 0; g = g / h; c3 = TCNT1;
  printf("Operations on int64_t: addition = %hu, multiplication = %hu, division = %hu (cycles) \r\n", c1, c2, c3);

  TCNT1 = 0; i = i + j; c1 = TCNT1;
  TCNT1 = 0; i = i * j; c2 = TCNT1;
  TCNT1 = 0; i = i / j; c3 = TCNT1;
  printf("Operations on float: addition = %hu, multiplication = %hu, division = %hu (cycles) \r\n", c1, c2, c3);
  */
  while(1) {}
}

