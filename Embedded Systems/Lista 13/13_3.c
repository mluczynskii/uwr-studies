// Mateusz Łuczyński 331826 L13.Z3
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <math.h>

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

FILE uart_file;

void setup() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  ADCSRA |= _BV(ADEN); // ADC enable
  ADMUX |= _BV(REFS0) | _BV(REFS1); // Vref = 1.1V
  ADCSRA |= _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // prescaler = 128
}

#define B 3155 // K
#define R_0 4700.0 // Ω
#define T_0 298.15 // K

#define I 130.0 // µA

float temperature(uint16_t reading) {
  float U = (reading/1023.0) * 1.1 * 1000000.0; // µV
  float R = U/I; // Ω
  float T = B*T_0 / (B - T_0*log(R_0/R)); // K
  return T - 273.15; // °C
}

int main() {
  setup();
  while(1) {
    ADCSRA |= _BV(ADSC);
    while(ADCSRA & _BV(ADSC));
    uint16_t reading = ADC;
    printf("Current temperature is: %.2f °C \r\n", temperature(reading));
    _delay_ms(1000);
  }
  return 0;
}