// Mateusz Łuczyński 331826 L13.Z2
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

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
  ADMUX |= _BV(REFS0); // Vref = 5V
  ADCSRA |= _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // prescaler = 128

  DDRD |= _BV(PD2);
}

float measure(uint8_t channel) {
  ADMUX &= ~0b1111;
  ADMUX |= channel;
  ADCSRA |= _BV(ADSC);
  while (ADCSRA & _BV(ADSC));
  uint16_t reading = ADC;
  return reading/1023.0 * 5.2;
}

#define ADC1 0b0001
#define ADC2 0b0010

int main() {
  setup();
  while(1) {
    printf("Obciazenie %s: \r\n", PORTD & _BV(PD2) ? "wlaczone" : "wylaczone");
    printf("ADC1: %.2fV, ADC2: %.2fV \r\n", measure(ADC1), measure(ADC2));
    PORTD ^= _BV(PD2);
    _delay_ms(3000);
  }
  return 0;
}