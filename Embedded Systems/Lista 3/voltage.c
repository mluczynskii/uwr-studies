// Mateusz Łuczyński 331826 L3.Z2
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

#define BAUD 9600                        
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   

#define LED PD2 
#define LED_PORT PORTD
#define LED_DDR DDRD

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

void setup() {
  ADCSRA |= _BV(ADEN); // enable ADC 
  ADMUX |= _BV(REFS0); // Vref = Vcc
  ADMUX |= _BV(MUX3) | _BV(MUX2) | _BV(MUX1); // connect 1.1V input to the ADC
  ADCSRA |= _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0); // set prescaler to 128  
  
  LED_DDR |= _BV(LED);
}

FILE uart_file;

// ADC produces 10-bit values, so they range from 0->1023
// -> our measured value V_in = V_cc * (ADC-measured value / 1023)
// assuming V_in = 1.1V, we can solve for V_cc: V_cc = (1.1 * 1023) / ADC-measured value.
int main() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  setup();
  while(1) {
    printf("Starting conversion... \r\n");
    ADCSRA |= _BV(ADSC); // start conversion

    LED_PORT |= _BV(LED); // LED experiment
    while (ADCSRA & _BV(ADSC)); // wait for conversion to finish
    LED_PORT &= ~_BV(LED);

    uint16_t value = ADC; // get measured value
    printf("Measured value: %d \r\n", value);
    float vcc = 1.1 * 1023.0 / value; // calculate V_cc
    printf("Vcc equals: %.2f V \r\n\n", vcc);
    _delay_ms(2000); // wait for next measurement
  }
}
