// Mateusz Łuczyński 331826 L5.Z2
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <stdio.h>

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
  PORTD |= _BV(PD2); // enable pull-up resistor on PD2 (INT0) 
  
  sei(); // enable global interrupts 

  ADCSRA |= _BV(ADEN); // enable ADC 
  ADMUX |= _BV(REFS0); // Vref -> Vcc, IN -> ADC0 (PC0)
  ADCSRA |= _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // ADC prescaler -> 128
  DIDR0 |= _BV(ADC0D); // disable digital input on ADC0
  ADCSRA |= _BV(ADIE) | _BV(ADATE); // enable ADC interrupts and ADC auto-trigger
  ADCSRB |= _BV(ADTS1); // auto-trigger source -> INT0 
  
  EIMSK |= _BV(INT0); // enable interrupt on INT0 
  EICRA |= _BV(ISC01) | _BV(ISC00); // INT0 interrupt on rising edge
  
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
}

#define R2 10.0 // 10kOhm resistor 
volatile static float R1 = -1.0; // resistance of the photoresistor

ISR(ADC_vect) {
  R1 = 1023.0/(float)ADC * R2 - R2; // calculate resistance after ADC has finished measuring 
}

ISR(INT0_vect) {} // no need to do anything because of auto-trigger 

int main() {
  setup();
  while(1) {
    if (R1 < 0) printf("Resistance has not been measured yet... \r\n");
    else printf("Last measured resistance is: %.2f kOhms \r\n", R1);
    _delay_ms(1000);
  }
}
