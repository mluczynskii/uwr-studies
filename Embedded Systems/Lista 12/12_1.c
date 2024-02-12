// Mateusz Łuczyński 331826 L12.Z1
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <avr/interrupt.h>
#include <string.h>

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

  ADCSRA |= _BV(ADEN) | _BV(ADATE); // enable + auto-trigger
  ADMUX |= _BV(REFS0) | _BV(REFS1); // Vref = 1.1V
  ADCSRA |= _BV(ADIE); // adc interrupt
  ADCSRA |= _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // prescaler = 128
  ADCSRB |= _BV(ADTS0) | _BV(ADTS1); // trigger source = timer0 compare match

  TCCR0A |= _BV(WGM01); // CTC mode
  OCR0A = 0xFF;
  TIMSK0 |= _BV(OCIE0A); // compare match interrupt
  TCCR0B |= _BV(CS02) | _BV(CS00); // prescaler = 1024

  DDRB |= _BV(PB5);

  sei();
}

volatile uint16_t currentTemperature = 0x00, setTemperature = 0x14;

#define T_c 10.0 // mV
#define V_0 500.0 // mV/°C

#define T_h 1 // °C

ISR (ADC_vect) {
  float V_out = ADC/1023.0 * 1.1; // V
  currentTemperature = (V_out * 1000.0 - V_0) / T_c; // °C

  if (currentTemperature > setTemperature) PORTB &= ~_BV(PB5); 
  else if (currentTemperature <= setTemperature - T_h) PORTB |= _BV(PB5);
}

ISR (TIMER0_COMPA_vect) {

}

int main() {
  setup();
  while (1) {
    char command = '\0';
    printf("Enter command: \r\n");
    scanf("%c", &command); printf("%c ", command);
    if (command == 'S') {
      scanf("%"PRIu16"", &setTemperature); printf("%"PRIu16"", setTemperature);
      printf("\r\nTemperature goal has been set to %"PRIu16" \r\n", setTemperature);
    } else if (command == 'R') {
      printf("\r\nCurrent temperature is %"PRIu16" °C \r\n", currentTemperature);
    } else {
      printf("\r\n Usage: \r\n (1) S [T] - sets temperature goal to T \r\n (2) R - reads current temperature \r\n");
    }
  }
  return 0;
}