// Mateusz Łuczyński 331826 L4.Z2
#define __AVR_ATmega328__

#include <avr/io.h>

// setup -> voltage divider with photoresistor (5V) and 10k resistor (GND)

void setup() {
  ADCSRA |= _BV(ADEN); // ADC -> ON
  ADMUX |= _BV(REFS0); // ADC Vref -> Vcc, ADC input -> ADC0
  ADCSRA |= _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0); // ADC prescaler -> 128
  DIDR0 |= _BV(ADC0D); // disable digital input on ADC0
 
  // fast PWM, non-inverting mode, top -> ICR1, prescaler 256
  ICR1 = 1023; 
  TCCR1A |= _BV(WGM11) | _BV(COM1A1);  
  TCCR1B |= _BV(WGM12) | _BV(WGM13) | _BV(CS12); 
  DDRB |= _BV(PB1); // output on pin PB1
}

uint16_t analog_read() {
  ADCSRA |= _BV(ADSC);
  while(ADCSRA & _BV(ADSC));
  return ADC;
}

const uint16_t brightness[] = {0, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023};

int main() {
  setup();
  while(1) { OCR1A = brightness[10 - analog_read()/100]; }
}

