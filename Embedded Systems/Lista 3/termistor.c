// Mateusz Łuczyński 331826 L3.Z4
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <math.h>

#define BAUD 9600                        
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)

#define VCC 5.2 // [V], measured in ex. 2
#define T_0 298.15 // [K] 
#define R_0 4.7 // [kOhm]

#define T_1 290.15 // temperature outside when calculating B -> 17 [C]
#define R_2 2.2 // resistance of the second resistor

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
  ADCSRA |= _BV(ADEN); // ADC -> ON
  ADMUX |= _BV(REFS0); // Vref -> Vcc, input -> ADC0
  ADCSRA |= _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0); // prescaler -> 128
  DIDR0 = _BV(ADC0D);
}

// assert -> V_out is measured in temperature T_1, second resistor is 2.2kOhm 
// the formula for B can be derived from the voltage divisor formula  
float calculate(float V_out) {
  float nominator = log(V_out * R_0 / (VCC - V_out) / R_2);
  float denominator = (1.0 / T_0) - (1.0 / T_1);
  return nominator / denominator;
} // B = 3155K (...?)

#define B 3155.0

// similar to B but now we are solving for T 
float temperature(float V_out) {
  float nominator = B * T_0;
  float denominator = T_0 * log(V_out * R_0 / (VCC - V_out) / R_2) - B;
  return (-1.0) * (nominator / denominator) - 273.15;
}

FILE uart_file;

int main() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  setup();
  while(1) {
    ADCSRA |= _BV(ADSC); // start conversion 
    while(ADCSRA & _BV(ADSC)); 
    uint16_t reading = ADC;
    float V_out = VCC * (reading / 1023.0);
    // printf("B is equal: %f K \r\n", calculate(V_out));
    printf("Current temperature is approx. %.2f Celsius \r\n", temperature(V_out));
    _delay_ms(1000);
  }
}
