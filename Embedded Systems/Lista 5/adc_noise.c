// Mateusz Łuczyński 331826 L5.Z3
#define __AVR_ATmega328__

#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <util/delay.h>

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

  ADCSRA |= _BV(ADEN);
  ADMUX |= _BV(REFS0);
  ADMUX |= _BV(MUX3) | _BV(MUX2) | _BV(MUX1);
  ADCSRA |= _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);
}

#define N 10
uint16_t id_a = 0, id_b = 0;
float a[N], b[N];

#define VCC 5.2
// 1.1V ~= ADC/1023.0 * Vcc

ISR(ADC_vect) {
  a[id_a] = ADC/1023.0 * VCC;
}

void noise(uint16_t idx) {
  id_b = idx;
  ADCSRA |= _BV(ADSC);
  while (ADCSRA & _BV(ADSC));
  b[id_b] = ADC/1023.0 * VCC;
}

// assert: ADC interrupts are ON 
void no_noise(uint16_t idx) {
  id_a = idx; 
  set_sleep_mode(SLEEP_MODE_ADC);
  sleep_mode(); // CPU will resume after ADC interrupt happens and ISR will store the result in a[]
}

float sigma(uint8_t reduction) {
  float acc = 0.0;
  for (int i = 0; i < N; i++) {
    float item = (reduction ? a[i] : b[i]);
    acc += (item - 1.1) * (item - 1.1);
  }
  return acc / N;
}

int main() {
  setup();
  while(1) {
    // start off with measurements without ADC noise reduction 
    for (int idx = 0; idx < N; idx++) noise(idx);

    // enable interrupts and proceed with measurements with ADC noise reduction 
    sei();
    ADCSRA |= _BV(ADIE);
    for (int idx = 0; idx < N; idx++) no_noise(idx);

    // print out the results 
    for (int i = 0; i < N; i++) printf("a[%d] -> %.4f, b[%d] -> %.4f \r\n", i+1, a[i], i+1, b[i]);
    printf("(variation) a -> %.12f, b -> %.12f \r\n", sigma(1), sigma(0));

    _delay_ms(10000);
  }
}
