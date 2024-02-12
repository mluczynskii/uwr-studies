// Mateusz Łuczyński 331826 L6.Z4
#define __AVR_ATmega328__

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>

void setup() {
  DDRB |= _BV(DDB3) | _BV(DDB5) | _BV(DDB2); // MOSI, SCK, ~SS -> output
  SPCR = _BV(SPE) | _BV(MSTR) | _BV(SPR1); // SPI enable, master mode, 250kHz

  TCCR1B |= _BV(WGM12); // CTC mode, TOP = OCR1A 
  TCCR1B |= _BV(CS12); // prescaler -> 256
  TIMSK1 |= _BV(OCIE1A); // enable OCR1A compare match interrupt 
  
  OCR1A = 31249*2; // 1Hz

  DDRB |= _BV(PB1); // latch control
  DDRB |= _BV(PB2); // on/off switch 
  sei();
}

uint8_t spi_transfer(uint8_t data) {
  cli();
    PORTB &= ~_BV(PB1); // latch current data 
    SPDR = data; // send new value 
    while(!(SPSR & _BV(SPIF)));
    SPSR |= _BV(SPIF);
    PORTB |= _BV(PB1); // update LED based on new value 
  sei();
  return 0;
}

volatile uint8_t curr = 1;

uint8_t numbers[]={
  0b00111111, // 0
  0b00000110, // 1
  0b01011011, // 2
  0b01001111, // 3
  0b01100110, // 4
  0b01101101, // 5
  0b11111101, // 6
  0b00000111, // 7
  0b01111111, // 8
  0b11101111  // 9
};

ISR(TIMER1_COMPA_vect) {
  spi_transfer(numbers[curr]); 
  curr = (curr + 1) % 10;
}

int main() {
  setup();

  PORTB |= _BV(PB1) | _BV(PB2);
  spi_transfer(numbers[0]); // start from 0 not some random stuff 
                            // that happened to be in the register
  PORTB &= ~_BV(PB2);
  set_sleep_mode(SLEEP_MODE_IDLE);
  while(1) {
    sleep_mode();
  }
}
