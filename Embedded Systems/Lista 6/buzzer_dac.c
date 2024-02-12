// Mateusz Łuczyński 338162 L6.Z3
#define __AVR_ATmega328__

#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/pgmspace.h>
#include "aux/whatsapp.h"
#include <avr/sleep.h>

void setup() {
  DDRB |= _BV(DDB3) | _BV(DDB5) | _BV(DDB2); // MOSI, SCK, SS -> output  
  SPCR |= _BV(SPE) | _BV(MSTR) | _BV(SPR1); // enable SPI in master mode with 250kHz clock

  TCCR1B |= _BV(WGM12); // CTC mode, TOP = OCR1A 
  TCCR1B |= _BV(CS11); // prescaler -> 8
  TIMSK1 |= _BV(OCIE1A); // enable OCR1A compare match interrupt 

  OCR1A = 124*2+1; 
  
  PORTB |= _BV(PB2);
  sei();
}

uint8_t spi_transfer(uint8_t data) {
  cli(); 
    SPDR = data;
    while (!(SPSR & _BV(SPIF)));
    SPSR |= _BV(SPIF);
  sei();
  return 0;
}

extern const uint8_t sound_raw[] PROGMEM;
extern uint16_t sound_raw_len;

uint8_t flags_mask = 0b0111 << 4;
volatile uint16_t idx = 0;

ISR(TIMER1_COMPA_vect) {
  uint8_t sample = pgm_read_byte(&sound_raw[idx]);
  idx = (idx + 1) % sound_raw_len;
  PORTB &= ~_BV(PB2); // set ~CS to 0
  spi_transfer(flags_mask | (sample >> 4)); // transmit flags + 4 high bits of the sample 
  spi_transfer(sample << 4); // transmit remaining 4 bits of the sample (NOTE: MCP4901 has 8-bit DAC so the 
                             // 4 least significant bits of the 16-bit register get ignored -> 
                             // FLAGS[15:11) | DATA[11:3) | IGNORED[3:0])
  PORTB |= _BV(PB2); // set ~CS to 1 
}

int main() {
  setup();
  set_sleep_mode(SLEEP_MODE_IDLE);
  while(1) {
    sleep_mode();
  }
}


