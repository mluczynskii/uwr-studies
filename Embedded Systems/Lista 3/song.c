// Mateusz Łuczyński 331826 L3.Z1
#define __DELAY_BACKWARD_COMPATIBLE__
#define __AVR_ATmega328__

#include <util/delay.h>
#include <avr/io.h>
#include <avr/pgmspace.h>

#define BUZZ PD2
#define BUZZ_DDR DDRD
#define BUZZ_PORT PORTD
 
#define TEMPO 150 
// note lengths in ms (60k / TEMPO -> QUARTER in ms)
#define QUARTER 60000/TEMPO 
#define EIGHT QUARTER/2

// note frequencies in Hz
#define C4 262
#define D4 294
#define E4 330
#define F4 349
#define G4 392
#define A4 450
#define B4 494

#define C5 523
#define D5 587
#define E5 659
#define F5 698
#define G5 784
#define A5 880
#define B5 988

typedef uint32_t note;
#define NOTE(f, n) (uint32_t)(f) << 16 | (n)
#define F(note) (uint16_t)(note >> 16);
#define MS(note) (uint16_t)(note & 0xFFFF);

// https://12holeocarina.com/zelda-songs/sarias-song-lost-woods/
const note song[] PROGMEM = {
  NOTE(F4, QUARTER), NOTE(A4, QUARTER), NOTE(B4, QUARTER), NOTE(0, EIGHT/8), 
  NOTE(F4, QUARTER), NOTE(A4, QUARTER), NOTE(B4, QUARTER), NOTE(0, EIGHT/8), 
  NOTE(F4, QUARTER), NOTE(A4, QUARTER), NOTE(B4, QUARTER), NOTE(E5, QUARTER), NOTE(D5, QUARTER), NOTE(0, EIGHT/4),
  NOTE(B4, QUARTER), NOTE(C5, QUARTER), NOTE(B4, QUARTER), NOTE(G4, QUARTER), NOTE(E4, QUARTER), NOTE(0, EIGHT/2), 
  NOTE(D4, QUARTER), NOTE(E4, QUARTER), NOTE(G4, QUARTER), NOTE(E4, QUARTER), NOTE(0, EIGHT), 
  NOTE(F4, QUARTER), NOTE(A4, QUARTER), NOTE(B4, QUARTER), NOTE(0, EIGHT/8),
  NOTE(F4, QUARTER), NOTE(A4, QUARTER), NOTE(B4, QUARTER), NOTE(0, EIGHT/8), 
  NOTE(F4, QUARTER), NOTE(A4, QUARTER), NOTE(B4, QUARTER), NOTE(E5, QUARTER), NOTE(D5, QUARTER), NOTE(0, EIGHT/4), 
  NOTE(B4, QUARTER), NOTE(C5, QUARTER), NOTE(E5, QUARTER), NOTE(B4, QUARTER), NOTE(G4, QUARTER), NOTE(0, EIGHT/2),
  NOTE(B4, QUARTER), NOTE(G4, QUARTER), NOTE(D4, QUARTER), NOTE(E4, QUARTER), NOTE(0, EIGHT),

  NOTE(D4, QUARTER), NOTE(E4, QUARTER), NOTE(F4, QUARTER), NOTE(0, EIGHT/8),
  NOTE(G4, QUARTER), NOTE(A4, QUARTER), NOTE(B4, QUARTER), NOTE(0, EIGHT/8),
  NOTE(D5, QUARTER), NOTE(B4, QUARTER), NOTE(E4, QUARTER), NOTE(0, EIGHT),

  NOTE(D4, QUARTER), NOTE(E4, QUARTER), NOTE(F4, QUARTER), NOTE(0, EIGHT/8), 
  NOTE(G4, QUARTER), NOTE(A4, QUARTER), NOTE(B4, QUARTER), NOTE(0, EIGHT/8), 
  NOTE(C5, QUARTER), NOTE(D5, QUARTER), NOTE(E5, QUARTER), NOTE(0, EIGHT),

  NOTE(D4, QUARTER), NOTE(E4, QUARTER), NOTE(F4, QUARTER), NOTE(0, EIGHT/8),
  NOTE(G4, QUARTER), NOTE(A4, QUARTER), NOTE(B4, QUARTER), NOTE(0, EIGHT/8),
  NOTE(D5, QUARTER), NOTE(B4, QUARTER), NOTE(E4, QUARTER), NOTE(0, EIGHT),

  NOTE(D4, QUARTER), NOTE(C4, QUARTER), NOTE(0, EIGHT/16),
  NOTE(F4, QUARTER), NOTE(E4, QUARTER), NOTE(0, EIGHT/16), 
  NOTE(G4, QUARTER), NOTE(F4, QUARTER), NOTE(0, EIGHT/16), 
  NOTE(A4, QUARTER), NOTE(G4, QUARTER), NOTE(0, EIGHT/16), 
  NOTE(B4, QUARTER), NOTE(A4, QUARTER), NOTE(0, EIGHT/16),
  NOTE(C5, QUARTER), NOTE(B4, QUARTER), NOTE(0, EIGHT/16),
  NOTE(D5, QUARTER), NOTE(C5, QUARTER), NOTE(0, EIGHT/4),
  NOTE(E5, QUARTER), NOTE(F5, QUARTER), NOTE(D5, QUARTER), NOTE(E5, QUARTER)
};

void playNote(uint16_t frequency, uint16_t duration) {
  float period = 1000000.0 / frequency;
  float acc = 0;
  while(acc <= (float)duration * 1000.0) {
    BUZZ_PORT |= _BV(BUZZ);
    _delay_us(period/2.0);
    BUZZ_PORT &= ~_BV(BUZZ);
    _delay_us(period/2.0);
    acc = acc + period;
  }
}

void setup() {
  BUZZ_DDR |= _BV(BUZZ);
}

int main() {
  setup();
  int length = sizeof(song)/sizeof(note);
  while(1) {
    for(int i = 0; i < length; i++) {
      note curr = pgm_read_dword(&song[i]);
      uint16_t freq = F(curr); uint16_t dur = MS(curr);
      if (freq != 0) {
        playNote(freq, dur);
      }
      else _delay_ms(dur);
    }
    _delay_ms(1000);
  }
}


