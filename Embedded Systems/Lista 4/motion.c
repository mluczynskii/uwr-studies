// Mateusz Łuczyński 331826 L4.Z4
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>

#define LED PD2
#define LED_PORT PORTD 
#define LED_DDR DDRD

void setup() {
  // CTC, top -> ICR1, toggle on compare match, prescaler -> 8  
  TCCR1A |= _BV(COM1B0);
  TCCR1B |= _BV(WGM13) | _BV(WGM12) | _BV(CS11);

  // F_OUT = F_CPU / 2 * PRESCALER * (1 + TOP)
  // -> ICR1(TOP) = 25 => F_OUT ~ 38,46kHz
  ICR1 = 25;

  LED_DDR |= _BV(LED);
}

// IR receiver connected to -> PC5
void probe() {
  if (PINC & _BV(PC5)) // signal ON -> receiver doesn't see the IR signal 
    LED_PORT &= ~_BV(LED);
  else // signal OFF -> receiver sees the IR signal 
    LED_PORT |= _BV(LED);
}

int main() {
  setup();
  uint8_t cnt = 0;
  while(1) {
    DDRB |= _BV(PB2);
    _delay_us(600);
    probe(); // at this point the IR diode has been sending signal with 38.4kHz frequency for 600us
             // so probe() should light up the LED only if the receiver sees the diode signal
             // (that is, when you reflect it with something, ex. a hand)
    DDRB &= ~_BV(PB2);
    _delay_us(600);
    cnt = (cnt + 1) % 11;
    if (cnt == 0) _delay_ms(93.4); // waiting period betwen cycles of 6 ON and 5 OFF segments
                                   // that way the whole cycle lasts 100ms, as described in the datasheet
  }
}


