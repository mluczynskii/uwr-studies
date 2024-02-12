// Mateusz Łuczyński 331826 L6.Z1
#define __AVR_ATmega328__

#include <avr/sleep.h>
#include <avr/interrupt.h>
#include <util/delay.h> // F_CPU

#define BAUD 9600
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)

void setup() {
  UBRR0 = UBRR_VALUE;
  UCSR0B |= _BV(RXEN0) | _BV(TXEN0); // enable transmitter and receiver
  UCSR0C |= _BV(UCSZ00) | _BV(UCSZ01); // 1-byte character size
  UCSR0B |= _BV(RXCIE0); // receive complete interrupt enable 
  
  sei(); // enable interrupts
}

volatile uint8_t buf = 0;

ISR(USART_RX_vect) { // receive complete interrupt routine 
  buf = UDR0; // read received character 
  UCSR0B |= _BV(UDRIE0); // enable data register empty interrupt so the character gets sent when the 
                         // transmitter is ready
}

ISR(USART_UDRE_vect) { 
  UCSR0B &= ~_BV(UDRIE0); // disable data register empty interrupt until a new character
                          // gets received 
  UDR0 = buf; // transmit the character
}

int main() {
  setup();
  set_sleep_mode(SLEEP_MODE_IDLE);
  while(1) {
    sleep_mode();
  }
}
