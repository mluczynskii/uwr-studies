#include <avr/io.h>
#include <avr/sfr_defs.h>
#include <stdio.h>
#include <inttypes.h>
#include <util/delay.h>

#define LED PB5 //built-in diode
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BAUD 9600 //baudrate                         
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)  

#define UNIT 200 //ms
#define DOT_MS UNIT //length of a dot
#define DASH_MS 3*UNIT //length of a dash 

#define INTRA_MS UNIT //delay between dit-dahs
#define INTER_MS 3*UNIT //delay between characters 
#define WORD_MS 7*UNIT //delay between words 

typedef char* morse;

//uart config
void uart_init() {
  UBRR0 = UBRR_VALUE;
  UCSR0A = 0;
  UCSR0B = _BV(RXEN0) | _BV(TXEN0);
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

//write one char 
int uart_transmit(char data, FILE *stream) {
  while(!(UCSR0A & _BV(UDRE0)));
  UDR0 = data;
  return 0;
}

//read one char
int uart_receive(FILE *stream) {
  while (!(UCSR0A & _BV(RXC0)));
  return UDR0;
}

FILE uart_file;

const morse code[] = {
  ".-",   // A 
  "-...", // B 
  "-.-.", // C 
  "-..",  // D 
  ".",    // E 
  "..-.", // F 
  "--.",  // G 
  "....", // H 
  "..",   // I 
  ".---", // J 
  "-.-",  // K
  ".-..", // L 
  "--",   // M 
  "-.",   // N 
  "---",  // O 
  ".--.", // P 
  "--.-", // Q 
  ".-.",  // R 
  "...",  // S 
  "-",    // T 
  "..-",  // U 
  "...-", // V 
  ".--",  // W 
  "-..-", // X 
  "-.--", // Y 
  "--.."  // Z
};

void dot () {
  printf("Displaying a dot! \r\n");
  LED_PORT |= _BV(LED);
  _delay_ms(DOT_MS);
  LED_PORT &= ~_BV(LED);
}

void dash () {
  printf("Displaying a dash! \r\n");
  LED_PORT |= _BV(LED);
  _delay_ms(DASH_MS);
  LED_PORT &= ~_BV(LED);
}

//convert a character to it's morse code counterpart, or return null if such doesn't exist
morse convert(char c) {
  if (c > 122 || c < 97) return NULL;
  return code[c-97];
}

//transmit a morse code letter 
void character(morse c) {
  for (int i = 0; i < strlen(c); i++) {
    if(c[i] == '.') dot();
    else dash();
    _delay_ms(INTRA_MS);
  }
}

int main() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  LED_DDR |= _BV(LED); 
  while (1) {
    char a = 1;
    scanf("%c", &a);
    printf("read: %c. displaying now... \r\n", a); 
    character(convert(a));
    printf("displayed! \r\n");
  }
}
