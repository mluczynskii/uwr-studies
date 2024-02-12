// Mateusz Łuczyński 331826 L10.Z1
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <inttypes.h>
#include "aux/lcd.h"

#define BAUD 9600                        
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1) 

void uart_init() {
  UBRR0 = UBRR_VALUE;
  UCSR0A = 0;
  UCSR0B = _BV(RXEN0) | _BV(TXEN0);
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

int uart_receive(FILE *stream) {
  while (!(UCSR0A & _BV(RXC0)));
  return UDR0;
}

int hd44780_transmit(char data, FILE *stream) {
  LCD_WriteData(data);
  return 0;
}

FILE hd44780_file, uart_file;

#define SIZE 16
char buf[SIZE] = {' '};
uint8_t row = 0, col = 0;

void clear_buf() {
  for (uint8_t idx = 0; idx < SIZE; idx++) 
    buf[idx] = ' ';
}

void scroll() {
  LCD_Clear();
  LCD_GoTo(0, 0);
  printf("%s", buf);
  clear_buf(buf, 16);
}

int main()
{
  uart_init();
  fdev_setup_stream(&uart_file, NULL, uart_receive, _FDEV_SETUP_READ);
  stdin = &uart_file;

  LCD_Initialize();
  LCD_Clear();
  fdev_setup_stream(&hd44780_file, hd44780_transmit, NULL, _FDEV_SETUP_WRITE);
  stdout = stderr = &hd44780_file;

  char input;

  LCD_WriteCommand(HD44780_DISPLAY_ONOFF | HD44780_DISPLAY_ON |
                   HD44780_CURSOR_ON | HD44780_CURSOR_BLINK);

  while(1) {
    LCD_GoTo(col, row);
    scanf("%c", &input);
    if (input == 0x0D) {
      if (row == 1) scroll();
      else row = 1;
      col = 0;
    } else {
      printf("%c", input);
      if (row == 1) buf[col] = input;
      col++;
      if (col > 15) {
        col = 0;
        row++;
      }
      if (row > 1) {
        scroll();
        col = 0; row = 1;
      }
    }
  }
}

