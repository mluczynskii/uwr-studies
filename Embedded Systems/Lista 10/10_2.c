// Mateusz Łuczyński 331826 L10.Z2
#define __AVR_ATmega328__

#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <inttypes.h>
#include "aux/lcd.h"

#define DELAY 100

int hd44780_transmit(char data, FILE *stream) {
  LCD_WriteData(data);
  return 0;
}

FILE hd44780_file;

int main()
{
  LCD_Initialize();
  LCD_Clear();
  fdev_setup_stream(&hd44780_file, hd44780_transmit, NULL, _FDEV_SETUP_WRITE);
  stdout = stderr = &hd44780_file;

  for (uint8_t ddram_addr = 0; ddram_addr < 6; ddram_addr++) {
    for (uint8_t cgram_addr = 0; cgram_addr < 7; cgram_addr++) {
      LCD_WriteCommand(HD44780_CGRAM_SET | (ddram_addr << 3) | cgram_addr);
      LCD_WriteData(((0b1 << ddram_addr) - 1) << (5 - ddram_addr)); 
    }
  }

  while(1) {
    LCD_Clear();
    LCD_GoTo(0, 0);
    printf("Progress: 0%%");
    _delay_ms(DELAY);

    for (uint8_t curr = 0; curr < 16; curr++) {
      for (uint8_t idx = 0; idx < 5; idx++) {
        LCD_GoTo(curr, 1);
        LCD_WriteData(idx+1);
        LCD_GoTo(0, 0);
        printf("Progress: %d%%", (5 * curr + idx + 1) * 100 / 80);
        _delay_ms(DELAY);
      }
    }
    _delay_ms(1000);
  }
}

