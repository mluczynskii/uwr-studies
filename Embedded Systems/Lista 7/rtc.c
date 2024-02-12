// Mateusz Łuczyński 331826 L7.Z3
#define __AVR_ATmega328__

#include <avr/io.h>
#include "aux/i2c.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <util/delay.h>

#define BAUD 9600                        
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)  

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
  UBRR0 = UBRR_VALUE;
  UCSR0A = 0;
  UCSR0B = _BV(RXEN0) | _BV(TXEN0);
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01); 

  i2cInit();
}

FILE uart_file;
const uint8_t addr = 0xD0;

uint8_t dec2bcd(uint8_t num) { // 2 digit decimal -> BCD
  uint8_t ones = num % 10, tens = num / 10;
  return ones | (tens << 4); 
}

uint8_t bcd2dec(uint8_t num) { // 2 digit BCD -> decimal
  uint8_t ones = num & 0xF, tens = (num >> 4);
  return ones + 10*tens;
}

// DD-MM-YYYY format
void read_date() {
  i2cStart();
    i2cSend(addr | 0b0);
    i2cSend(0x04); // DAY register address
  i2cStart();
    i2cSend(addr | 0b1);
    uint8_t day = i2cReadAck();
    uint8_t month = i2cReadAck();
    uint8_t year = i2cReadNoAck();
  i2cStop();
  day = bcd2dec(day);
  month = bcd2dec(month & 0x1F);
  year = bcd2dec(year);
  printf("Current date: %.2d-%.2d-%.2d\r\n", day, month, year);
}

// HH:MM:SS format
void read_time() {
  i2cStart();
    i2cSend(addr | 0b0);
    i2cSend(0x00); // SECONDS register address
  i2cStart();
    i2cSend(addr | 0b1);
    uint8_t seconds = i2cReadAck();
    uint8_t minutes = i2cReadAck();
    uint8_t hours = i2cReadNoAck();
  i2cStop();
  seconds = bcd2dec(seconds);
  minutes = bcd2dec(minutes);
  uint8_t format = hours & 0x40;
  if (format) { // 12h format
    uint8_t pm = (hours & 0x20) >> 6;
    hours = bcd2dec(hours & 0x1F);
    if ((pm ^ 0b1) & hours == 12) // 12a.m. -> 00
      hours = 0;
    else if (pm & hours != 12) // 1p.m - 11p.m. -> 13 - 23 
      hours = hours + 12;
    // 12p.m -> 12 and 1a.m. - 11a.m -> 1 - 11
  } else { // 24h format
    hours = bcd2dec(hours & 0x3F);
  }
  printf("Current time: %.2d:%.2d:%.2d\r\n", hours, minutes, seconds);
}

void set_date(uint8_t day, uint8_t month, uint8_t year) {
  if ((day > 31) | (month > 12) | (year > 99)) {
    printf("Incorrect date\r\n");
    return;
  }
  i2cStart();
    i2cSend(addr | 0b0);
    i2cSend(0x04);
    i2cSend(dec2bcd(day));
    i2cSend(dec2bcd(month));
    i2cSend(dec2bcd(year));
  i2cStop();
  printf("Date has been set to: %.2d-%.2d-%.2d\r\n", day, month, year);
}

void set_time(uint8_t hours, uint8_t minutes, uint8_t seconds) {
  if ((hours > 23) | (minutes > 59) | (seconds > 59)) {
    printf("Incorrect time\r\n");
    return;
  }
  i2cStart();
    i2cSend(addr | 0b0);
    i2cSend(0x00);
    i2cSend(dec2bcd(seconds));
    i2cSend(dec2bcd(minutes));
    i2cSend(dec2bcd(hours));
  i2cStop();
  printf("Time has been set to %.2d:%.2d:%.2d\r\n", hours, minutes, seconds);
}

// https://stackoverflow.com/questions/26620388/c-substrings-c-string-slicing
void slice(const char* str, char* result, size_t start, size_t end) {
  strncpy(result, str + start, end - start);
}

int main() {
  setup();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  while(1) {
    char instr[8] = {'\0'};
    scanf("%s", &instr); printf("%s ", instr);
    if (strcmp("date", instr) == 0) {
      printf("\r\n");
      read_date();
    } else if (strcmp("time", instr) == 0) {
      printf("\r\n");
      read_time();
    } else if (strcmp("set", instr) == 0) {
      char type[8] = {'\0'};
      scanf("%s", &type); printf("%s ", type);
      if (strcmp("date", type) == 0) {
        char args[12] = {'\0'};
        scanf("%s", &args); printf("%s\r\n", args);
        char day_buf[3] = {'\0'}, month_buf[3] = {'\0'}, year_buf[5] = {'\0'};
        slice(args, day_buf, 0, 2); slice(args, month_buf, 3, 5); slice(args, year_buf, 6, 10);
        uint8_t day = strtol(day_buf, NULL, 10), month = strtol(month_buf, NULL, 10); 
        uint16_t year = strtol(year_buf, NULL, 10);
        set_date(day, month, year%100);
      } else if (strcmp("time", type) == 0) {
        char args[12] = {'\0'};
        scanf("%s", &args); printf("%s\r\n", args);
        char hours_buf[3] = {'\0'}, minutes_buf[3] = {'\0'}, seconds_buf[3] = {'\0'};
        slice(args, hours_buf, 0, 2); slice(args, minutes_buf, 3, 5); slice(args, seconds_buf, 6, 8);
        uint8_t hours = strtol(hours_buf, NULL, 10), minutes = strtol(minutes_buf, NULL, 10); 
        uint16_t seconds = strtol(seconds_buf, NULL, 10);
        set_time(hours, minutes, seconds);
      }
    } else {
      printf("\r\nUnknown command...\r\n");
    }
  }

}