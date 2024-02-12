// Mateusz Łuczyński 331826 L7.Z1
#define __AVR_ATmega328__

#include <avr/io.h>
#include "aux/i2c.h"
#include <stdio.h>
#include <util/delay.h>
#include <string.h>

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
char buf[8] = {'\0'};

uint8_t read(uint8_t device_select, uint8_t addr) {
  printf("\r\n");
  i2cStart();
    i2cSend(device_select | 0b0); // ID + BLOCK SELECT + WRITE
    i2cSend(addr); // send ADDRESS 
  i2cStart(); // reSTART
    i2cSend(device_select | 0b1); // ID + BLOCK SELECT + READ
    uint8_t data = i2cReadNoAck();
  i2cStop();
  return data;
}

void write(uint8_t device_select, uint8_t addr, uint8_t data) {
  i2cStart();
    i2cSend(device_select | 0b0); // ID + BLOCK SELECT + WRITE
    i2cSend(addr); // send ADDRESS
    i2cSend(data); // send DATA
  i2cStop();
}

int main() {
  setup();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  const uint8_t eeprom_addr = 0xA0; //0b1010[A2=0][A1=0][BLOCK SELECT][R/W]

  while(1) {
    scanf("%s", &buf); printf("%s ", buf);
    if (strcmp("read", buf) == 0) {
      uint16_t addr; // 512B memory -> 256B per block
      uint8_t data;
      scanf("%x", &addr); printf("0x%.3X ", addr);
      uint8_t device_select = eeprom_addr | ((addr & 0x100) >> 7);
      addr &= 0x1FF; // 9-bit addresses
      data = read(device_select, addr & 0xFF);
      printf("Read 0x%.2X from 0x%.3X\r\n", data, addr);
    } else if (strcmp("write", buf) == 0) {
      uint16_t addr; 
      uint8_t data;
      scanf("%x", &addr); printf("0x%.3X ", addr);
      uint8_t device_select = eeprom_addr | ((addr & 0x100) >> 7);
      addr &= 0x1FF;
      scanf("%x", &data); printf("%.2X\r\n", data);
      write(device_select, addr & 0xFF, data);
      printf("Wrote 0x%.2X to 0x%.3X\r\n", data, addr);
    } else {
      printf("\r\nUnknown command\r\n");
    }
  }
}