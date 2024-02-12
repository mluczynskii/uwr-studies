// Mateusz Łuczyński 331826 L7.Z1
#define __AVR_ATmega328__

#include <avr/io.h>
#include "aux/i2c.h"
#include <stdio.h>
#include <util/delay.h> // F_CPU
#include <string.h> // strcmp(), strncpy()
#include <stdlib.h> // strtol()

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
const uint8_t eeprom_addr = 0xA0; //0b1010[A2=0][A1=0][BLOCK SELECT][R/W]

// prints out read_buf in I8HEX format
I8HEX_pp_print(uint16_t addr, uint8_t length, uint8_t* read_buf) { 
  printf(":%.2X%.4X00", length, addr); // :[length][addr][00]
  uint8_t checksum = 0;
  for (int i = 0; i < length; i++) {
    uint8_t data = read_buf[i];
    checksum += data;
    printf("%.2X", data);
  }
  checksum = ~checksum + 1; // 2's complement
  printf("%.2X\r\n", checksum);
}

// https://stackoverflow.com/questions/26620388/c-substrings-c-string-slicing
void slice(const char* str, char* result, size_t start, size_t end) {
  strncpy(result, str + start, end - start);
}

void write_seq() {
  char args[100] = {'\0'};
  scanf("%s", &args); printf("%s\r\n", args);
  char length_buf[3] = {'\0'}, addr_buf[5] = {'\0'}, type_buf[3] = {'\0'};
  slice(args, length_buf, 1, 3); slice(args, addr_buf, 3, 7); slice(args, type_buf, 7, 9);

  if (strcmp("01", type_buf) == 0) // EOF record type
    return;
  else {
    uint8_t length = strtol(length_buf, NULL, 16);
    uint16_t addr = strtol(addr_buf, NULL, 16); addr &= 0x1FF;
    uint8_t device_select = eeprom_addr | ((addr & 0x100) >> 7);
    i2cStart();
      i2cSend(device_select | 0b0);
      i2cSend(addr & 0xFF); 
      for (int i = 0; i < length; i++) {
        if (addr + i > 0x1FF) {
          printf("Reached end of memory\r\n");
          break;
        } else if ((addr <= 0xFF) & (addr + i == 0x100)) {
          i2cStop();
          printf("Reached end of block\r\n");
          i2cStart();
          i2cSend(eeprom_addr | 0b10 | 0b0);
          i2cSend(0x00);
        }
        char data_buf[3] = {'\0'};
        slice(args, data_buf, 9+2*i, 11+2*i);
        uint8_t data = strtol(data_buf, NULL, 16);
        i2cSend(data);
        printf("Wrote 0x%.2X to 0x%.3X\r\n", data, (addr+i)%512);
      }
    i2cStop();
  }
  write_seq(); // loop until EOF
}

void read_seq(uint16_t addr, uint8_t length, uint8_t* read_buf) {
  uint8_t device_select = eeprom_addr | ((addr & 0x100) >> 7);
  i2cStart();
    i2cSend(device_select | 0b0);
    i2cSend(addr & 0xFF);
  i2cStart();
    i2cSend(device_select | 0b1);
    for (int i = 0; i < length; i++) {
      uint8_t data = (i == length-1 ? i2cReadNoAck() : i2cReadAck());
      read_buf[i] = data;
      printf("Read 0x%.2X from 0x%.3X\r\n", data, (addr+i)%512);
    }
  i2cStop();
}

int main() {
  setup();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  while(1) {
    char instr[8] = {'\0'};
    scanf("%s", &instr); printf("%s ", instr);
    if (strcmp("read", instr) == 0) {
      uint16_t addr;
      scanf("%x", &addr); addr &= 0x1FF; printf("0x%.3X ", addr);
      uint8_t length; 
      scanf("%d", &length); printf("%d\r\n", length);
      uint8_t read_buf[256];
      read_seq(addr, length, read_buf);
      I8HEX_pp_print(addr, length, read_buf);
    } else if (strcmp("write", instr) == 0) {
      printf("\r\n");
      write_seq();
    } else {
      printf("\r\nUnknown command...\r\n");
    }
  }
}