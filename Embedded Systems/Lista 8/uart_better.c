#define __AVR_ATmega328__

#include "FreeRTOS.h"
#include "task.h"
#include "queue.h"
#include <avr/io.h>
#include <avr/interrupt.h>
#include <inttypes.h>
#include <stdio.h>
#include "uart.h"

#ifndef F_CPU
#define F_CPU 16000000UL
#endif

#ifndef BAUD
#define BAUD 9600
#endif

#include <util/setbaud.h>

uint8_t uart_transmit(uint8_t data, FILE *stream);
uint8_t uart_receive(FILE *stream);

#define QUEUE_SIZE 50 // :(
QueueHandle_t xQueueRX, xQueueTX;

void uart_init() {
  UBRR0H = UBRRH_VALUE;
  UBRR0L = UBRRL_VALUE;
#if USE_2X
  UCSR0A |= _BV(U2X0);
#else
  UCSR0A &= ~(_BV(U2X0));
#endif
  UCSR0C = _BV(UCSZ01) | _BV(UCSZ00);
  UCSR0B = _BV(RXEN0) | _BV(TXEN0); 

  xQueueRX = xQueueCreate(QUEUE_SIZE, sizeof(uint8_t));
  xQueueTX = xQueueCreate(QUEUE_SIZE, sizeof(uint8_t));
  UCSR0B |= _BV(RXCIE0);
  sei();
}

ISR(USART_RX_vect) {
  uint8_t data = UDR0;
  BaseType_t xStatus = xQueueSendFromISR(xQueueRX, &data, NULL);
  // If xQueueRX was full then we lose the received data (unlucky)
}

ISR(USART_UDRE_vect) {
  uint8_t data;
  BaseType_t xStatus = xQueueReceiveFromISR(xQueueTX, &data, NULL);
  if (xStatus == pdPASS) // There was something waiting for transmitting in xQueueTX
    UDR0 = data;
  else // xQueueTX is empty
    UCSR0B &= ~_BV(UDRIE0);
}

uint8_t uart_transmit(uint8_t data, FILE *stream) {
  BaseType_t xStatus = xQueueSend(xQueueTX, &data, portMAX_DELAY);
  UCSR0B |= _BV(UDRIE0);
  return 0;
}

uint8_t uart_receive(FILE *stream) {
  uint8_t data;
  BaseType_t xStatus = xQueueReceive(xQueueRX, &data, portMAX_DELAY);
  return data;
}

FILE uart_file = FDEV_SETUP_STREAM(uart_transmit, uart_receive, _FDEV_SETUP_RW); 

