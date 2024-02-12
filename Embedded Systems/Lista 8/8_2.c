// Mateusz Łuczyński 331826 L8.Z2
#define __AVR_ATmega328__

#include "FreeRTOS.h"
#include "task.h"
#include "queue.h"

#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include "uart.h"

#define mainLED_TASK_PRIORITY 2
#define mainUART_TASK_PRIORITY 1

// Blinking LED
#define LED_PORT PORTC
#define LED_DDR DDRC
#define LED PC5

static void vLED(void* pvParameters);
static void vUART(void* pvParameters);

#define QUEUE_SIZE 10
QueueHandle_t xQueue;

int main(void) {
    xTaskHandle led_handle;
    xTaskHandle uart_handle;

    xQueue = xQueueCreate(QUEUE_SIZE, sizeof(uint16_t));

    xTaskCreate
        (
         vLED,
         "led",
         configMINIMAL_STACK_SIZE,
         NULL,
         mainLED_TASK_PRIORITY,
         &led_handle
        );
    
    xTaskCreate
        (
         vUART,
         "uart",
         500, 
         NULL,
         mainUART_TASK_PRIORITY,
         &uart_handle
        );

    vTaskStartScheduler();
    return 0;
}

void vApplicationIdleHook(void) {

}

static void vLED(void* pvParameters) {
    LED_DDR |= _BV(LED);
    uint16_t delay;
    while(1) {  
        BaseType_t xStatus = xQueueReceive(xQueue, &delay, portMAX_DELAY);
        if (xStatus == pdPASS) {
          LED_PORT |= _BV(LED);
          vTaskDelay(delay / portTICK_PERIOD_MS);
          LED_PORT &= ~_BV(LED);
          vTaskDelay(500 / portTICK_PERIOD_MS); // 0.5s pause inbetween
        }
    }
}

static void vUART(void* pvParameters) {
    uart_init();
    stdin = stdout = stderr = &uart_file;
    uint16_t delay;
    while(1) {
        printf("Insert next delay value: ");
        scanf("%"PRIu16"", &delay); printf("%"PRIu16" \r\n", delay);
        BaseType_t xStatus = xQueueSend(xQueue, &delay, portMAX_DELAY);
        if (xStatus != pdPASS) { // Shouldn't happen cause of portMAX_DELAY wait time
          printf("Can't send data to the queue... \r\n");
        } else {
          printf("Added %"PRIu16" to the queue \r\n", delay);
        }
    }
}
