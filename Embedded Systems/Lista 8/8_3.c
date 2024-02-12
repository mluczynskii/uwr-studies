// Mateusz Łuczyński 331826 L8.Z2
#define __AVR_ATmega328__

#include "FreeRTOS.h"
#include "task.h"

#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include "uart.h"

#define mainLED_TASK_PRIORITY 1
#define mainUART_TASK_PRIORITY 2

// Blinking LED
#define LED_PORT PORTC
#define LED_DDR DDRC
#define LED PC5

static void vLED(void* pvParameters);
static void vUART(void* pvParameters);

int main(void) {
    xTaskHandle led_handle;
    xTaskHandle uart_handle;
    
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
         1024, 
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
    while(1) {  
        LED_PORT ^= _BV(LED);
        vTaskDelay(500 / portTICK_PERIOD_MS);
    }
}

static void vUART(void* pvParameters) {
    uart_init();
    stdin = stdout = stderr = &uart_file;
    char input;
    while(1) {
        scanf("%c", &input);
        printf("Got: %c \r\n", input); 
    }
}
