// Mateusz Łuczyński 331826 L8.Z1
#define __AVR_ATmega328__

#include "FreeRTOS.h"
#include "task.h"

#include <avr/io.h>

#define mainLED_BAR_TASK_PRIORITY   2
#define mainLED_REMEMBER_TASK_PRIORITY 1

// LED bar
#define BAR_PORT PORTD 
#define BAR_DDR DDRD

// Blinking LED
#define LED_PORT PORTC
#define LED_DDR DDRC
#define LED PC5

// Blinking LED Pushbutton
#define BTN_PIN PINC
#define BTN_PORT PORTC
#define BTN PC4

static void vBarLED(void* pvParameters);
static void vRememberLED(void* pvParameters);

int main(void) {
    xTaskHandle bar_handle;
    xTaskHandle remember_handle;

    xTaskCreate
        (
         vBarLED,
         "ledbar",
         configMINIMAL_STACK_SIZE,
         NULL,
         mainLED_BAR_TASK_PRIORITY,
         &bar_handle
        );
    
    xTaskCreate
        (
         vRememberLED,
         "remberled",
         configMINIMAL_STACK_SIZE, 
         NULL,
         mainLED_REMEMBER_TASK_PRIORITY,
         &remember_handle
        );

    vTaskStartScheduler();
    return 0;
}

void vApplicationIdleHook(void) {

}

void slide(uint8_t init, uint8_t direction) {
  for (int i = 0; i < 8; i++) {
    BAR_PORT &= 0b0;
    BAR_PORT |= init;
    init = direction ? init << 1 : init >> 1;
    vTaskDelay(500 / portTICK_PERIOD_MS); // 0.5s
  }
}

static void vBarLED(void* pvParameters) {
    BAR_DDR |= ~0b0;
    while(1) {  
      slide(0b1, 0b1); // slide 0->7
      slide(0b10000000, 0b0); // slide 7->0
    }
}

#define SIZE 1024
uint8_t state[SIZE] = {0};

static void vRememberLED(void* pvParameters) {
    BTN_PORT |= _BV(BTN); // enable pull-up
    LED_DDR |= _BV(LED);
    LED_PORT |= _BV(LED);

    uint16_t curr = 0;

    while(1) {
        if (state[curr]) LED_PORT |= _BV(LED);
        else LED_PORT &= ~_BV(LED);

        if (BTN_PIN & _BV(BTN)) state[(curr+100) % SIZE] = 0;
        else state[(curr+100) % SIZE] = 1;

        state[curr] = 0;
        curr = (curr + 1) % SIZE;
        vTaskDelay(10 / portTICK_PERIOD_MS); // 10ms
    }
}
