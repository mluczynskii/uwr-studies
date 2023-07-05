/*
 * Binary search with linearly placed tree levels.
 *
 * Intel® Core™ i5-6600 CPU @ 3.30GHz
 *
 * $ ./binsearch -S 0x5bab3de5da7882ff -n 23 -t 24 -v 0
 * Time elapsed: 7.616777 seconds.
 * $ ./binsearch -S 0x5bab3de5da7882ff -n 23 -t 24 -v 1
 * Time elapsed: 2.884369 seconds.
 */
#include "binsearch.h"

bool binsearch0(T *arr, long size, T x) {
  do {
    size >>= 1;
    T y = arr[size];
    if (y == x)
      return true;
    if (y < x)
      arr += size + 1;
  } while (size > 0);
  return false;
}

void linearize(T *dst, T *src, long size) {
  long step = size+1;
  long start = (size >> 1);
  long i = 0;
  while (i < size) {
    long index = start;
    while (index < size) {
      T elem = src[index];
      dst[i] = elem;
      i = i + 1;
      index = index + step;
    }
    step >>= 1;
    start >>= 1;
  }
}

bool binsearch1(T *arr, long size, T x) {
  long index = 1;
  while (index <= size) {
    T y = arr[index-1];
    if (y == x)
      return true;
    index <<= 1;
    index += (y < x);
  }
  return false;
}