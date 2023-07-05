/*
 * row-major vs. tiled texture queries
 *
 * Intel® Core™ i5-6600 CPU @ 3.30GHz
 *
 * $ ./texture -S 0xdeadc0de -t 65536 -v 0
 * Time elapsed: 1.707234 seconds.
 * $ ./texture -S 0xdeadc0de -t 65536 -v 1
 * Time elapsed: 1.031514 seconds.
 * $ ./texture -S 0xdeadc0de -t 65536 -v 2
 * Time elapsed: 0.935953 seconds.
 */
#include "texture.h"

static inline long index_0(long x, long y) {
  return y * N + x;
}

#define VARIANT 0
#include "texture_impl.h"

// interleaves 0's between each of the lower 32bits of n
long split(long n) {
  n &= 0xFFFFFFFF;
  n = (n | n << 16) & 0x0000FFFF0000FFFF;
  n = (n | n << 8) & 0x00FF00FF00FF00FF;
  n = (n | n << 4) & 0x0F0F0F0F0F0F0F0F;
  n = (n | n << 2) & 0x3333333333333333;
  n = (n | n << 1) & 0x5555555555555555;
  return n;
}

// morton's code
static inline long index_1(long x, long y) {
  return split(y) << 1 | split(x);
}

#define VARIANT 1
#include "texture_impl.h"
