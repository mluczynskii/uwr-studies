/*
 * Matrix multiplication with and without blocking.
 *
 * Intel® Core™ i5-6600 CPU @ 3.30GHz
 *
 * $ ./matmult -n 1024 -v 0
 * Time elapsed: 3.052755 seconds.
 * $ ./matmult -n 1024 -v 1
 * Time elapsed: 0.746337 seconds.
 * $ ./matmult -n 1024 -v 2
 * Time elapsed: 9.882309 seconds.
 * $ ./matmult -n 1024 -v 3
 * Time elapsed: 0.698795 seconds.
 */
#include "matmult.h"

/* Useful macro for accessing row-major 2D arrays of size n×n. */
#define M(a, i, j) a[(i) * n + (j)]

/* ijk (& jik) */
void matmult0(int n, T_p a, T_p b, T_p c) {
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      long sum = 0;
      for (int k = 0; k < n; k++) 
        sum += M(a, i, k) * M(b, k, j);
      M(c, i, j) = sum;
    }
  }
}

/* kij (& ikj) */
void matmult1(int n, T_p a, T_p b, T_p c) {
  for (int k = 0; k < n; k++) {
    for (int i = 0; i < n; i++) {
      long r = M(a, i, k);
      for (int j = 0; j < n; j++) 
        M(c, i, j) += r * M(b, k, j);
    }
  }
}

/* jki (& kji) */
void matmult2(int n, T_p a, T_p b, T_p c) {
  for (int j = 0; j < n; j++) {
    for (int k = 0; k < n; k++) {
      long r = M(b, k, j);
      for (int i = 0; i < n; i++) 
        M(c, i, j) += M(a, i, k) * r;
    }
  }
}

/* BLOCK*BLOCK tiled version */
void matmult3(int n, T_p a, T_p b, T_p c) {
  for (int i = 0; i < n; i+=BLOCK) 
    for (int j = 0; j < n; j+=BLOCK)
      for (int k = 0; k < n; k+=BLOCK)
        for (int i1 = i; i1 < i + BLOCK; i1++)
          for (int j1 = j; j1 < j + BLOCK; j1++)
            for (int k1 = k; k1 < k + BLOCK; k1++)
              M(c, i1, j1) += M(a, i1, k1) * M(b, k1, j1);
}
