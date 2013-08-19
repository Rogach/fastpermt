#include <math.h>
#include <stdio.h>
#include <stdlib.h>

float* fast_ttest(float** xs, float** ys, int n, int nsubj) {
  float* result = (float*) malloc(n*sizeof(float));
  for (int q = 0; q < n; q++) {
    float sum = 0;
    float sum2 = 0;
    for (int s = 0; s < nsubj; s++) {
      float v = xs[s][q] - ys[s][q];
      sum += v;
      sum2 += v*v;
    }
    result[q] = sum / sqrt((sum2*nsubj - sum*sum)/(nsubj-1));
  }
  return result;
}
