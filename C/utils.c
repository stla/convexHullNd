#include <stdlib.h> // to use realloc
#include <math.h>   // to use NAN
#include <stdio.h>  // to use printf

double* getpoint(double* points, unsigned dim, unsigned id) {
  double* out = malloc(dim * sizeof(double));
  for(unsigned i = 0; i < dim; i++) {
    out[i] = points[id*dim+i];
  }
  return out;
}

/* dot product of two vectors */
double dotproduct(double* p1, double* p2, unsigned dim) {
  double out = 0;
  for(unsigned i = 0; i < dim; i++) {
    out += p1[i] * p2[i];
  }
  return out;
}

/* middle of segment [p1,p2] */
double* middle(double* p1, double* p2, unsigned dim) {
  double* out = malloc(dim * sizeof(double));
  for(unsigned i = 0; i < dim; i++) {
    out[i] = (p1[i] + p2[i]) / 2.0;
  }
  return out;
}

/* vector of NANs */
double* nanvector(int dim) {
  double* out = malloc(dim * sizeof(double));
  for(unsigned i = 0; i < dim; i++) {
    out[i] = NAN;
  }
  return out;
}

// to use the qsort function
int cmpfunc(const void *a, const void *b) {
   return *(int*)a - *(int*)b;
}
int cmpfuncdbl(const void *a, const void *b) {
   return *(double*)a - *(double*)b > 0 ? 1 : -1;
}
void qsortu(unsigned* vector, unsigned length) {
  qsort(vector, length, sizeof(unsigned), cmpfunc);
}


double square(double x) {
  return x * x;
}

/* append to a vector of unsigned */
void appendu(unsigned x, unsigned** array, unsigned length, unsigned* flag) {
  *flag = 1;
  for(unsigned i = 0; i<length; i++) {
    if(x == *(*array + i)){
      *flag = 0;
      break;
    }
  }
  if(*flag == 1){
    *array = realloc(*array, (length+1) * sizeof(unsigned));
    if(*array == NULL) {
      printf("%s", "realloc failure - exiting");
      exit(1);
    }
    *(*array + length) = x;
  }
}

/* make a vector of zeros */
unsigned* uzeros(unsigned length) {
  unsigned* out = malloc(length * sizeof(unsigned));
  for(unsigned i = 0; i < length; i++) {
    out[i] = 0;
  }
  return out;
}

/* squared distance between two points */
double squaredDistance(double* p1, double* p2, unsigned dim) {
  double out = 0;
  for(unsigned i = 0; i < dim; i++) {
    out += square(p1[i] - p2[i]);
  }
  return out;
}
