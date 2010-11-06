#include <stdlib.h>
#include <time.h>
#include <math.h>

int rand_max(void) {return RAND_MAX;}

int clocks_per_sec(void) {return CLOCKS_PER_SEC;}

double infinity(void) {return HUGE_VAL;}

double minus_infinity(void) {return -HUGE_VAL;}

#ifdef ALPHA
double c_nan(void) {return 0;}
#else
double c_nan(void) {return HUGE_VAL-HUGE_VAL;}
#endif
