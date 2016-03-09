#include <iostream>
#include <iomanip>

using namespace std;

long base_generator(long a, long x, long q, long r, long m) {
	long k=x/q;
    x = (a * (x - (k * q))) - (k * r);
    while(x<0) x+=m;
    return x;
}


