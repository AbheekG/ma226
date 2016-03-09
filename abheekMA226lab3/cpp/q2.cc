#include <iostream>
#include <iomanip>
#include <cmath>
#include <cstdlib>

#define RANGE 90

using namespace std;

class LCG {
private:
	long a, b, m, x, q, r;
public:
	LCG() {
		a = 1103515245;
		b = 12345;
		m = 2147483648;
		q=m/a;
		r=m%a;
		x = std::rand()/32768;
	}
	long base_generator() {
		long k=x/q;
    	x = (a * (x - (k * q))) - (k * r);
    	x = (x + b) % m;
    	while(x<0) x+=m;
    	return x;
	}
	double generate() {
		return (double)base_generator()/m;
	}
	void set_all(long sa, long sb, long sm, long sx) {
		a = sa;
		b = sb;
		m = sm;
		q=m/a;
		r=m%a;
		x = sx;
	}
	void set_x(long sx) {
		x = sx;
	}
};

double inv_exp_cdf(double x, double lamda) {
	return (-log(x)/lamda);
}

int main() {
	int n = 5000, Density[RANGE];
	double lamda = 5;
	double mean=5, emax = 0, emin = 1.79769e+30, x;
	LCG lcg;

	for(int i = 0; i < RANGE; ++i) {
		Density[i] = 0;
	}

	for(int i = 0; i < n; ++i) {
		x = 0;
		for(int j=0; j<5; ++j) {
			x += inv_exp_cdf(lcg.generate(), lamda);
		} 
	//	cout<<x<<" ";
		mean = ((mean * i) + x)/(i+1);
		if(emin > x)
			emin = x;
		if(emax < x)
			emax = x;
		if(x*30 <= RANGE) {
			Density[(int)(x*30)]++;
		}
	}
	cout<<"Mean = "<<mean<<endl;
	cout<<"Minimum = "<<emin<<endl;
	cout<<"Maximum = "<<emax<<endl;
	for(int i = 0; i < RANGE; ++i) {
		if(i%30==0) 
			cout<<i/30<<" "<<Density[i]<<endl;
		else
			cout<<". "<<Density[i]<<endl;
	}
	return 0;
}