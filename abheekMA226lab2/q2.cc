#include <iostream>
#include <iomanip>
#include "lab2.h"

using namespace std;

void generator(long A[], long m, int range, int n) {
  long *Density, x;
  long double u,v;
  long double auto_cor[5]= {0,0,0,0,0}, mean = 0, variance = 0, absolute_var = 0;
  Density = new long[range];

  for(int i=0; i<range; ++i) {
    Density[i] = 0;
  }

  x = A[16];

  for(int i=0;i<n;++i) {
    for(int j=0;j<16;++j) {
      A[j] = A[j+1];
    }
    A[16] = x;
    u = (long double)x/(long double)m;
    x = (A[0] + A[12]) % m;
    Density[(int)(u*range)]++ ;
    v = u;
    u = (long double)x/(long double)m;
    cout<<fixed<<setprecision(10)<<v<<" "<<u<<"\n";
    mean = ((mean * i) + u)/(i+1);
    variance = ((variance * i) + ((u - mean) * (u - mean)))/(i+1);
    absolute_var = ((absolute_var*i) + ((u - 0.5) * (u - 0.5))) / (i+1);
    for(int k=0;k<5;++k) {
      auto_cor[k] = auto_cor[k] + (long double)(((long double)x/(long double)m) - mean)* (((long double)A[16-k]/(long double)m) - mean);
    }
  }

  cout<<"\n\nProbability in each interval\n";

  u = (long double)1/(long double)range;
  for(int i=0;i<range;++i) {
    cout<<setprecision(2)<<(u*i)<<"-"<<(u*(i+1))<<" "<<setprecision(5)<<(long double)Density[i]/n<<"\n";
  }

  cout<<"\n\n";
  cout<<"Mean = "<<setprecision(15)<<mean<<"\n";
  cout<<"Variance (with changing mean) = "<<variance<<"\n";
  cout<<"Variance (with mean = 0.5) = "<<absolute_var<<"\n";
  for(int i = 0; i <5; ++i) {
    cout<<"Autocorrelation (with lag = "<<i+1<<") = "<<auto_cor[i]/(variance * n)<<"\n";
  }
  cout<<"\n\n\n";

}


int main() {
  long A[17], m = 2147483648, n = 1000;

  //We can take 1000 and 10000 values from the 100000 generated ones.
  for(int j=0; j<3; ++j, n*=10) {
    A[0] = 522329230;
    for(int i=1; i<17; ++i) {
      A[i] = base_generator(16807, A[i-1], 2147483647/16807, 2147483647%16807, 2147483647);
    } 
    generator(A, m, 20 ,n);
  }
}
