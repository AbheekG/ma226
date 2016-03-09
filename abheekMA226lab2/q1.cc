#include<iostream>
#include<iomanip>
#include"lab2.h"

using namespace std;

long generate(long a, long m, long x, long n, int range) {
  double u;
  unsigned long Density[range];
  for(int i=0;i<range;++i) {
    Density[i] = 0;
  }
  cout<<"Initial values, a = "<<a<<", m = "<<m<<", x_0 = "<<x<<", n = "<<n<<"\n";
  long q=m/a, r=m%a, k;
  for(int j=0;j<n;++j) {
    u = (double)x/(double)m;
    x = base_generator(a,x,q,r,m);
    Density[(int)(u*range)]++ ;
    cout<<"x"<<j<<"= "<<x<<"\tu"<<j<<"= "<<setprecision(10)<<u<<"\n";
  }

cout<<"$$a = "<<a<<", m = "<<m<<", x_0 = "<<x<<", n = "<<n<<"$$\n";
  cout<<"\n\\begin{center} \\begin{tabular}{||c | c||}  \\hline\n";
  cout<<"Range & Frequency \\\\ [0.5ex] \\hline \\hline";
  u = (long double)1/(long double)range;
  for(int i=0;i<range;++i) {
    cout<<setprecision(2)<<(u*i)<<"-"<<(u*(i+1))<<" & "<<Density[i]<<"\\\\\n \\hline \n";
  }
  cout<<"\\end{tabular} \n \\end{center}\n";
  return 0;
}

int plotdata(long a, long m, long x, long n) {
      double u,v;
      long q=m/a, r=m%a, k;
      cout<<"Initial value, $x_0$ = "<<x<<"\n";
        u = (long double)x/(long double)m;
      for(int j=0;j<n;++j) {
        x = base_generator(a,x,q,r,m);
        v = u;
        u = (long double)x/(long double)m;
        cout<<"u"<<j<<"= "<<fixed<<setprecision(10)<<v<<"\tu"<<j+1<<"= "<<u<<"\n";
      }
      cout<<endl;
      return 0;
    }

int main() {
  long x=1,a[] = {16807,40692,40014},m[] = {2147483647,2147483399,2147483563},n=1000;

  for(int i=0;i<3;++i) {
    for(n=1000;n<=100000; n=n*10)
      generate(a[i],m[i],x,n,20);
  }

  plotdata(a[0],m[0],x,100000);
}
