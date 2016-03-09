#include<iostream>
#include<iomanip>

using namespace std;

//Our main uniform random values genrator class
class UniformGenerator {
  public:
    UniformGenerator() {
      a=1,b=0,m=1;   //Just some initializing values.
    }
    unsigned long long int a,b,m;

    //The first value is the value of x0 and second no of terms in sequence.
    int generate(unsigned long int x, unsigned long int n) {
      long double u;
      long rep=x,k;
      cout<<"Initial value, $x_0$ = "<<x<<"\n\\begin{center} \\begin{tabular}{||c | c | c||}  \\hline\n";
      cout<<"$i$ index & $x$ & $u$ \\\\ [0.5ex] \\hline \\hline";
      for(int j=0;j<n;++j) {
        u = (long double)x/(long double)m;
        cout<<j<<" & "<<x<<" & "<<setprecision(10)<<u<<"\\\\\n \\hline \n";
        x = (a*x + b) % m;
        if(rep == x) {
          k=j+1;
          rep = -1;
        }
      }
      cout<<"\\end{tabular} \n \\end{center}";
      cout<<" Repetition after "<<k<<" terms.\n\\\\ \\noindent\\rule[0.5ex]{\\linewidth}{1pt}\n";
      return 0;
    }
     int covariance(unsigned long int x, unsigned long int n) {
      long double u,v;
      cout<<"Initial value, $x_0$ = "<<x<<"\n";
        u = (long double)x/(long double)m;
      for(int j=0;j<n;++j) {
        x = (a*x + b) % m;
        v = u;
        u = (long double)x/(long double)m;
        cout<<"u"<<j<<"= "<<setprecision(10)<<v<<"\tu"<<j+1<<"= "<<u<<"\n";
      }
      cout<<endl;
      return 0;
    }
    //The first value is the value of x0, second no of terms in sequence 
    //and third is the no. of intervals.
    int goodness(unsigned long long int x, unsigned long long int n, int k) {
      long double u;
      // Initializing the density array
      unsigned long long Density[k];
      for(int i=0;i<k;++i) {
        Density[i] = 0;
      }
      //Generating terms
      for(int j=0;j<n;++j) {
        u = (long double)x/(long double)m;
        Density[(int)(u*k)]++ ;
        x = (a*x + b) % m;
      }
      //Printing frequency output
      u = (long double)1/(long double)k;
      cout<<"\n\\begin{center} \\begin{tabular}{||c | c||}  \\hline\n";
      cout<<"Range & Frequency \\\\ [0.5ex] \\hline \\hline";
      for(int i=0;i<k;++i) {
        cout<<setprecision(2)<<(u*i)<<"-"<<(u*(i+1))<<" & "<<Density[i]<<"\\\\\n \\hline \n";
       // cout<<"\t"<<setprecision(20)<<((long double)Density[i]/n)<<endl;
      }
      cout<<"\\end{tabular} \n \\end{center}\n";
      return 0;
    }
  };



