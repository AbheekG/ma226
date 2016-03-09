#include<iostream>
#include<iomanip>
#include"uniform.h"

using namespace std;

int main() {
  UniformGenerator uni;
  //The values of variables
  uni.m = 244944;
  uni.a = 1597;
  uni.b = 0;
  //Taking x per our choice;
  unsigned long int x = 1;
  cout<<"#a= "<<uni.a<<"  b= "<<uni.b<<"  m= "<<uni.m<<"  x0= "<<x<<"\n";
  //Number of values to generate.
  unsigned long int n = 100;
  for(int i=0;i<5;++i) {
    cout<<"#n= "<<n<<endl;
    uni.goodness(x,n,20);
    n=n*10;
    cout<<"\n\n";
  }
  return 0;
}

