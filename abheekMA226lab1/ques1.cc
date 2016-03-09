#include <iostream>
#include <iomanip>
#include "uniform.h" //Main file with functions

using namespace std;

int main() {
  UniformGenerator uni;
  //The values of variables
  cout << "Enter a, b and m respectively.\n";
  cin>> uni.a >> uni.b >> uni.m;
  for(int i=0;i<11;++i)
    uni.generate(i,12);


  return 0;
}




