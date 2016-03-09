#include<iostream>
#include<iomanip>
#include"uniform.h"

using namespace std;

int main() {
  UniformGenerator uni;
  //The values of variables
  uni.m = 2048;
  uni.a = 1229;
  uni.b = 1;
  //Taking x per our choice;
  unsigned long int x = 1;
  //Number of values to generate.
  unsigned long int n = 2047;
  uni.covariance(x,n);
  return 0;
}

