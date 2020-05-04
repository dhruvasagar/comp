#include<iostream>
#include<math.h>
using namespace std;

int nextPowerOf2minus1(int n) {
  return pow(2, floor(log(n) / log(2)) + 1) - 1;
}

int findComplement(int n) {
  return n ^ nextPowerOf2minus1(n);
}

int main() {
  cout << findComplement(5) << endl;
  cout << findComplement(1) << endl;
  cout << findComplement(2) << endl;
  return 0;
}
