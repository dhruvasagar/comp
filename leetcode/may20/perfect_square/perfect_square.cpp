#include<iostream>
using namespace std;

int nsqsrt(int n) {
  if (n == 1)
    return n;
  float x = n / 2;
  while ((x*x - n) > 0.1) {
    x = (x + n/x)/2;
  }
  return x;
}

bool isPerfectSquare(int num) {
  cout << "nsqsrt: " << nsqsrt(num) << endl;
  return nsqsrt(num) * nsqsrt(num) == num;
}

int main() {
  cout << isPerfectSquare(16) << endl;
  cout << isPerfectSquare(14) << endl;
  return 0;
}
