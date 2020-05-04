#include<iostream>
#define ull unsigned long long
using namespace std;

int numOf1s(ull n) {
  int ans = 0;
  while (n) {
    int r = n % 2;
    if (r == 1)
      ans++;
    n /= 2;
  }
  return ans;
}

void test_case() {
  ull P, M;
  cin >> P >> M;
  cout << numOf1s(P ^ M) << endl;
}

int main() {
  int T;
  cin >> T;
  for (int i = 1; i <= T; ++i) {
    test_case();
  }
  return 0;
}
