#include<iostream>
#include<iomanip>
using namespace std;

void test_case()
{
  double N;
  cin >> N;
  double prob = (N - 1) / N;
  cout << fixed;
  cout << setprecision(6);
  cout << prob << endl;
}

int main()
{
  int T;
  cin >> T;
  for (int i = 1; i <= T; ++i) {
    test_case();
  }
  return 0;
}
