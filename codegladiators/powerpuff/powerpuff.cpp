#include<iostream>
#include<vector>
using namespace std;

int main(int argc, char *argv[])
{
  long long n;
  scanf("%lld", &n);
  vector<long long int> mi(10000000);
  vector<long long int> ai(10000000);
  for (long long i = 0; i < n; ++i) {
    scanf("%lld", &mi[i]);
  }
  for (long long i = 0; i < n; ++i) {
    scanf("%lld", &ai[i]);
  }
  long long minQ;
  for (long long i = 0; i < n; ++i) {
    long long int m, a, q;
    m = mi[i];
    a = ai[i];
    q = a/m;
    if (minQ == 0 || minQ > q) {
      minQ = q;
    }
  }
  cout << minQ << endl;
  return 0;
}
