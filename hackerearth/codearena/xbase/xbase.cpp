#include<iostream>
#include<math.h>
#define ll long long
using namespace std;

int numOfDigitsInBase(ll n, int k)
{
  return floor(log(n) / log(k)) + 1;
}

void solve(ll n, int k)
{
  int ans = 1;
  for (ll i = 1; i <= n; ++i) {
    ans += numOfDigitsInBase(i, k);
  }
  cout << ans << endl;
}

void test_case()
{
  ll n;
  int k;
  scanf("%lld", &n);
  cin >> k;
  solve(n, k);
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
