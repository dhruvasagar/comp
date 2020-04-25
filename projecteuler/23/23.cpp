#include<iostream>
using namespace std;

const int limit = 28123;

int cache[limit];
int sumOfDivisors(int n)
{
  if (cache[n] != -1)
    return cache[n];
  int sum = 0;
  for (int i = 2; i < n; ++i) {
    if (n % i == 0) {
      sum += n/i;
    }
  }
  return cache[n] = sum + 1;
}

bool isAbundant(int n)
{
  return sumOfDivisors(n) > n;
}

bool isSumOfAbundant(int n)
{
  for (int i = 12; i < n; ++i) {
    if (isAbundant(i) && isAbundant(n-i))
      return true;
  }
  return false;
}

int main()
{
  memset(cache, -1, sizeof(cache));
  int ans = 0;
  cout << isSumOfAbundant(limit) << endl;
  for (int i = 1; i < limit; ++i) {
    if (!isSumOfAbundant(i))
      ans += i;
  }
  cout << ans << endl;
  return 0;
}
