#include<iostream>
#include<map>
using namespace std;

void printNs(int Ns[], int n)
{
  for (int i = 0; i < n; ++i) {
    cout << Ns[i] << " ";
  }
  cout << endl;
}

map<int, bool> primes;
bool isPrime(int n)
{
  if (n <= 1)
    return false;

  if (n <= 3)
    return true;

  auto p = primes.find(n);
  if (p != primes.end())
    return p->second;

  if (n % 2 == 0 || n % 3 == 0) {
    primes[n] = false;
    return false;
  }

  for (int i = 5; i*i <= n; i+=6) {
    if (n % i == 0 || n % (i + 2) == 0) {
      primes[n] = false;
      return false;
    }
  }
  return primes[n] = true;
}

void test_case()
{
  int N;
  cin >> N;
  int Ns[N];
  for (int i = 0; i < N; ++i) {
    cin >> Ns[i];
    // build cache
    isPrime(Ns[i]);
  }
  int Q;
  cin >> Q;
  for (int i = 0; i < Q; ++i) {
    string s;
    int X, Y;
    cin >> s >> X >> Y;
    if (s == "C")
    {
      Ns[X-1] = Y;
      isPrime(Y);
    }
    else
    {
      int ans = 0;
      for (int i = X-1; i < Y; ++i) {
        if (isPrime(Ns[i]))
          ans++;
      }
      cout << ans << endl;
    }
  }
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
