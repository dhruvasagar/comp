#include<iostream>
#include<map>
using namespace std;


map<int, int> dmemory;
int sum_of_divisors(int n)
{
  auto mem = dmemory.find(n);
  if (mem != dmemory.end()) {
    return mem->second;
  }

  int sum = 0;
  for (int i = 1; i < n; ++i) {
    if (n % i == 0)
      sum += i;
  }
  dmemory[n] = sum;
  return sum;
}


map<int, int> amemory;
bool are_amicable(int a, int b)
{
  if (a == b) return false;
  auto mem = amemory.find(a);
  if (mem != amemory.end()) {
    return b == mem->second;
  }

  bool res = sum_of_divisors(a) == b && sum_of_divisors(b) == a;
  if (res) {
    amemory[a] = b;
    amemory[b] = a;
  }
  return res;
}

int sum_amicable(int max)
{
  int sum = 0;
  for (int i = 1; i < max; ++i) {
    for (int j = i+1; j < max; ++j) {
      if (are_amicable(i, j)) {
        cout << "Found amicable numbers: " << i << " and " << j << endl;
        sum += i + j;
      }
    }
  }
  return sum;
}

int main(int argc, char *argv[])
{
  const int max = 10000;
  cout << sum_amicable(max) << endl;
  /* cout << are_amicable(220, 284) << endl; */
  return 0;
}

