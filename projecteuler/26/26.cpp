#include<iostream>
#include<map>
using namespace std;

const int maxn = 999;

string getRecurDigits(int num)
{
  int d = 1;
  string res;
  map<int, int> mp;
  mp.clear();
  int rem = d % num;

  while ((rem != 0) && (mp.find(rem) == mp.end()))
  {
    mp[rem] = res.length();

    rem = rem * 10;

    int res_part = rem / num;
    res += to_string(res_part);

    rem = rem % num;
  }
  return (rem == 0) ? "" : res.substr(mp[rem]);
}

int max(int a, int b)
{
  if (a > b)
    return a;
  return b;
}

int main()
{
  int maxi = 0, maxl = 0;
  for (int i = 2; i < maxn; ++i) {
    int recurDigits = getRecurDigits(i).length();
    if (recurDigits > maxl) {
      maxl = recurDigits;
      maxi = i;
    }
  }
  cout << maxi << endl;
  return 0;
}
