#include<iostream>
#include<vector>
#include<queue>
using namespace std;

string stringShift(string str, vector<vector<int>>& shift)
{
  for (auto s : shift) {
    int d = s[0],
        a = s[1],
        n = str.length();
    for (int i = 1; i < (n-1); ++i) {
      if (d == 0) {
        int t = a;
        while (t--) s[n-1-t] = s[t-i];
        s[i-1] = s[a+i];
      } else {
        int t = a;
        while (t--) s[a-t] = s[a+t];
        s[i+a] = s[n-i];
      }
    }
  }

  return str;
}

int main()
{
  vector<vector<int>> t1 = {{0,1},{1,2}};
  vector<vector<int>> t2 = {{1,1},{1,1},{0,2},{1,3}};
  cout << stringShift("abc", t1) << endl;
  cout << stringShift("abcdefg", t2) << endl;
  return 0;
}
