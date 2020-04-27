#include<iostream>
#include<algorithm>
using namespace std;

string reverse(string s)
{
  string rev = "";
  for (char c : s)
    rev = c + rev;
  return rev;
}

int main()
{
  int N;
  cin >> N;
  string passwords[N];
  for (int i = 0; i < N; ++i) {
    cin >> passwords[i];
  }
  bool found = false;
  string password;
  for (int i = 0; i < N; ++i) {
    string p = passwords[i];
    string revp = reverse(p);
    for (int j = i; j < N; ++j) {
      if (passwords[j] == revp) {
        password = p;
        found = true;
        break;
      }
    }
    if (found)
      break;
  }
  int mid = password.length() / 2;
  cout << password.length() << " " << password[mid] << endl;
  return 0;
}
