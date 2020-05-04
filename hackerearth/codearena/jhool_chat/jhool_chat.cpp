#include<iostream>
using namespace std;

void solve(string msg) {
  const string s = "love";
  int index = 0;
  bool ans = false;
  for (char m : msg) {
    if (s[index] == m) {
      index++;
      if (index == s.size()) {
        ans = true;
        break;
      }
    }
  }
  cout << (ans ? "I love you, too!" : "Let us breakup!") << endl;
}

int main() {
  string msg;
  cin >> msg;
  solve(msg);
  return 0;
}
