#include<iostream>
#include<map>
using namespace std;

int numJewelsInStones(string J, string S) {
  map<char, bool> isJewel;
  for (char j : J)
    isJewel[j] = true;
  int ans = 0;
  for (char s : S)
    if (isJewel[s])
      ans++;
  return ans;
}

int main() {
  cout << numJewelsInStones("aA", "aAAbbbb") << endl;
  cout << numJewelsInStones("z", "ZZ") << endl;
  return 0;
}
