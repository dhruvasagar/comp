#include<iostream>
#include<map>
using namespace std;

void printMap(map<char, int> m)
{
  auto it = m.begin();
  cout << "map: " << endl;
  while (it != m.end()) {
    cout << it->first << ": " << it->second;
    it++;
    if (it != m.end())
      cout << ", ";
  }
  cout << endl;
}

bool canConstruct(string ransomNote, string magazine) {
  map<char, int> ransom;
  for (char r : ransomNote)
    ransom[r]++;
  map<char, int> magaz;
  for (char m : magazine)
    magaz[m]++;
  for (char r : ransomNote) {
    if (ransom[r] > magaz[r]) {
      return false;
    }
  }
  return true;
}

int main() {
  cout << canConstruct("a", "b") << endl;
  cout << canConstruct("aa", "ab") << endl;
  cout << canConstruct("aa", "aba") << endl;
  cout << canConstruct("bg", "efjbdfbdgfjhhaiigfhbaejahgfbbgbjagbddfgdiaigdadhcfcj") << endl;
  return 0;
}
