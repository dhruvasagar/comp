#include<iostream>
#include<map>
using namespace std;

class Solution {
public:
    int firstUniqChar(string s) {
        map<char, int> oc;
        for (char c : s) {
            oc[c]++;
        }
        for (int i = 0; i < s.length(); i++) {
            char c = s[i];
            if (oc[c] == 1)
                return i;
        }
        return -1;
    }
};

int main() {
  return 0;
}
