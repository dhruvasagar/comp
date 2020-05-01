#include<iostream>
using namespace std;

// The API isBadVersion is defined for you.
bool isBadVersion(int version);

class Solution {
  public:
    int firstBadVersion(int n) {
      int low = 0, high = n;
      int ans = -1;
      while (low <= high) {
        int mid = low + (high - low) / 2;
        if (isBadVersion(mid)) {
          ans = mid;
          high = mid - 1;
        } else {
          low = mid + 1;
        }
      }
      return ans;
    }
};
