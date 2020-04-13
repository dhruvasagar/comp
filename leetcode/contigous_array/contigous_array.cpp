#include<iostream>
#include<vector>
#include<map>
using namespace std;

int findMaxLength(vector<int>& nums)
{
  int maxLength = 0;
  map<int, int> counts;
  counts[0]=-1;
  int sum = 0;
  for (int i = 0; i < nums.size(); i++) {
    sum += (nums[i] == 0) ? -1 : 1;
    auto count = counts.find(sum);
    if (count != counts.end()) {
      maxLength = max(maxLength, i-counts[sum]);
    } else {
      counts[sum] = i;
    }
  }
  return maxLength;
}

int main()
{
  vector<int> t1 = {0, 1};
  vector<int> t2 = {0, 1, 0};
  vector<int> t3 = {1,1,1,1,1,1};
  cout << findMaxLength(t1) << endl;
  cout << findMaxLength(t2) << endl;
  cout << findMaxLength(t3) << endl;
}
