#include<iostream>
#include<vector>
using namespace std;

int CountSubArray(vector<int>& nums, int idx, int k)
{
  int n = nums.size();
  int s = 0;
  int cnt = 0;
  for (int i = idx; i < n; ++i) {
    int num = nums[i];
    s += num;
    if (s == k)
      cnt++;
  }
  return cnt;
}

int subarraySum(vector<int>& nums, int k)
{
  int n = nums.size();
  int cnt = 0;
  for (int i = 0; i < n; ++i) {
    cnt += CountSubArray(nums, i, k);
  }
  return cnt;
}

int main()
{
  vector<int> t1 = {1,1,1};
  cout << subarraySum(t1, 2) << endl;
  vector<int> t2 = {28,54,7,-70,22,65,-6};
  cout << subarraySum(t2, 100) << endl;
  vector<int> t3 = {0,0,0,0,0,0,0,0,0,0};
  cout << subarraySum(t3, 0) << endl;
  return 0;
}
