#include<iostream>
#include<vector>
using namespace std;

int search(vector<int>& nums, int target)
{
  int n = nums.size();
  if (n == 0)
    return -1;
  if (n == 1)
    return nums[0] == target ? 0 : -1;
  if (target < nums[0] && target > nums[n-1])
    return -1;
  if (nums[0] == target)
    return 0;
  if (nums[n-1] == target)
    return n-1;
  int low = 0, high = n - 1;
  int first = nums[0];
  while (low <= high)
  {
    int mid = low + (high - low) / 2;
    int num = nums[mid];
    if (num == target)
      return mid;
    bool am_big = num >= first;
    bool target_big = target >= first;
    if (am_big == target_big) {
      if (num < target) {
        low = mid + 1;
      } else {
        high = mid - 1;
      }
    } else {
      if (am_big) {
        low = mid + 1;
      } else {
        high = mid - 1;
      }
    }
  }
  return -1;
}

int main()
{
  vector<int> arr = {4,5,6,7,0,1,2};
  int t1 = 0;
  int t2 = 3;
  cout << search(arr, t1) << endl;
  cout << search(arr, t2) << endl;
  return 0;
}
