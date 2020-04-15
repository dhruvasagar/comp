#include<iostream>
#include<vector>
using namespace std;

void printVector(vector<int> nums)
{
  for (int n : nums)
    cout << n << " ";
  cout << endl;
}

vector<int> productExceptSelf(vector<int>& nums)
{
  int n = nums.size();
  vector<int> l(n), r(n), p(n);
  for (int i = 0; i < n; ++i) {
    l[i] = 1;
    r[i] = 1;
  }

  for (int i = 0; i < n-1; ++i) {
    l[i+1] = l[i]*nums[i];
  }
  for (int i = n-1; i > 0; --i) {
    r[i-1] = r[i]*nums[i];
  }
  for (int i = 0; i < n; ++i) {
    p[i] = l[i]*r[i];
  }
  return p;
}

// Solution in O(1) space, the output vector p(n) does not count towards space
// as per the problem statement
vector<int> productExceptSelf2(vector<int>& nums)
{
  int n = nums.size();
  vector<int> p(n);
  p[0]=1;
  for (int i = 0; i < n-1; ++i) {
    p[i+1] = p[i]*nums[i];
  }
  int R = 1;
  for (int i = n-1; i >= 0; --i) {
    p[i] = p[i] * R;
    R *= nums[i];
  }
  return p;
}

int main()
{
  vector<int> t1 = {1,2,3,4};
  printVector(productExceptSelf(t1));
  printVector(productExceptSelf2(t1));
  return 0;
}
