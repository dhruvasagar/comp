#include<iostream>
#include<vector>
#include<map>
using namespace std;

void printMap(map<int, bool> m)
{
  auto it = m.begin();
  while (it != m.end())
  {
    cout << it->first << " ";
    it++;
  }
  cout << endl;
}

bool canJump(vector<int>& nums)
{
  int n = nums.size();
  int can_reach = 0;
  for (int i = 0; i <= can_reach; ++i) {
    if (i == n - 1)
      return true;
    can_reach = max(can_reach, i + nums[i]);
  }
  return false;
}

int main()
{
  vector<int> t1 = {2,3,1,1,4};
  cout << canJump(t1) << endl;
  vector<int> t2 = {3,2,1,0,4};
  cout << canJump(t2) << endl;
  vector<int> t3 = {2,3,1,2,0,4};
  cout << canJump(t3) << endl;
  vector<int> t5 = {2,5,0,0};
  cout << canJump(t5) << endl;
  vector<int> t6 = {1,1,2,2,0,1,1};
  cout << canJump(t6) << endl;
  return 0;
}
