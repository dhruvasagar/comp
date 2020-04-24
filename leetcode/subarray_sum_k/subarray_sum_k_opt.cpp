#include<iostream>
#include<vector>
#include<unordered_map>
using namespace std;

int subarraySum(vector<int>& nums, int k)
{
  int n = nums.size();
  int answer = 0;
  int pref = 0;
  unordered_map<int, int> countPref;
  countPref[pref]++;
  for (int i = 0; i < n; ++i) {
    pref += nums[i];
    int need = pref - k;
    answer += countPref[need];
    countPref[pref]++;
  }
  return answer;
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
