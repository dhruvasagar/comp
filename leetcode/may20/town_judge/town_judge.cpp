#include<iostream>
#include<vector>
#include<map>
using namespace std;

void printVector(vector<int> nums) {
  cout << "vector: ";
  for (int n : nums)
    cout << n << " ";
  cout << endl;
}

void printMap(map<int, int> m) {
  cout << "map: ";
  auto it = m.begin();
  while (it != m.end()) {
    cout << it->first << ": " << it->second << ", ";
    it++;
  }
  cout << endl;
}

/*
 * Town Judge rules :
 * 1. The Judge trusts nobody.
 * 2. Everybody (except for the town judge) trusts the town judge.
 * 3. There is exactly one person that satisfies properties 1 and 2.
 */
int findJudge(int N, vector<vector<int>>& trust) {
  if (N == 2 && trust.size() == 1)
    return trust[0][1];
  map<int, int> tm;
  for (vector<int> vt : trust) {
    tm[vt[0]] = vt[1];
  }
  for (vector<int> vt : trust) {
    if (tm.find(vt[1]) == tm.end()) {
      tm[vt[0]] = vt[1];
    }
  }
  printMap(tm);
  for (int i = 1; i <= N; ++i) {
    if (tm.find(i) != tm.end()) // i violates rule #1, so can't be judge
      continue;
    // Check for rule #2
    bool allTrust = true;
    for (int j = 1; j <= N; ++j) {
      if (j == i)
        continue;
      allTrust = allTrust && tm[j] == i;
      if (!allTrust)
        break;
    }
    if (allTrust)
      return i;
  }
  return -1;
}

int main() {
  vector<vector<int>> t1 = {{1,2}};
  cout << findJudge(2, t1) << endl;
  vector<vector<int>> t2 = {{1,3},{2,3}};
  cout << findJudge(3, t2) << endl;
  vector<vector<int>> t3 = {{1,3},{2,3},{3,1}};
  cout << findJudge(3, t3) << endl;
  vector<vector<int>> t4 = {{1,2},{2,3}};
  cout << findJudge(3, t4) << endl;
  vector<vector<int>> t5 = {{1,3},{1,4},{2,3},{2,4},{4,3}};
  cout << findJudge(4, t5) << endl;
  vector<vector<int>> t6 = {{1,8},{1,3},{2,8},{2,3},{4,8},{4,3},{5,8},{5,3},{6,8},{6,3},{7,8},{7,3},{9,8},{9,3},{11,8},{11,3}};
  cout << findJudge(11, t6) << endl;
  return 0;
}
