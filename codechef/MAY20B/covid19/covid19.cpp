#include<iostream>
#include<vector>
#include<climits>
using namespace std;


void test_case() {
  const int distance = 2;
  int N;
  cin >> N;
  int X[N];
  for (int i = 0; i < N; ++i) {
    cin >> X[i];
  }
  int minGroupSize = INT_MAX, maxGroupSize = 1, groupSize = 1;
  vector<int> groups;
  groups.push_back(X[0]);
  for (int i = 1; i < N; ++i) {
    if((X[i] - groups.back()) > distance) {
      groupSize = groups.size();
      minGroupSize = min(minGroupSize, groupSize);
      maxGroupSize = max(maxGroupSize, groupSize);
      groups.clear();
    }
    groups.push_back(X[i]);
  }
  groupSize = groups.size();
  if (groupSize > 0) {
    minGroupSize = min(minGroupSize, groupSize);
    maxGroupSize = max(maxGroupSize, groupSize);
  }
  cout << minGroupSize << " " << maxGroupSize << endl;
}

int main() {
  int T;
  cin >> T;
  for (int i = 1; i <= T; ++i) {
    test_case();
  }
  return 0;
}
