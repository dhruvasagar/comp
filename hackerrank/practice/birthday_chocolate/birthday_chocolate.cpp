#include<iostream>
#include<vector>
using namespace std;

void printVector(vector<int> S) {
  for (int s : S)
    cout << s << " ";
  cout << endl;
}

int validBar(vector<int> S, int D, int M, int idx) {
  int r = 0;
  for (int i = 0; i < M; ++i) {
    r += S[idx + i];
  }
  return r == D ? 1 : 0;
}

int countValidBars(vector<int> S, int D, int M, int idx, int cnt) {
  if (idx == S.size())
    return cnt;
  cnt += validBar(S, D, M, idx);
  return countValidBars(S, D, M, idx + 1, cnt);
}

void solve(vector<int>& S, int D, int M) {
  cout << countValidBars(S, D, M, 0, 0) << endl;
}

int main() {
  int N;
  cin >> N;
  vector<int> S(N);
  for (int i = 0; i < N; ++i) {
    cin >> S[i];
  }
  int D, M;
  cin >> D >> M;
  solve(S, D, M);
  return 0;
}
