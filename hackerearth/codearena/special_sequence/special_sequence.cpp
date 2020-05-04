#include<iostream>
#include<vector>
#include<algorithm>
using namespace std;

void test_case() {
  int N;
  cin >> N;
  vector<int> A(N);
  for (int i = 0; i < N; ++i) {
    cin >> A[i];
  }
  sort(A.begin(), A.end());
  int d = A[1] - A[0];
  bool ans = true;
  for (int i = 2; i < N; ++i) {
    if ((A[i] - A[i-1]) != d) {
      ans = false;
      break;
    }
  }
  cout << (ans ? "YES" : "NO") << endl;
}

int main() {
  int T;
  cin >> T;
  for (int i = 1; i <= T; ++i) {
    test_case();
  }
  return 0;
}
