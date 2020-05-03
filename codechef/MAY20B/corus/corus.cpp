#include<iostream>
#include<vector>
#include<map>
#define ll long long
using namespace std;

void solve(int N, string S, ll C) {
  // C queues for each isolation center
  vector<map<char, bool>> cs(C);
  int ans = 0;
  for (int i = 0; i < N; ++i) {
    char virus = S[i];
    bool found = false;
    for (ll i = 0; i < C; ++i) {
      if (cs[i].find(virus) != cs[i].end()) {
        continue;
      }
      found = true;
      cs[i][virus] = true;
      break;
    }
    if (!found) {
      ans++;
    }
  }
  cout << ans << endl;
}

void solve2(int N, string S, ll C) {
  ll ans = 0;
  map<char, int> reps;
  for (int i = 0; i < N; ++i) {
    char v = S[i];
    reps[v]++;
    if (reps[v] > C)
      ans++;
  }
  cout << ans << endl;
}

void test_case() {
  int N, Q;
  cin >> N >> Q;
  string S;
  cin >> S;
  for (int i = 0; i < Q; ++i) {
    ll C;
    cin >> C;
    solve2(N, S, C);
  }
}

int main() {
  int T;
  cin >> T;
  for (int i = 0; i < T; ++i) {
    test_case();
  }
}
