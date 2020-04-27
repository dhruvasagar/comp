#include<iostream>
#include<vector>
using namespace std;

void test_case()
{
  int N, M, X1, Y1, X2, Y2;
  cin >> N >> M >> X1 >> Y1 >> X2 >> Y2;
  vector<vector<int>> mat(N, vector<int>(M));
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      cin >> mat[i][j];
    }
  }
  vector<pair<int, int>> directions = {{1,0}, {0,1}, {-1,0}, {0,-1}};
  bool rescue = false;
  auto inside = [&](int row, int col) {
    return 0 <= row && row < N && 0 <= col && col < M;
  };
  vector<vector<bool>> vis(N, vector<bool>(M));
  int x = X1-1, y = Y1-1;
  vis[x][y] = true;
  while (1)
  {
    bool found = false;
    for (auto d : directions) {
      int nx = x + d.first;
      int ny = y + d.second;
      if (inside(nx, ny) && !vis[nx][ny] && mat[nx][ny] > mat[x][y]) {
        x = nx;
        y = ny;
        vis[nx][ny] = true;
        found = true;
      }
    }
    if (x == X2-1 && y == Y2-1) {
      rescue = true;
      break;
    }
    if (!found) {
      break;
    }
  }
  cout << (rescue ? "YES" : "NO") << endl;
}

int main()
{
  int T;
  cin >> T;
  for (int i = 1; i <= T; ++i) {
    test_case();
  }
  return 0;
}
