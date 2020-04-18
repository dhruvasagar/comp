#include<iostream>
#include<vector>
#include<map>
#include<queue>
using namespace std;

int numIslands(vector<vector<char>>& grid)
{
  if (grid.empty() || grid[0].empty())
    return 0;
  int H = grid.size();
  int W = grid[0].size();
  auto inside = [&](int row, int col) {
    return 0 <= row && row < H && 0 <= col && col < W;
  };
  vector<pair<int,int>> directions{{1,0},{0,1},{-1,0},{0,-1}};
  vector<vector<bool>> vis(H, vector<bool>(W));
  int answer = 0;
  for (int i = 0; i < H; ++i) {
    for (int j = 0; j < W; ++j) {
      if (!vis[i][j] && grid[i][j] == '1') {
        answer++;
        queue<pair<int,int>> q;
        q.push({i, j});
        vis[i][j] = true;
        while(!q.empty()) {
          pair<int, int> p = q.front();
          q.pop();
          for (pair<int,int> dir : directions) {
            int new_row = p.first + dir.first;
            int new_col = p.second + dir.second;
            if (inside(new_row, new_col) && !vis[new_row][new_col] && grid[new_row][new_col] == '1') {
              q.push({new_row, new_col});
              vis[new_row][new_col] = true;
            }
          }
        }
      }
    }
  }
  return answer;
}

int main()
{
  vector<vector<char>> t1 = {
    {'1','1','1','1','0'},
    {'1','1','0','1','0'},
    {'1','1','0','0','0'},
    {'0','0','0','0','0'},
  };
  vector<vector<char>> t2 = {
    {'1','1','0','0','0'},
    {'1','1','0','0','0'},
    {'0','0','1','0','0'},
    {'0','0','0','1','1'},
  };
  cout << numIslands(t1) << endl;
  cout << numIslands(t2) << endl;
  return 0;
}
