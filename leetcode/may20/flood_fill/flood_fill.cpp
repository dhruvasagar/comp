#include<iostream>
#include<vector>
#include<queue>
#include<map>
using namespace std;

void debugVector(vector<vector<int>> image) {
  cout << "vector: " << endl;
  for (vector<int> row : image) {
    for (int pixel : row) {
      cout << pixel << " ";
    }
    cout << endl;
  }
}

vector<vector<int>> floodFill(vector<vector<int>>& image, int sr, int sc, int newColor) {
  if (image.empty() || image[0].empty())
    return image;
  int H = image.size(), W = image[0].size();
  auto inside = [&](int row, int col) {
    return 0 <= row && row < H && 0 <= col && col < W;
  };
  vector<pair<int, int>> directions = {{1,0}, {0,1}, {-1,0}, {0,-1}};
  vector<vector<bool>> vis(H, vector<bool>(W));
  int color = image[sr][sc];
  queue<pair<int, int>> q;
  q.push({sr, sc});
  vis[sr][sc] = true;
  image[sr][sc] = newColor;
  while (!q.empty()) {
    pair<int, int> p = q.front();
    q.pop();
    for (pair<int, int> dir : directions) {
      int new_row = p.first + dir.first;
      int new_col = p.second + dir.second;
      if (inside(new_row, new_col) && !vis[new_row][new_col] && image[new_row][new_col] == color) {
        q.push({new_row, new_col});
        vis[new_row][new_col] = true;
        image[new_row][new_col] = newColor;
      }
    }
  }
  return image;
}

int main() {
  vector<vector<int>> t1 = {{1,1,1},{1,1,0},{1,0,1}};
  debugVector(t1);
  debugVector(floodFill(t1, 1, 1, 2));
  return 0;
}
