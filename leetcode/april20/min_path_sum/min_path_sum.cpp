#include<iostream>
#include<vector>
using namespace std;

void print2DArray(vector<vector<int>> a) {
  cout << endl;
  cout << "array: " << endl;
  int M = a.size(), N = a[0].size();
  for (int i = 0; i < M; ++i) {
    for (int j = 0; j < N; ++j) {
      cout << a[i][j] << " ";
    }
    cout << endl;
  }
  cout << endl;
}

int cache[1000][1000];
int minPathSumRec(vector<vector<int>>& grid, int m, int n)
{
  if (n < 0 || m < 0)
    return INT_MAX;
  else if (m == 0 && n == 0)
    return grid[m][n];
  else if (cache[m][n] != -1)
    return cache[m][n];
  else
    return cache[m][n] = grid[m][n] + min(
        minPathSumRec(grid, m-1, n),
        minPathSumRec(grid, m, n-1)
        );
}

int minPathSum(vector<vector<int>>& grid)
{
  return minPathSumRec(grid, grid.size()-1, grid[0].size()-1);
}

int main()
{
  vector<vector<int>> t1 = {{1,3,1}, {1,5,1}, {4,2,1}};
  memset(cache, -1, sizeof(cache));
  cout << minPathSum(t1) << endl;
  return 0;
}
