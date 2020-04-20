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

int minPathSum(vector<vector<int>>& nums)
{
  print2DArray(nums);
  int M = nums.size(), N = nums[0].size();
  vector<vector<int>> dp(M, vector<int>(N));
  dp[0][0] = nums[0][0];
  for (int i = 1; i < M; ++i) {
    dp[i][0] = dp[i-1][0] + nums[i][0];
  }
  print2DArray(dp);
  for (int j = 1; j < N; ++j) {
    dp[0][j] = dp[0][j-1] + nums[0][j];
  }
  print2DArray(dp);
  for (int i = 1; i < M; ++i) {
    for (int j = 1; j < N; ++j) {
      dp[i][j] = nums[i][j] + min(dp[i-1][j], dp[i][j-1]);
    }
  }
  print2DArray(dp);
  return dp[M-1][N-1];
}

int main()
{
  vector<vector<int>> t1 = {{1,3,1}, {1,5,1}, {4,2,1}};
  cout << minPathSum(t1) << endl;
  return 0;
}

