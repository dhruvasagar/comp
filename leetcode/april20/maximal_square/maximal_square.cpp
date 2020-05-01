#include<iostream>
#include<vector>
#include<math.h>
using namespace std;

int isPerfectSquare(int n) {
  long double sr = sqrt(n);
  return ((sr - floor(sr)) == 0);
}

int maximalSquare(vector<vector<char>>& matrix)
{
  if (matrix.empty() || matrix[0].empty())
    return 0;
  int H = matrix.size(), W = matrix[0].size();
  vector<vector<int>> dp(H, vector<int>(W));
  int answer = 0;
  for (int i = 0; i < H; ++i) {
    for (int j = 0; j < W; ++j) {
      if (matrix[i][j] == '1') {
        dp[i][j] = 1;
        if (i > 0 && j > 0) {
          dp[i][j] += min({dp[i-1][j], dp[i][j-1], dp[i-1][j-1]});
        }
        answer = max(answer, dp[i][j]);
      }
    }
  }
  return answer * answer;
}

int main()
{
  vector<vector<char>> t1 = {
    {'1', '0', '1', '0', '0'},
    {'1', '0', '1', '1', '1'},
    {'1', '1', '1', '1', '1'},
    {'1', '0', '0', '1', '0'},
  };
  cout << maximalSquare(t1) << endl;

  vector<vector<char>> t2 = {
    {'0','0','1','0'},
    {'1','1','1','1'},
    {'1','1','1','1'},
    {'1','1','1','0'},
    {'1','1','0','0'},
    {'1','1','1','1'},
    {'1','1','1','0'},
  };
  cout << maximalSquare(t2) << endl;
  return 0;
}
