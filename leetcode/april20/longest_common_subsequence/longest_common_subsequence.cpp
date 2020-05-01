#include<iostream>
#include<vector>
#include<map>
using namespace std;

int max(int a, int b)
{
  if (a > b)
    return a;
  return b;
}

int lcs(string X, string Y, int m, int n)
{
  int dp[m + 1][n + 1];
  int i, j;
  /*
   * Following steps build dp[m+1][n+1] in bottom up fashion. Note that dp[i][j]
   * contains length of LCS of X[0..i-1] and Y[0..j-1]
   */
  for (i = 0; i <= m; i++)
  {
    for (j = 0; j <= n; j++)
    {
      if (i == 0 || j == 0)
        dp[i][j] = 0;
      else if (X[i - 1] == Y[j - 1])
        dp[i][j] = dp[i - 1][j - 1] + 1;
      else
        dp[i][j] = max(dp[i - 1][j], dp[i][j - 1]);
    }
  }
  /* dp[m][n] contains length of LCS for X[0..n-1] and Y[0..m-1] */
  return dp[m][n];
}

int longestCommonSubsequence(string text1, string text2)
{
  return lcs(text1, text2, text1.length(), text2.length());
}

int main()
{
  cout << longestCommonSubsequence("abcde", "ace") << endl;
  cout << longestCommonSubsequence("abc", "abc") << endl;
  cout << longestCommonSubsequence("abc", "def") << endl;
  cout << longestCommonSubsequence("oxcpqrsvwf", "shmtulqrypy") << endl;
  cout << longestCommonSubsequence("bl", "yby") << endl;
  cout << longestCommonSubsequence("ezupkr", "ubmrapg") << endl;
  cout << longestCommonSubsequence("ylqpejqbalahwr", "yrkzavgdmdgtqpg") << endl;
  return 0;
}
