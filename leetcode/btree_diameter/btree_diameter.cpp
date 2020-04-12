#include<iostream>
using namespace std;

struct TreeNode {
  int val;
  TreeNode *left;
  TreeNode *right;
  TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};

class Solution {
  pair<int, int> dfs(TreeNode *root) {
    if (root == NULL) {
      return {0, 0};
    }

    pair<int, int> left = dfs(root->left);
    pair<int, int> right = dfs(root->right);

    int diam = max({left.first, right.first, left.second + right.second});
    return {diam, max(left.second, right.second)};
  }

  public:
  int diameterOfBinaryTree(TreeNode *root) {
    return dfs(root).first;
  }
};
