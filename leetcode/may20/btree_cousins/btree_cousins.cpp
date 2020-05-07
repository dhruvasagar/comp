#include<iostream>
using namespace std;

/* Definition for a binary tree node. */
struct TreeNode {
  int val;
  TreeNode *left;
  TreeNode *right;
  TreeNode() : val(0), left(nullptr), right(nullptr) {}
  TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
  TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left), right(right) {}
};

class Solution {
  pair<int, TreeNode*> dfs(TreeNode* root, int x, int depth) {
    pair<int, TreeNode*> p = {-1, NULL};
    if (!root)
      return {-1, NULL};
    if (root->left) {
      if (root->left->val == x)
        return {depth+1, root};
      p = dfs(root->left, x, depth+1);
    }
    if (p.first == -1) {
      if (root->right) {
        if (root->right->val == x)
          return {depth+1, root};
        p = dfs(root->right, x, depth+1);
      }
    }
    return p;
  }
  public:
  bool isCousins(TreeNode* root, int x, int y) {
    pair<int, TreeNode*> p1 = dfs(root, x, 0);
    pair<int, TreeNode*> p2 = dfs(root, y, 0);
    return p1.first == p2.first && p1.second != p2.second;
  }
};
