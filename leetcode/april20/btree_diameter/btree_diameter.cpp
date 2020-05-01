#include<iostream>
#include<vector>
using namespace std;

struct TreeNode {
  int val;
  TreeNode *left;
  TreeNode *right;
  TreeNode(int x) : val(x), left(NULL), right(NULL) {}

  bool isFull() {
    return left != NULL && right != NULL;
  }
};

void Print(TreeNode *tree) {
  TreeNode *node = tree;
  cout << node->val << endl;
  if (node->left != NULL) {
    cout << node->val << "->";
    Print(node->left);
  }
  if (node->right != NULL) {
    cout << node->val << "->";
    Print(node->right);
  }
}

class Solution {
  pair<int, int> dfs(TreeNode *root) {
    if (root == NULL) {
      return {0, 0};
    }

    pair<int, int> left = dfs(root->left);
    pair<int, int> right = dfs(root->right);

    int diam = max({left.first, right.first, left.second + right.second});
    return {diam, 1 + max(left.second, right.second)};
  }

  public:
  TreeNode *tree;

  Solution(vector<int> values) {
    tree = new TreeNode(values[0]);
    TreeNode *node = tree;
    for (int i = 1; i < values.size(); i++) {
      TreeNode *cnode = new TreeNode(values[i]);
      while (node->isFull()) {
        if (node->left != NULL) {
          node = node->left;
        } else {
          node = node->right;
        }
      }
      if (node->left == NULL) {
        node->left = cnode;
      } else {
        node->right = cnode;
      }
    }
  }

  int diameterOfBinaryTree() {
    return dfs(tree).first;
  }
};

int main()
{
  vector<int> values = {1, 2, 3, 4, 5};
  Solution *solution = new Solution(values);
  Print(solution->tree);
  cout << solution->diameterOfBinaryTree() << endl;
}
