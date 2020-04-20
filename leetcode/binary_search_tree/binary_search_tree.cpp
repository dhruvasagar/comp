#include<iostream>
#include<vector>
#include<queue>
using namespace std;

struct TreeNode {
  int val;
  TreeNode* left;
  TreeNode* right;
  TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};

queue<TreeNode*> BSTreeQueue(TreeNode* root)
{
  queue<TreeNode*> q, lq, rq;
  q.push(root);
  if (root->left != NULL) {
    q.push(root->left);
    lq = BSTreeQueue(root->left);
    lq.pop();
  }
  if (root->right != NULL) {
    q.push(root->right);
    rq = BSTreeQueue(root->right);
    rq.pop();
  }
  while (!lq.empty()) {
    q.push(lq.front());
    lq.pop();
  }
  while(!rq.empty()) {
    q.push(rq.front());
    rq.pop();
  }
  return q;
}

void PrintBSTree(TreeNode* root)
{
  queue<TreeNode*> q = BSTreeQueue(root);
  cout << endl;
  while (!q.empty()) {
    TreeNode* n = q.front();
    cout << n->val <<  " ";
    q.pop();
  }
  cout << endl;
}

void insert(TreeNode* root, int val)
{
  TreeNode* node = root;
  while (node != NULL) {
    if (val < node->val) {
      if (node->left == NULL)
        break;
      node = node->left;
    } else {
      if (node->right == NULL)
        break;
      node = node->right;
    }
  }
  TreeNode* cnode = new TreeNode(val);
  if (val < node->val)
    node->left = cnode;
  else
    node->right = cnode;
}

TreeNode* bstFromPreorder(vector<int>& preorder)
{
  TreeNode* root = new TreeNode(preorder[0]);
  int n = preorder.size();
  for (int i = 1; i < n; i++) {
    insert(root, preorder[i]);
  }
  return root;
}

int main()
{
  vector<int> t1 = {8,5,1,7,10,12};
  TreeNode* root = bstFromPreorder(t1);
  PrintBSTree(root);
  return 0;
}
