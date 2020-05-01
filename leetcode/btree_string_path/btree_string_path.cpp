#include<iostream>
#include<vector>
using namespace std;

// Definition for a binary tree node.
struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode() : val(0), left(nullptr), right(nullptr) {}
    TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
    TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left), right(right) {}
};

class Solution {
    bool dfs(TreeNode* root, const vector<int>& arr, int depth) {
        if (root == NULL)
            return false;
        if (depth >= arr.size() || root->val != arr[depth])
            return false;
        if (root->left == NULL && root->right == NULL)
            return depth == arr.size() - 1;
        return dfs(root->left, arr, depth + 1) || dfs(root->right, arr, depth + 1);
    }
public:
    bool isValidSequence(TreeNode* root, vector<int>& arr) {
        return dfs(root, arr, 0);
    }
};
