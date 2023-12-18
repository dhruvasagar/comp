#include<iostream>
#include<vector>
#include<set>
#include<queue>
#include<string>
using namespace std;

const pair<int, int> dirs[4] = {
  {1,0},
  {0,-1},
  {-1,0},
  {0,1},
};

bool is_backward(pair<int, int>dir1, pair<int, int>dir2) {
  if (dir1.first == 0) return dir1.second == (-1 * dir2.second);
  if (dir1.second == 0) return dir1.first == (-1 * dir2.first);
  return false;
}

struct Node {
  int cost;
  int dir_index;
  int block_size;
  pair<int, int> pos;

  bool operator<(const Node &n) const {
    return cost > n.cost;
  }
};

struct VisitedItem {
  int dir_index;
  int block_size;
  pair<int, int> pos;

  bool operator<(const VisitedItem &v) const {
    if (dir_index == v.dir_index) {
      if (block_size == v.block_size) {
        return pos < v.pos;
      }
      return block_size < v.block_size;
    }
    return dir_index < v.dir_index;
  }

  bool operator==(const VisitedItem &v) const {
    return dir_index == v.dir_index && block_size == v.block_size && pos == v.pos;
  }
};

struct Grid {
  int ymax;
  int xmax;
  vector<vector<int>> grid;

  Grid(vector<vector<int>> nums) {
    grid = nums;
    ymax = nums.size();
    xmax = nums[0].size();
  }

  bool outside(pair<int, int> npos) {
    return npos.first < 0 || npos.first >= xmax || npos.second < 0 || npos.second >= ymax;
  }

  int findPath(int min, int max) {
    set<VisitedItem> visited {{0, 0, {0, 0}}};
    priority_queue<Node> queue;
    queue.push({0,0,0,{0,0}});
    pair<int, int> target = {xmax-1, ymax-1};
    while (!queue.empty()) {
      Node node = queue.top();
      queue.pop();

      for (int i = 0; i < 4; i++) {
        if (i == node.dir_index && node.block_size == max) continue;
        if (i != node.dir_index && node.block_size < min) continue;

        int block_size = 1;
        if (i == node.dir_index) block_size = node.block_size + 1;

        int dx = dirs[i].first, dy = dirs[i].second;
        pair<int, int> npos = {node.pos.first + dx, node.pos.second + dy};
        if (outside(npos) || is_backward(dirs[i], dirs[node.dir_index])) continue;

        int ncost = node.cost + grid[npos.second][npos.first];
        if (npos == target) return ncost;

        if (visited.count({i, block_size, npos}) > 0) continue;
        visited.insert({i, block_size, npos});

        queue.push({ncost, i, block_size, npos});
      }
    }
    return 0;
  }
};

int main() {
  vector<vector<int>> vnums;
  for (string line; getline(cin, line);) {
    vector<int> nums;
    for(char c : line) {
      int num = int(c) - 48;
      nums.push_back(num);
    }
    vnums.push_back(nums);
  }
  Grid grid = Grid(vnums);
  cout << grid.findPath(0, 3) << endl;
  cout << grid.findPath(4, 10) << endl;
}
