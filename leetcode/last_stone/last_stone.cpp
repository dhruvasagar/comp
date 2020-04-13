#include<iostream>
#include<vector>
#include<queue>
using namespace std;

template<typename T> void print_queue(T q) {
  while(!q.empty()) {
    std::cout << q.top() << " ";
    q.pop();
  }
  std::cout << '\n';
}

int lastStoneWeight(vector<int>& stones)
{
  priority_queue<int> q;
  for (int s : stones)
    q.push(s);

  while (q.size() > 1) {
    int x = q.top();
    q.pop();
    int y = q.top();
    q.pop();

    if (x > y) {
      q.push(x - y);
    }
  }

  if (q.empty())
    return 0;

  return q.top();
}

int main(int argc, char *argv[])
{
  vector<int> stones = {2,2};
  cout << lastStoneWeight(stones) << endl;
  vector<int> stones2 = {2, 7, 4, 1, 8, 1};
  cout << lastStoneWeight(stones2) << endl;
  return 0;
}
