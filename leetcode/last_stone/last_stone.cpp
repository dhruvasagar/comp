#include<iostream>
#include<vector>
using namespace std;

int lastStoneWeight(vector<int>& stones)
{
  if (stones.size() == 0) {
    return 0;
  }
  if (stones.size() == 1) {
    return stones[1];
  }

  sort(stones.begin(), stones.end(), greater<int>());
  if (stones[0] == stones[1]) {
    stones.erase(stones.begin(), stones.begin()+2);
  } else {
    stones[0] = stones[0]-stones[1];
    stones.erase(stones.begin()+1);
  }

  return lastStoneWeight(stones);
}

int main(int argc, char *argv[])
{
  vector<int> stones = {2,2};
  cout << lastStoneWeight(stones) << endl;
  vector<int> stones2 = {2, 7, 4, 1, 8, 1};
  cout << lastStoneWeight(stones2) << endl;
  return 0;
}
