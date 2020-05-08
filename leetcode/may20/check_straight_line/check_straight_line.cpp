#include<iostream>
#include<vector>
using namespace std;

void printCoordinate(vector<int> coordinate) {
  cout << coordinate[0] << " " << coordinate[1] << endl;
}

void printCoordinates(vector<vector<int>> coordinates) {
  for (vector<int> coord : coordinates)
    printCoordinate(coord);
}

float slope(vector<int> p1, vector<int> p2) {
  return float(p2[1] - p1[1]) / (p2[0] - p1[0]);
}

bool checkStraightLine(vector<vector<int>>& coordinates) {
  int n = coordinates.size();
  if (n == 0)
    return false;
  if (n <= 2)
    return true;
  float m = slope(coordinates[1], coordinates[0]);
  for (int i = 1; i < n; ++i) {
    float mn = slope(coordinates[i], coordinates[i-1]);
    if (m != mn)
      return false;
  }
  return true;
}

int main() {
  vector<vector<int>> t1 = {{1,2},{2,3},{3,4},{4,5},{5,6},{6,7}};
  cout << checkStraightLine(t1) << endl;
  vector<vector<int>> t2 = {{1,1},{2,2},{3,4},{4,5},{5,6},{7,7}};
  cout << checkStraightLine(t2) << endl;
  return 0;
}
