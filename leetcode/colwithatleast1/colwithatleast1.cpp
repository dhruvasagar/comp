#include<iostream>
#include<vector>
using namespace std;

class BinaryMatrix {
  vector<vector<int>> mat;
  public:
  BinaryMatrix(vector<vector<int>> m) {
    mat = m;
  }

  int get(int x, int y) {
    return mat[x][y];
  }

  vector<int> dimensions() {
    return {(int)mat.size(), (int)mat[0].size()};
  }
};

int leftMostColumnWithOne(BinaryMatrix &binaryMatrix) {
  vector<int> dimensions = binaryMatrix.dimensions();
  int N = dimensions[0], M = dimensions[1];
  int low = 0, high = M-1, prevMid = -1;
  int qcnt = 0;
  while (low <= high) {
    int mid = low + (high - low) / 2;
    bool foundOne = false;
    for (int i = 0; i < N; ++i) {
      int n = binaryMatrix.get(i, mid);
      qcnt++;
      if (n == 1) {
        foundOne = true;
        break;
      }
    }
    if (foundOne) {
      prevMid = mid;
      high = mid - 1;
    } else {
      low = mid + 1;
    }
  }
  /* cout << "Number of queries: " << qcnt << endl; */
  return prevMid;
}

int main()
{
  vector<vector<int>> t1 = {{0,0,0,1}, {0,0,1,1}, {0,1,1,1}};
  BinaryMatrix* b1 = new BinaryMatrix(t1);
  cout << leftMostColumnWithOne(*b1) << endl;

  vector<vector<int>> t2 = {{0,0}, {1,1}};
  BinaryMatrix* b2 = new BinaryMatrix(t2);
  cout << leftMostColumnWithOne(*b2) << endl;

  vector<vector<int>> t3 = {{0,0}, {0,0}};
  BinaryMatrix* b3 = new BinaryMatrix(t3);
  cout << leftMostColumnWithOne(*b3) << endl;
  return 0;
}
