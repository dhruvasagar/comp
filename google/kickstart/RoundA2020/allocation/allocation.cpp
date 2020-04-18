#include<iostream>
#include<vector>
#include<algorithm>
using namespace std;

void printVector(vector<int> v)
{
  for (int vi : v)
    cout << vi << " ";
  cout << endl;
}


void test_case()
{
  int n, b;
  scanf("%d", &n);
  scanf("%d", &b);
  int a[n];
  for (int i = 0; i < n; ++i) {
    scanf("%d", &a[i]);
  }
  sort(a, a+n);
  int cnt = 0;
  for (int i = 0; i < n; ++i) {
    int ai = a[i];
    if (b < ai) break;
    b -= ai;
    cnt++;
  }
  cout << cnt << endl;
}

int main()
{
  int T;
  scanf("%d", &T);
  for (int i = 1; i <= T; ++i) {
    printf("Case #%d: ", i);
    test_case();
  }
  return 0;
}
