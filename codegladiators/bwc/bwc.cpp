#include<iostream>
#include<vector>
#include<algorithm>
using namespace std;

const int NIL = -1;

void test_case()
{
  int n;
  scanf("%d", &n);
  vector<long long int> gpowers(n);
  vector<long long int> opowers(n);
  vector<int> faught(n);
  for (int i = 0; i < n; ++i) {
    scanf("%lld", &gpowers[i]);
    faught[i]=NIL;
  }
  for (int i = 0; i < n; ++i) {
    scanf("%lld", &opowers[i]);
  }
  sort(gpowers.begin(), gpowers.end());
  sort(opowers.begin(), opowers.end(), greater<int>());
  for (int i = 0; i < n; ++i) {
    long long gp = gpowers[i];
    for(int j = 0; j < n; ++j) {
      if (faught[j] != NIL) {
        continue;
      }
      long long op = opowers[j];
      if (gp > op) {
        faught[j] = 1;
        break;
      }
    }
  }
  long long int res = 0;
  for (int i = 0; i < n; ++i) {
    if (faught[i] == 1) {
      res++;
    }
  }
  cout << res << endl;
}

int main(int argc, char *argv[])
{
  int T;
  scanf("%d", &T);
  for (int i = 1; i <= T; ++i) {
    test_case();
  }
  return 0;
}
