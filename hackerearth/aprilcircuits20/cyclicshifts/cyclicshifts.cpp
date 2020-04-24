#include<iostream>
#define us unsigned short
using namespace std;

void test_case()
{
  us N, m;
  char c;
  cin >> N >> m >> c;
  if (c == 'L')
    cout << (us)((N << m) + (N >> (16-m))) << endl;
  else
    cout << (us)((N >> m) + (N << (16-m))) << endl;
}

int main()
{
  int T;
  scanf("%d", &T);
  for (int i = 1; i <= T; ++i) {
    test_case();
  }
  return 0;
}
