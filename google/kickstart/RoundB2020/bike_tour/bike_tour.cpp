#include<iostream>
using namespace std;

void test_case()
{
  int N;
  scanf("%d", &N);
  int HS[N];
  for (int i = 0; i < N; ++i) {
    scanf("%d", &HS[i]);
  }
  int answer = 0;
  for (int i = 1; i < N-1; ++i) {
    if (HS[i] > HS[i-1] && HS[i] > HS[i+1])
      answer++;
  }
  cout << answer << endl;
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
