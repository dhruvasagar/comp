#include<stdio.h>

int rangeBitwiseAnd(int m, int n){
  int answer = 0;
  for (int bit = 30; bit >= 0; --bit) {
    int bdigit = (1 << bit);
    if ((m & bdigit) != (n & bdigit)) {
      break;
    } else {
      answer |= (m & bdigit);
    }
  }
  return answer;
}

int main()
{
  int T;
  scanf("%d", &T);
  for (int i = 0; i < T; ++i) {
    int m, n;
    scanf("%d", &m);
    scanf("%d", &n);

    printf("%d\n", rangeBitwiseAnd(m, n));
  }
  return 0;
}
