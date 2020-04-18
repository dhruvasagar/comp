#include<iostream>
#include<stack>
using namespace std;

void printStack(stack<char> st)
{
  while (!st.empty()) {
    char stc = st.top();
    st.pop();
    cout << stc << " ";
  }
  cout << endl;
}

bool checkValidString(string s)
{
  int scnt = 0;
  stack<char> st;
  for (char c : s) {
    if (c == '(') {
      st.push(c);
    } else if (c == '*') {
      scnt++;
    } else if (c == ')') {
      /* cout << "* count : " << scnt << " stack size: " << st.size() << endl; */
      if (scnt == 0 && st.empty())
        return false;

      if (st.empty()) {
        scnt--;
      } else {
        st.pop();
      }
    }
    /* cout << "test"; */
    printStack(st);
  }
  if (scnt > 0 && !st.empty()) {
    for (int i = 0; i < scnt; ++i) {
      if (!st.empty()) {
        st.pop();
      }
    }
  }
  return st.empty();
}

int main()
{
  /* cout << checkValidString("()") << endl; */
  /* cout << checkValidString("(*)") << endl; */
  /* cout << checkValidString("())") << endl; */
  /* cout << checkValidString("(()") << endl; */
  /* cout << checkValidString("(*))") << endl; */
  /* cout << checkValidString("(*()") << endl; */
  cout << checkValidString("(())((())()()(*)(*()(())())())()()((()())((()))(*") << endl;
  return 0;
}
