#include<iostream>
#include<stack>
using namespace std;

class MinStack {
  stack<pair<int, int>> s;

  public:

  MinStack() {}

  void push(int x) {
    if (s.empty()) {
      s.push({x, x});
    } else {
      s.push({x, min(x, s.top().second)});
    }
  }

  void pop() {
    if (!s.empty()) {
      s.pop();
    }
  }

  int top() {
    return s.top().first;
  }

  int getMin() {
    return s.top().second;
  }
};

int main()
{
  MinStack* obj = new MinStack();
  obj->push(-2);
  obj->push(0);
  obj->push(-3);
  cout << "min: " << obj->getMin() << endl;
  obj->pop();
  cout << "top: " << obj->top() << endl;
  cout << "min: " << obj->getMin() << endl;
  return 0;
}
