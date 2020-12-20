#include<iostream>
#include<string>
#include<stack>
#include<ctype.h>
using namespace std;

const char ADD = '+';
const char MUL = '*';
const char PARL = '(';
const char PARR = ')';

int precedence(char c) {
  switch (c) {
    case ADD: return 2;
    case MUL: return 1;
  }
  return 0;
}

long int evalOp(long int a, long int b, char op) {
  switch (op) {
    case ADD: return a + b;
    case MUL: return a * b;
  }
  return 0;
}

void evalOpOnStack(stack<char> &ops, stack<long int> &vals) {
  long int val2 = vals.top();
  vals.pop();

  long int val1 = vals.top();
  vals.pop();

  char op = ops.top();
  ops.pop();

  vals.push(evalOp(val1, val2, op));
}

long int evaluate(string expr) {
  stack<char> ops;
  stack<long int> vals;

  for (int i = 0; i < expr.length(); i++) {
    if (expr[i] == ' ')
      continue;

    else if (isdigit(expr[i])) {
      int val = 0;
      for (; i < expr.length() && isdigit(expr[i]); i++) {
        val = val*10 + (expr[i]-'0');
      }
      vals.push(val);
      i--;
    }

    else if (expr[i] == PARL) {
      ops.push(expr[i]);
    }

    else if (expr[i] == PARR) {
      while (!ops.empty() && ops.top() != PARL) {
        evalOpOnStack(ops, vals);
      }

      if (!ops.empty()) {
        ops.pop();
      }
    }

    else {
      while (!ops.empty() && precedence(ops.top()) >= precedence(expr[i])) {
        evalOpOnStack(ops, vals);
      }

      ops.push(expr[i]);
    }
  }

  while (!ops.empty()) {
    evalOpOnStack(ops, vals);
  }

  return vals.top();
}

int main() {
  long int sum = 0;
  for (string expr; getline(cin, expr);) {
    sum += evaluate(expr);
  }
  cout << sum << endl;
  return 0;
}
