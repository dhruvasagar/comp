#include<iostream>
#include<regex>
using namespace std;

string applyBackspace(string S)
{
  auto sub = S.find("#");
  if (sub == string::npos) return S;
  regex re("[a-z]#");
  return applyBackspace(regex_replace(S, re, ""));
}

bool backspaceCompare(string S, string T)
{
  if (S.length() != T.length()) return false;
  string sb = applyBackspace(S), tb = applyBackspace(T);
  cout << sb << " " << tb << endl;
  return sb == tb;
}

int main(int argc, char *argv[])
{
  cout << backspaceCompare("ab#c", "ad#c") << endl;
  cout << backspaceCompare("ab##", "c#d#") << endl;
  cout << backspaceCompare("a##c", "#a#c") << endl;
  cout << backspaceCompare("a#c", "b") << endl;
  return 0;
}
