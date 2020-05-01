#include<iostream>
#include<vector>
#include<queue>
#include<map>
using namespace std;

class FirstUnique {
  private:
    queue<int> q;
    map<int,int> occur;
  public:
    FirstUnique(vector<int>& nums) {
      for (int n : nums)
        add(n);
    }

    int showFirstUnique() {
      while (!q.empty() && occur[q.front()] >= 2)
        q.pop();
      if (q.empty())
        return -1;
      return q.front();
    }

    void add(int value) {
      occur[value]++;
      q.push(value);
    }
};

int main()
{
  vector<int> t1 = {809};
  FirstUnique *firstUnique1 = new FirstUnique(t1);
  cout << "showFirstUnique: " << firstUnique1->showFirstUnique() << endl;
  firstUnique1->add(809);
  cout << "showFirstUnique: " << firstUnique1->showFirstUnique() << endl;

  cout << endl;
  vector<int> t2 = {};
  FirstUnique *firstUnique2 = new FirstUnique(t2);
  firstUnique2->add(233);
  cout << "showFirstUnique: " << firstUnique2->showFirstUnique() << endl;
  firstUnique2->add(11);
  cout << "showFirstUnique: " << firstUnique2->showFirstUnique() << endl;

  cout << endl;
  vector<int> t3 = {7,7,7,7,7,7};
  FirstUnique *firstUnique3 = new FirstUnique(t3);
  cout << "showFirstUnique: " << firstUnique3->showFirstUnique() << endl;
  firstUnique3->add(7);
  firstUnique3->add(3);
  firstUnique3->add(3);
  firstUnique3->add(7);
  firstUnique3->add(17);
  cout << "showFirstUnique: " << firstUnique3->showFirstUnique() << endl;

  cout << endl;
  vector<int> t4 = {2,3,5};
  FirstUnique* firstUnique4 = new FirstUnique(t4);
  cout << "showFirstUnique: " << firstUnique4->showFirstUnique() << endl;
  firstUnique4->add(5);
  cout << "showFirstUnique: " << firstUnique4->showFirstUnique() << endl;
  firstUnique4->add(2);
  cout << "showFirstUnique: " << firstUnique4->showFirstUnique() << endl;
  firstUnique4->add(3);
  cout << "showFirstUnique: " << firstUnique4->showFirstUnique() << endl;

  return 0;
}
