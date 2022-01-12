#include<iostream>
#include<vector>
#include<algorithm>
using namespace std;

bool has_overlap(vector<int> a, vector<int> b)
{
  if (a.empty() || b.empty()) {
    return false;
  }
  return max(a[0], b[0]) < min(a[1], b[1]);
}

bool has_overlap_with(vector<vector<int> >& activities, vector<int> activity)
{
  if (activities.empty()) {
    return false;
  }
  for (int i = 0; i < activities.size(); ++i) {
    if (has_overlap(activities[i], activity)) {
      return true;
    }
  }
  return false;
}

string assign_activities(vector<vector<int> > activities)
{
  int n = activities.size();
  vector<string> as(n);
  vector<vector<int> > cs(n);
  vector<vector<int> > js(n);
  sort(activities.begin(), activities.end());

  for (int i = 0; i < n; ++i) {
    vector<int> activity = activities[i];
    if (has_overlap_with(cs, activity)) {
      if (has_overlap_with(js, activity)) {
        return "IMPOSSIBLE";
      }
      js.push_back(activity);
      as[activity[2]] = "J";
    } else {
      cs.push_back(activity);
      as[activity[2]] = "C";
    }
  }
  string s = "";
  for (int i = 0; i < n; ++i) {
    s += as[i];
  }
  return s;
}

void test_case()
{
  int n;
  scanf("%d", &n);
  vector<vector<int> > activities(n);
  for (int i = 0; i < n; ++i) {
    vector<int> activity(3);
    scanf("%d", &activity[0]);
    scanf("%d", &activity[1]);
    activity[2] = i;
    activities[i] = activity;
  }
  cout << assign_activities(activities) << endl;
}

int main()
{
  int T;
  scanf("%d", &T);
  for (int i = 1; i <= T; ++i) {
    printf("Case #%d: ", i);
    test_case();
  }
}
