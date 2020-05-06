#include<iostream>
#include<map>
using namespace std;

class Solution {
public:
    int majorityElement(vector<int>& nums) {
        map<int, int> oc;
        int size = nums.size();
        for (int n : nums) {
            oc[n]++;
            if (oc[n] > size/2)
                return n;
        }
        return -1;
    }
};
