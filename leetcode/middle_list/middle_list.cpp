#include<iostream>
using namespace std;

struct Node {
  int val;
  Node* next;
  Node(int x) : val(x), next(NULL) {}
};

Node* createLinkedList(int n, int arr[])
{
  Node* head = new Node(arr[0]);
  Node* ptr = head;
  for (int i = 1; i < n; ++i) {
    Node* node = new Node(arr[i]);
    ptr->next = node;
    ptr = node;
  }
  return head;
}

Node* middleNode(Node* head)
{
  Node *ptr = head, *mid = head;
  int cnt = 1;
  while (ptr->next != NULL) {
    cnt++;
    if(cnt%2==0){
      mid = mid->next;
    }
    ptr = ptr->next;
  }
  return mid;
}

void log(Node* node)
{
  Node *ptr = node;
  cout << "[";
  while (ptr != NULL) {
    cout << ptr->val;
    if (ptr->next != NULL) {
      cout << ",";
    }
    ptr = ptr->next;
  }
  cout << "]" << endl;
}

int main(int argc, char *argv[])
{
  int arr1[5] = {1,2,3,4,5}, arr2[6] = {1,2,3,4,5,6};
  log(middleNode(createLinkedList(5, arr1)));
  log(middleNode(createLinkedList(6, arr2)));
  return 0;
};
