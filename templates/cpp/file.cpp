#include<iostream>
#include<vector>
#include<map>
using namespace std;

void debugVector(vector<int> nums) {
  cout << "vector: ";
  for (int n : nums)
    cout << n << " ";
  cout << endl;
}

void debugMap(map<int, int> m) {
  cout << "map: ";
  auto it = m.begin();
  while (it != m.end()) {
    cout << it->first << ": " << it->second << ", ";
    it++;
  }
  cout << endl;
}

void test_case() {

}

int main() {
  int T;
  cin >> T;
  for (int i = 1; i <= T; ++i) {
    printf("Case #%d: ", i);
    test_case();
  }
  return 0;
}
