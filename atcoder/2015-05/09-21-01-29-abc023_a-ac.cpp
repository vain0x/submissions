#include <iostream>
#include <cstdio>

using namespace std;

#define empb emplace_back
int main() { void mymain(); std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }

void mymain() {
	int x;
	cin >> x;

	cout << (x / 10 + x % 10) << endl;
}
