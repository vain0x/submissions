#include <iostream>
#include <cstdio>
#include <cmath>

using namespace std;
#define rep(_I, _N) for(unsigned int _I = 0; (_I) < (_N); ++ (_I))

int main() { void mymain(); std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }

int norm2(int x, int y) {
	return x * x + y * y;
}

void mymain() {
	int txa, tya, txb, tyb, t, v, n;
	cin >> txa >> tya >> txb >> tyb >> t >> v >> n;

	bool able = false;
	rep(i, n) {
		int x, y;
		cin >> x >> y;

		int d1 = norm2(txa - x, tya - y);
		int d2 = norm2(txb - x, tyb - y);
		if ( sqrt(d1) + sqrt(d2) <= v*t ) able = true;
	}
	cout << (able ? "YES" : "NO") << endl;
}
