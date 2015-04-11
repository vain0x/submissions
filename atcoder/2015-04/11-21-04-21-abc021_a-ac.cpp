#include <iostream>
using namespace std;
#define repi(_I, _Init, _N) for (int _I = (_Init); _I < (_N); ++ _I)
#define rep(_I, _N) repi(_I, 0, _N)

void mymain()
{
	int n;
	cin >> n;
	int k = 0;
	rep(i, 4) { if ( n & (1 << i) ) k ++; }
	cout << k << endl;
	rep(i, 4) {
		int b = 1 << i;
		if ( n & b ) cout << b << endl;
	}
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
