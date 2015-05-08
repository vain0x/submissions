#include <iostream>
#include <cstdio>
#include <ctime>
#include <climits>
#include <cassert>

#ifdef _LOCAL
# include "for_local.h"
#else
# define echo(...) ((void)0)
#endif

using namespace std;
using uint = unsigned int;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (unsigned int _I = (_Init); _I < (_N); ++ (_I))
#define rep(_I, _N) for(unsigned int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) begin(_X), end(_X)
#define rall(_X) rbegin(_X), rend(_X)
#define mkp make_pair
#define mkt make_tuple
#define empb emplace_back
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
