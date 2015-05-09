#include <random>
#include <memory>
#include <functional>
#include <algorithm>
#include <array>
#include <vector>
#include <queue>
#include <map>
#include <set>
#include <string>
#include <iostream>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <cmath>
#include <climits>
#include <cassert>
#include <bitset>

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

int n;

ull hs[100000 + 1];
ull ss[100000 + 1];
uint us[100000 + 1];

void mymain() {
	cin >> n;

	ull maxh = 0;
	rep(i, n) {
		cin >> hs[i] >> ss[i];
		maxh = max(maxh, hs[i]);
	}

	ull low = maxh; //達成不可能な最大数
	ull high = ull(1e9) * 3 + 1; //達成可能な最小数
	rep(_, 50) {
		int m = (high + low) / 2;

		memset(us, 0, n * sizeof(uint));
		rep(i, n) {
			ull u = (m - hs[i]) / ss[i]; //t[i]はu以下でなければいけない
			us[u] ++;
		}

		bool ok = true;
		uint u = 0;
		rep(i, n) {
			u += us[i];
			if ( u > i + 1 ) { ok = false; break; }
		}
		if ( ok ) {
			high = m;
		} else {
			low = m;
		}
		if ( low == high ) break;
	}

	cout << high << endl;
}