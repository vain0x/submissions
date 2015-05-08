#include <random>
#include <memory>
#include <functional>
#include <algorithm>
#include <array>
#include <vector>
#include <map>
#include <set>
#include <string>
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

int n;
int gs[10000];
int gcs[10000];

void mymain() {
	cin >> n;

	gs[0] = 0;
	int gg = 0;
	repi(i, 1, n) {
		int c, a;
		cin >> c >> a;

		copy(&gs[i - c], &gs[i], gcs);
		sort(gcs, gcs + c);
		int g = 0;
		rep(j, c) {
			if ( g == gcs[j] ) ++g;
		}
		gs[i] = g;

		if ( (a & 1) != 0 ) gg ^= gs[i];
	}

	cout << (gg != 0 ? "First" : "Second") << endl;
}
