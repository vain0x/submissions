#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <string>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#ifndef ONLINE_JUDGE //POJ
# include <random>
# include <array>
# define mkt make_tuple
# define empb emplace_back
#endif
#ifdef _LOCAL
# include "for_local.h"
#else
# define echo(...) ((void)0)
#endif
using namespace std;
typedef unsigned int uint; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()
int main() { void mymain(); mymain(); return 0; }

//http://www.slideshare.net/chokudai/abc024

using ll = long long;
static ll const divisor = 1000000007;
ll getmod(ll x) { return ((x % divisor) + divisor) % divisor; }

ll mpow(ll x, int n) {
	ll b = 1;
	for (int i = 0; (n >> i) != 0; ++i) {
		if ( (n >> i) & 1 ) {
			b = (b * x) % divisor;
		}
		x = (x * x) % divisor;
	}
	return b;
}
ll inv(ll x) {
	return mpow(x, divisor - 2);
}

void mymain() {
	ll x, y, z;
	cin >> x >> y >> z;

	ll r, c;
	r = getmod(y * z - z * x);
	c = getmod(z * y - y * x);
	r = getmod(r * inv(getmod(y * x - z * y + x * z)));
	c = getmod(c * inv(getmod(z * x - y * z + x * y)));
	cout << r << ' ' << c << endl;
}
