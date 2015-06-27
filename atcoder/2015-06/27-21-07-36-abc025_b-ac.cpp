#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <cstring>
#include <string>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#ifndef ONLINE_JUDGE //POJ
# include <complex>
# include <random>
# include <array>
# define mkt make_tuple
# define empb emplace_back
#endif
#ifdef _LOCAL
# include "for_local.h"
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()
inline int scani() { int n; scanf("%d", &n); return n; }

int n, a, b;


signed main() {
	cin >> n >> a >> b;

	ll p = 0;
	rep(i, n) {
		int d;
		string s;
		cin >> s >> d;
		
		int sg = (s == "East" ? 1 : -1);
		p += sg * max(min(d, b), a);
	}

	if ( p == 0 ) {
		cout << '0' << endl;
	} else {
		cout << (p > 0 ? "East" : "West") << ' ' << abs(p) << endl;
	}
	return 0;
}
