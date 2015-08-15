#if 1
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
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# define mkt make_tuple
#endif
#ifdef _LOCAL
# include "for_local.h"
#else
# define ifdebug if (false)
# define echo ((void)0)
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()
#define mkp make_pair
inline int scani() { int n; scanf(" %d", &n); return n; }

signed main()
{
	int n, a, b;
	cin >> n >> a >> b;

	vector<ll> ss(n);
	ll ma =0, mi = 1LL<<59;
	rep(i, n)
	{
		ll x; cin >> x;
		ss[i] = x;
		ma = max(ma, x);
		mi = min(mi, x);
	}

	if ( ma == mi && b != 0 ) {
		cout << "-1" << endl; return 0;
	}
	double p = (ma == mi)
		? 1
		: double(b) / (ma - mi);

	double ave = 0;
	rep(i, n)
	{
		ave += p * ss[i];
	}
	ave /= n;
	double q = a - ave;

	printf("%.16f %.16f\n", p, q);
	return 0;
}
#endif
