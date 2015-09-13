#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <cstring>
#include <string>
#include <stack>
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
# include <unordered_map>
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

ll n, h, a, b, c, d, e;
unordered_map<ll, ll> dp, dp_next;

ll solve()
{
	if ( h > e * n ) return 0;

	ll mi = 1LL << 62;

	dp[0] = h;
	rep(i, n)
	{
		// 残りの食事を全部抜いたときの満腹度減少量
		ll const req = e * (n - 1 - i);

		dp_next.clear();
		for ( auto&& kv : dp ) {
			if ( req < kv.second + b ) {
				mi = min(mi, kv.first + a);
			} else {
				dp_next[kv.first + a] = max(dp_next[kv.first + a], kv.second + b);
			}
			if ( req < kv.second + d ) {
				mi = min(mi, kv.first + c);
			} else {
				dp_next[kv.first + c] = max(dp_next[kv.first + c], kv.second + d);
			}
		}
		swap(dp, dp_next);
	}

	for ( auto&& kv : dp ) {
		mi = min(mi, kv.first);
	}
	return mi;
}

signed main()
{
	cin >> n >> h >> a >> b >> c >> d >> e;
	cout << solve() << endl;
	return 0;
}
