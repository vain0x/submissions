#include <iostream>
#include <cstdio>
#include <string>
#include <cstring>
#include <algorithm>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <queue>
#include <functional>
#include <cmath>
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
# define assert(...) ((void)0)
# define ifdebug if (false)
# define echo(...) ((void)0)
#endif
using namespace std;
typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

ll n, t;
vector<ll> as, bs;
vector<int> dp;
static int const INF = 1<<29;

int solve()
{
	dp.resize(n + 1, INF);
	dp[0] = 0;

	int m = 0;
	rep(j, n)
	{
		for ( int i = j + 1; i >= m; --i ) {
			if ( i == 0 ) {
				dp[0] += as[j];
			} else {
				dp[i] = min(dp[i] + as[j], dp[i - 1] + bs[j]);
			}
		}

		repi(i, m, j + 2)
		{
			if ( dp[i] <= t ) { m = i; break; }
		}
	}

	ifdebug {
		rep(i, n)
		{
			if ( dp[i] <= t ) { assert(m == i); break; }
		}
	}
	return (m == n ? -1 : m);
}

int main()
{
	cin >> n >> t;
	as.resize(n);
	bs.resize(n);

	rep(i, n)
	{
		cin >> as[i] >> bs[i];
	}

	cout << solve() << endl;
	return 0;
}
