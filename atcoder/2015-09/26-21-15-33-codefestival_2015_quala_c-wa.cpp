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
vector<vector<ll>> dp;

int main()
{
	cin >> n >> t;
	as.resize(n);
	bs.resize(n);

	ll b_sum = 0;
	rep(i, n)
	{
		cin >> as[i] >> bs[i];
		b_sum += bs[i];
	}
	if ( b_sum > t ) { cout << "-1" << endl; return 0; }

	dp.resize(n+1, vector<ll>(n+1, -1));
	dp[0][0] = t;
	rep(i, n)
	{
		rep(j, n)
		{
			dp[i + 1][j] = max(dp[i + 1][j], dp[i][j] - as[i]);
			dp[i + 1][j + 1] = max(dp[i + 1][j + 1], dp[i][j] - bs[i]);
		}
	}

	rep(j, n)
	{
		if ( 0 <= dp[n][j] ) {
			cout << j << endl;
			break;
		}
	}

	return 0;
}
