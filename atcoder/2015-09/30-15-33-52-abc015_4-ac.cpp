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

int w_max, n, k_max;
vector<int> xs, vs;

int solve()
{
	vector<vector<int>> dp(w_max, vector<int>(k_max, 0));

	rep(i, n)
	for (int j = w_max - 1; j >= 0; -- j )
			rep(k, k_max - 1)
	{
		if ( j + xs[i] < w_max ) {
			dp[j + xs[i]][k + 1] =
				max(dp[j + xs[i]][k + 1], dp[j][k] + vs[i]);
		}
	}

	int ma = 0;
	rep(j, w_max) rep(k, k_max)
	{
		ma = max(ma, dp[j][k]);
	}
	return ma;
}

int main()
{
	cin >> w_max >> n >> k_max;
	++w_max;
	++k_max;
	xs.resize(n);
	vs.resize(n);

	rep(i, n)
	{
		cin >> xs[i] >> vs[i];
	}

	cout << solve() << endl;
	return 0;
}
