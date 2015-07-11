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
#else
# define debug if (false)
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()

signed main()
{
	int n;
	cin >> n;

	vector<vector<int>> cs(n);
	repi(i, 1, n)
	{
		int b;
		cin >> b; --b;
		cs[b].push_back(i);
	}

	vector<ll> dp(n, 0);
	for ( int i = n - 1; i >= 0;--i ) {
		if ( cs[i].empty() ) {
			dp[i] = 1;
		} else {
			ll mi = ll(1)<<60, ma = 0;
			for ( auto&& c : cs[i] ) {
				mi = min(mi, dp[c]);
				ma = max(ma, dp[c]);
			}
			dp[i] = mi + ma + 1;
		}
	}

	cout << dp[0] << endl;


	return 0;
}
