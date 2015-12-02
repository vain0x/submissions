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
#include <numeric>
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# include <unordered_map>
# define mkt make_tuple
#endif
#ifdef _LOCAL
# include "local/local.hpp"
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
#define IF
#define THEN ?
#define ELSE :

static ll const MOD = 1000000007;
int n, k_max;

int main()
{
	cin >> n >> k_max;

	vector<ll> dp(k_max, 0);
	dp[0] = 1;
	rep(i, n)
	{
		vector<ll> dp_next(k_max, 0);

		rep(k, k_max) {
			// i 番目の駅に停車しない場合
			// (最初と最後の駅には停車しなければならない)
			if (i != 0 && i != n - 1) {
				dp_next[0] += dp[k];
				dp_next[0] %= MOD;
			}

			// 停車する場合
			if (k + 1 < k_max) {
				dp_next[k + 1] += dp[k];
				dp_next[k + 1] %= MOD;
			}
		}
		swap(dp, dp_next);
	}

	ll kount = 0;
	rep(k, k_max)
	{
		kount += dp[k];
		kount %= MOD;
	}
	cout << kount << endl;
	return 0;
}
