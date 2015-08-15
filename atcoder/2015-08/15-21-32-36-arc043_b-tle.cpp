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

int const MOD = 1000000007;

int n;
vector<ll> ds;
vector<ll> ds_next;

//dp[j][i]: j番目の問題の難しさがds[i]であるような選び方の数
vector<ll> dp[4];

//dp[j + 1][i] = sum { dp[j][k] | ds[k] <= ds[i] / 2 }

int solve()
{
	dp[0].resize(n, 1);
	repi(i, 1, 4)
	{
		dp[i].resize(n, 0);
	}

	repi(j, 1, 4)
	{
		rep(i, n)
		{
			repi(k, ds_next[i], n)
			{
				dp[j][k] = (dp[j][k] + dp[j - 1][i]) % MOD;
			}
		}
	}

	ll total = 0;
	rep(j, n)
	{
		total = (total + dp[3][j]) % MOD;
	}
	return total;
}

signed main()
{
	cin >> n;
	ds.resize(n);
	rep(i, n)
	{
		cin >> ds[i];
	}
	sort(all(ds));

	rep(i, n)
	{
		ll k = distance(
			begin(ds),
			lower_bound(all(ds), ds[i] * 2)
			);
		ds_next.push_back(k);
	}

	cout << solve() << endl;
	return 0;
}
#endif
