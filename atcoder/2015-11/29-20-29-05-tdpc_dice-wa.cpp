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
#include <memory>
#include <numeric>
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# include <unordered_map>
# include <unordered_set>
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

ll n;
ll d;

double solve()
{
	double p = 0;
	unordered_map<ll, double> dp, dp2;
	dp[1] = 1;

	rep(i, n)
	{
		dp2.clear();

		for ( auto&& kv : dp ) {
			repi(j, 1, 7)
			{
				auto&& k2 = kv.first * j;
				double p2 = kv.second / 6;

				if ( k2 % d == 0 ) {
					p += p2;
				} else {
					dp2[move(k2)] += p2;
				}
			}
		}

		swap(dp, dp2);
	}

	for ( auto&& kv : dp ) {
		if ( kv.first % d == 0 ) {
			p += kv.second;
		}
	}
	return p;
}

int main()
{
	cin >> n >> d;

	printf("%.16f\n", solve());
	return 0;
}
