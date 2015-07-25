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
#define mkt make_tuple
#define all(_X) (_X).begin(), (_X).end()
inline int scani() { int n; scanf("%d", &n); return n; }

using pii = pair<int,int>;
int n, p;
vector<pii> xs;

signed main()
{
	cin >> n >> p;
	
	xs.resize(n);
	rep(i, n)
	{
		cin >> xs[i].first >> xs[i].second;
	}
	sort(all(xs));

	int result = 0;
	rep(j, n) {
		vector<int> dp(p + 1, 0);
		repi(i, j + 1, n)
		{
			for(int q = p; q >= 0; --q) {
				if ( q < xs[i].first ) continue;
				dp[q] = max(dp[q],
					dp[q - xs[i].first] + xs[i].second);
			}
		}
		result = max(result, dp[p] + xs[j].second);
	}
	cout << result << endl;

	return 0;
}
