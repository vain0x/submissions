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

int n, m, s[2];
vector<int> as[2];

int dfs(int port, int t)
{
	auto&& lb = lower_bound(all(as[port]), t);
	if ( lb != as[port].end() ) {
		t = *lb;
		return 1 + dfs(1 - port, t + s[port]);
	}
	return 0;
}

int main()
{
	cin >> n >> m >> s[0] >> s[1];
	as[0].resize(n);
	as[1].resize(m);
	rep(i, n)
	{
		cin >> as[0][i];
	}
	rep(i, m)
	{
		cin >> as[1][i];
	}

	cout << (dfs(0, 0) / 2) << endl;

	return 0;
}