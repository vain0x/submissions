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


int main()
{
	int n, m;
	cin >> n >> m;
	map<int, int> fs;
	rep(i, n)
	{
		int a;
		cin >> a;
		fs[a] ++;
	}

	vector<pair<int, int>> gs;
	for ( auto& kv : fs ) {
		gs.emplace_back(kv.second, kv.first);
	}
	sort(all(gs));
	int ma = gs.back().first;
	auto&& ran = equal_range(all(gs), make_pair(ma, 0),
		[](pair<int, int> const& l, pair<int, int> const& r) { return l.first < r.first; });
	assert(ran.second == gs.end());
	int k = distance(ran.first, ran.second);
	if ( k == 1 && ma > ((n + 1) / 2) ) {
		cout << gs.back().second << endl;
	} else {
		cout << '?' << endl;
	}

	return 0;
}
