#include <iostream>
#include <cstdio>
#include <string>
#include <cstring>
#include <algorithm>
#include <numeric>
#include <vector>
#include <list>
#include <map>
#include <set>
#include <stack>
#include <queue>
#include <functional>
#include <cmath>
#include <memory>
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

using pt = pair<int, int>; // rate, hand

int main()
{
	int n;
	cin >> n;

	auto s = vector<pt>(n);
	rep(i, n)
	{
		cin >> s[i].first >> s[i].second;
		s[i].second --;
	}

	auto t = vector<pair<pt, int>>();
	rep(i, n)
	{
		t.emplace_back(s[i], i);
	}
	sort(all(t));
	rep(i, n)
	{
		s[i] = t[i].first;
	}

	auto solve = [&] (int i) {
		auto rate = s[i].first;
		auto hand = s[i].second;

		auto lo = upper_bound(all(s), make_pair(rate, -1));
		auto hi = lower_bound(all(s), make_pair(rate + 1, 0));

		auto bd = equal_range(all(s), make_pair(rate, (hand + 0) % 3));
		auto bw = equal_range(all(s), make_pair(rate, (hand + 1) % 3));
		auto bl = equal_range(all(s), make_pair(rate, (hand + 2) % 3));

		auto w = distance(bw.first, bw.second) + distance(begin(s), lo);
		auto l = distance(bl.first, bl.second) + distance(hi, end(s));
		auto d = distance(bd.first, bd.second);
		return make_tuple(w, l, d - 1);
	};

	auto results = vector<tuple<int, int, int>>(n);
	rep(i, n)
	{
		results[t[i].second] = solve(i);
	}
	rep(i, n)
	{
		int w, l, d;
		tie(w, l, d) = results[i];
		cout << w << ' ' << l << ' ' << d << endl;
	}

	return 0;
}
