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

int n, m;
vector<tuple<int, int>> vs;
map<int, int> ks;

int main()
{
	cin >> n >> m;
	vs.resize(m);
	rep(i, m)
	{
		int s, t;
		cin >> s >> t;
		-- s;

		vs[i] = mkt(s, t);
		ks[s * 2] ++;
		ks[t * 2 + 1] --;
	}

	int cur = 0;
	for ( auto&& kv : ks ) {
		cur = (kv.second += cur);
	}
	
	vector<int> result; result.reserve(m);
	rep(i, m)
	{
		int s, t;
		tie(s, t) = vs[i];

		bool ok = true;
		auto it = ks.lower_bound(s * 2);
		auto&& ub = ks.lower_bound(t * 2 + 1);
		while ( it != ub ) {
			if ( it->second <= 1 ) { ok = false; break; }
			++it;
		}
		if ( ok ) {
			result.push_back(i);
		}
	}

	int const k = result.size();
	cout << k << endl;
	rep(i, k)
	{
		cout << (result[i] + 1) << endl;
	}
	return 0;
}
