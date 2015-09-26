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

int main()
{
	int n;
	cin >> n;

	vector<int> deg(n, 0);
	vector<vector<int>> gr(n);

	auto&& add_edge = [&](int x, int y) {
		gr[x].push_back(y);
		gr[y].push_back(x);
		++deg[x];
		++deg[y];
	};

	rep(i, n - 1)
	{
		int x, y;
		cin >> x >> y;
		--x; --y;
		add_edge(x, y);
	}

	// 次数最大の点
	int const r = distance(deg.begin(), max_element(all(deg)));

	vector<int> par(n, -1), depth(n, 0);

	{
		vector<bool> done(n, false);
		stack<int> st;
		st.push(r);
		int p = -1;
		while (! st.empty() ) {
			int const v = st.top();
			st.pop();
			if ( done[v] ) continue;
			done[v] = true;

			for ( int u : gr[v] )
			{
				if ( done[u] ) continue;
				st.push(u);
				par[u] = v;
				depth[u] = depth[v] + 1;
			}
		}
	}

	int q;
	cin >> q;

	rep(i, q)
	{
		int a, b;
		cin >> a >> b; --a; --b;

		int dep_a = depth[a];
		int dep_b = depth[b];
		int l = 1;

		while ( dep_a > dep_b ) {
			a = par[a];
			--dep_a;
			++l;
			assert(dep_a == depth[a]);
		}
		while ( dep_a < dep_b ) {
			b = par[b];
			--dep_b;
			++l;
			assert(dep_b == depth[b]);
		}
		while ( a != b ) {
			a = par[a]; --dep_a; ++l;
			b = par[b]; --dep_b; ++l;
		}
		cout << l << endl;
	}
	return 0;
}
