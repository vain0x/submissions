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

template<typename Fun> void bitset_each_combi(int n, int k, Fun&& f)
{
	long long const u = (1LL << n) - 1;
	long long s = (1LL << k) - 1;
	while ( !(s & ~u) ) {
		f(s);
		long long const t = (s | (s - 1)) + 1;
		s = t | ((((t & -t) / (s & -s)) >> 1) - 1);
	}
}

int n, m, p, q, r;
vector<vector<int>> gr;
int ma = 0;

int main()
{
	cin >> n >> m >> p >> q >> r;

	gr.resize(n, vector<int>(m, 0));
	rep(i, r)
	{
		int x, y, z;
		cin >> x >> y >> z;
		--x;
		--y;
		gr[x][y] = z;
	}

	vector<int> dp(m);
	bitset_each_combi(n, p, [&dp](int s) {
		fill(all(dp), 0);
		rep(i, n) rep(j, m)
		{
			if ( !(s >> i & 1) ) continue;
			dp[j] += gr[i][j];
		}

		partial_sort(dp.begin(), dp.begin() + q, dp.end(), greater<int>());
		int acc = 0;
		rep(i, q) { acc += dp[i]; }
		ma = max(ma, acc);
	});
	cout << ma << endl;
	return 0;
}