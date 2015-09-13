#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <cstring>
#include <string>
#include <stack>
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
# include <unordered_map>
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

int n, m, d;
vector<int> as;

void solve()
{
	vector<int> s(n), t(n);

	rep(i, n)
	{
		t[i] = i;
	}

	rep(i, m)
	{
		swap(t[as[i]], t[as[i] + 1]);
	}

	rep(i, n)
	{
		s[t[i]] = i;
	}

	vector<bool> done(n, false);
	rep(i, n)
	{
		if ( done[i] ) continue;

		vector<int> c;
		int j = i;
		do {
			done[j] = true;
			c.push_back(j);
			j = s[j];
		} while (j != i);

		int const step = d % c.size();
		rep(k, c.size())
		{
			t[c[k]] = c[(k + step) % c.size()];
		}
	}

	rep(i, n)
	{
		cout << (t[i] + 1) << endl;
	}
	return;
}

signed main()
{
	cin >> n >> m >> d;
	as.resize(m);
	rep(i, m)
	{
		cin >> as[i];
		as[i] --;
	}

	solve();
	return 0;
}
