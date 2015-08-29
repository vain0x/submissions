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

signed main()
{
	int ns[5];
	rep(i, 5)
	{
		cin >> ns[i];
	}
	set<int> s;
	rep(i, 5) rep(j, 5) rep(k, 5)
	{
		if ( i != j && j != k && k != i ) {
			s.insert(ns[i] + ns[j] + ns[k]);
		}
	}
	auto it = s.end();
	advance(it, -3);
	cout << *it << endl;

	return 0;
}