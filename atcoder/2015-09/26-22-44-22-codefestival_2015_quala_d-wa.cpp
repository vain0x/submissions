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

static int const INF = 1<<30;

int n, m;
vector<int> ds;

bool check(int t)
{
	int rig = 0;
	rep(i, m)
	{
		int lef = max(0, ds[i] - rig);
		if ( lef > t ) return false;

		rig = max(0, max(t - 2 * lef, (t - lef) / 2));
	}
	return rig >= ds[m];
}

int main()
{
	cin >> n >> m;
	ds.resize(m + 1);

	int prev_x = 0;
	rep(i, m)
	{
		int x;
		cin >> x;

		ds[i] = x - prev_x - 1;
		prev_x = x;
	}
	ds[m] = n - prev_x;
	
	int lb = -1, ub = INF;
	while ( ub - lb > 1 ) {
		int const mid = lb + (ub - lb) / 2;
		(check(mid) ? ub : lb) = mid;
	}
	cout << ub << endl;
	return 0;
}
