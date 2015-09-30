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

int n, k;
vector<vector<int>> t;

bool dfs(int i, int v)
{
	if ( i == n ) return (v == 0);

	rep(j, k)
	{
		if ( dfs(i + 1, v ^ t[i][j]) ) return true;
	}
	return false;
}

int main()
{
	cin >> n >> k;
	t.resize(n, vector<int>(k));
	rep(i, n) rep(j, k)
	{
		cin >> t[i][j];
	}

	cout << (dfs(0, 0) ? "Found" : "Nothing") << endl;
	return 0;
}
