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

static int const INF = 1 << 29;

int n;
vector<int> b[2];

pair<int, int> score(int i, int j)
{
	if ( i > j ) swap(i, j);
	++j;

	int se = b[0][(j + 1) / 2] - b[0][(i + 1) / 2];
	int so = b[1][(j + 0) / 2] - b[1][(i + 0) / 2];
	return (i % 2 == 0
		? make_pair(se, so)
		: make_pair(so, se));
}

int aoki(int it)
{
	int ma = -INF;
	int ia = 0;

	rep(i, n)
	{
		if ( i == it ) continue;

		int s = score(i, it).second;
		if ( ma < s ) {
			ma = s; ia = i;
		}
		echo("aoki:" << mkt(i, it) << "->" << s);
	}
	return ia;
}

int takahashi()
{
	int ma = -INF;
	int it = 0;
	rep(i, n)
	{
		int ia = aoki(i);
		int s = score(i, ia).first;
		if ( ma < s ) {
			ma = s; it = i;
		}
		echo("takahashi:" << mkt(i, ia) << "->" << s);
	}
	return ma;
}

int main()
{
	cin >> n;
	int k = 0;

	b[0].push_back(0);
	b[1].push_back(0);
	rep(i, n)
	{
		int a;
		cin >> a;
		b[k].push_back(a + (b[k].empty() ? 0 : b[k].back()));
		k = 1 - k;
	}

	cout << takahashi() << endl;
	return 0;
}
