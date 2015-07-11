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
#ifndef ONLINE_JUDGE //POJ
# include <complex>
# include <random>
# include <array>
# define mkt make_tuple
# define empb emplace_back
#endif
#ifdef _LOCAL
# include "for_local.h"
#else
# define debug if (false)
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()

static double const PI = 3.14159265358979323;

signed main()
{
	int n;
	scanf("%d", &n);

	vector<int> rs(n);
	rep(i, n)
	{
		scanf("%d", &rs[i]);
	}

	sort(all(rs));

	double total = 0;
	rep(i, n)
	{

		total += ((n - i) & 1 ? 1 : -1) * rs[i] * rs[i] * PI;
	}

	printf("%.7f", total);

	return 0;
}
