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
static double const EPS = 1e-9;
int a, b, c;

double f(double t)
{
	return a * t + b * sin(c * t * PI) - 100;
}

double binsearch(double lb, double ub, double const y)
{
	rep(_, 300)
	{
		double const mid = lb + (ub - lb) / 2;
		double const f_m = f(mid);
		( y * f_m > 0 ? lb : ub) = mid;
	}
	return lb;
}

double solve()
{
	int const n = 100000;
	double const w = (1e9 - 0) / n;
	rep(i, n)
	{
		double const lb = w * i,
			ub = lb + w;
		double const f_l = f(lb),
			f_u = f(lb + w);

		if ( f_l * f_u < 0 ) {
			return binsearch(lb, ub, f_l);
		}
	}
	return 0;
}

signed main()
{
	scanf("%d %d %d", &a, &b, &c);
	printf("%.16f", solve());
	return 0;
}
