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

double p_fact(vector<int> ks)
{
	double p = 1;
	int k = 1;
	for ( int i = 0; i < ks.size(); ++ i )
	{
		for ( int j = 1; j <= ks[i]; ++j ) {
			p = (p * k) / j;
			++k;
		}
	}
	return p;
}

int n, k;

signed main()
{
	cin >> n >> k;
	double q =
		+ p_fact({ 3, 0, 0 }) * 1 * 1 * 1
		+ p_fact({ 2, 1, 0 }) * 1 * 1 * (n - 1)
		+ p_fact({ 1, 1, 1 }) * 1 * (k - 1) * (n - k)
		;
	double p = q / powf(n, 3);
	
	printf("%.16f\n", p);
	return 0;
}