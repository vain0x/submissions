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

int n;
vector<int> as;
int ave;

int solve()
{
	if ( ave % n != 0 ) {
		return -1;
	}
	ave /= n;

	int kav = 0;
	int kn = 0;

	bool ok = false;
	int cnt = 0;
	rep(i, n)
	{
		kav += as[i];
		++kn;
		if ( kav % kn != 0 ) continue;
		if ( kav / kn == ave ) {
			kav = kn = 0;
			++cnt;
			if ( i == n - 1 ) {
				ok = true;
			}
		}
	}
	return (ok ? n - cnt : -1);
}

signed main()
{
	cin >> n;
	as.resize(n);
	rep(i, n)
	{
		cin >> as[i];
		ave += as[i];
	}
	cout << solve() << endl;
	return 0;
}
