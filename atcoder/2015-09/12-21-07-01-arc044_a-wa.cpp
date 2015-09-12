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

bool is_prime(int n)
{
	if ( n == 1 ) return false;
	int i = 2;
	for ( ; i * i <= n; ++i ) {
		if ( (n % i) == 0 ) return false;
	}
	return i > 1;
}

bool solve(int n)
{
	if ( is_prime(n) ) return true;
	if ( n > 1 && (n & 1) != 0 && (n & 1) != 5 ) {
		int sum = 0;
		for ( int m = n; m != 0; ) {
			sum += m % 10;
			m /= 10;
		}
		if ( sum % 3 != 0 ) {
			return true;
		}
	}
	return false;
}

signed main()
{
	int n;
	cin >> n;
	cout << (solve(n) ? "Prime" : "Not Prime") << endl;
	return 0;
}
