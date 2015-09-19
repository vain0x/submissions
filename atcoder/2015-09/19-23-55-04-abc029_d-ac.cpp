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

string s;
ll n;

int const max_digits = 11;
ll memo[max_digits + 1][2][10];

ll solve(int i, bool less_than, int cnt)
{
	ll& r = memo[i][less_than][cnt];
	if ( r != -1 ) return r;
	if ( i == max_digits ) return (r = cnt);

	int const k = s[i] - '0';
	r = 0;
	rep(d, 10)
	{
		if ( !(less_than || d <= k) ) continue;
		r += solve(i + 1
			, less_than || d < k
			, cnt + (d == 1)
			);
	}
	return r;
}

signed main()
{
	cin >> n;
	s = to_string(n);
	s = string(max_digits - s.size(), '0') + s; //桁数あわせ
	memset(memo, -1, sizeof(memo));
	cout << solve(0, false, 0) << endl;
	return 0;
}
