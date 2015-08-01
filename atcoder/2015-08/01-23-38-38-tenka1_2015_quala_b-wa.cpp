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

inline int readTime()
{
	int hh, mm, ss, mmm;
	scanf("%d:%d:%d.%d", &hh, &mm, &ss, &mmm);
	return (((hh - 21) * 60 + mm) * 60 + ss) * 1000 + mmm;
}

vector<int> solve1(int n, vector<int> ss, vector<int> es)
{
	int can_l = -1, can_r = 1LL << 29;
	rep(i, n)
	{
		if ( ss[i] > es[i] ) {
			can_l = max(can_l, ss[i]);
			can_r = min(can_r, es[i] + 1000);
		}
	}

	vector<int> res;
	bool uniq = (can_l <= can_r);
	rep(i, n)
	{
		if ( !uniq ) break;
		if ( ss[i] > es[i] ) {
			res.push_back(1000 + es[i] - ss[i]);
		} else if (
			can_l < ss[i] && ss[i] < can_r
			|| ss[i] <= can_l && can_l < es[i] + 1000 && es[i] < can_r
			) {
			res.push_back(-1);
		} else if ( ss[i] <= can_l && can_r <= es[i] ) {
			res.push_back(es[i] - ss[i] + 1000) ;
		} else {
			res.push_back(es[i] - ss[i]) ;
		}
	}
	return std::move(res);
}

signed main()
{ 
	int n;
	cin >> n;

	vector<int> ss(n), es(n);
	rep(i, n)
	{
		ss[i] = readTime();
		es[i] = readTime();
	}
	auto&& res = solve1(n, move(ss), move(es));
	for ( int span : res ) {
		cout << span << endl;
	}
	return 0;
}
