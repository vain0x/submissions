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

ll parse(string s)
{
	ll r = 0;
	r += (atoi(s.substr(0, 2).c_str()) - 21) * 3600;
	r += (atoi(s.substr(3, 2).c_str())) * 60;
	r += atoi(s.substr(6, 2).c_str());
	r *= 1000;
	r += atoi(s.substr(9, 3).c_str());
	return r;
}

signed main()
{
	ll can_l = -1, can_r = 1LL << 50;

	int n;
	cin >> n;

	vector<ll> ss(n), es(n);
	rep(i, n)
	{
		string s, t;
		cin >> s >> t;

		ll l = parse(s);
		ll r = parse(t);
		ss[i] = l; es[i] = r;

		if ( l > r ) {
			r += 1000;
			can_l = max(can_l, l);
			can_r = min(can_r, r);
		}
	}

	vector<ll> intervals;
	bool uniq = (can_l <= can_r);
	rep(i, n)
	{
		if ( !uniq ) break;
		if ( es[i] <= can_r ) {
			if ( can_l - 1000 <= es[i] ) {
				uniq = false;
			} else {
				intervals.emplace_back(es[i] - ss[i]);
			}
		} else if ( ss[i] > es[i] ) {
			intervals.push_back(1000 + es[i] - ss[i]);
		} else if ( ss[i] <= can_l && can_r <= es[i] ) {
			intervals.emplace_back(es[i] - ss[i] + 1000);
		} else {
			intervals.emplace_back(es[i] - ss[i]);
		}
	}

	if ( uniq ) {
		rep(i, n)
		{
			cout << intervals[i] << endl;
		}
	} else {
		cout << "-1" << endl;
	}

	return 0;
}
