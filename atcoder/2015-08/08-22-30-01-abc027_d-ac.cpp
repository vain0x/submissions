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
string s;
vector<int> vals;
int cnt_moves;

int comp_abs(int l, int r) {
	return abs(l) - abs(r) < 0;
}

ll solve()
{
	vals.reserve(n);
	{
		int val = 0;
		rep(i2, n) { int i = n - 1 - i2;
			switch ( s[i] ) {
				case '+': ++val; break;
				case '-': --val; break;
				case 'M': vals.push_back(val); ++cnt_moves; break;
			}
		}
		sort(vals.begin(), vals.begin() + cnt_moves, comp_abs);
	}

	ll total_val = 0;
	int move_plus  = cnt_moves / 2;
	int move_minus = cnt_moves - move_plus;
	rep(i2, cnt_moves) { int i = cnt_moves - 1 - i2;
		if ( move_plus > 0 && vals[i] > 0 || move_minus == 0 ) {
			--move_plus;
			total_val += vals[i];
		} else {
			--move_minus;
			total_val -= vals[i];
		}
	}
	return total_val;
}

signed main()
{
	cin >> s;
	n = s.size();
	cout << solve() << endl;
	return 0;
}
