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

int r, c, k;

ll solve_naive()
{
	vector<string> board;
	board.resize(r);
	rep(i, r)
	{
		cin >> board[i];
	}

	auto&& check = [&](int i0, int j0) {
		repi(di, -k, k) repi(dj, -k, k)
		{
			int i = i0 + di;
			int j = j0 + dj;
			if ( !(abs(di) + abs(dj) <= k - 1) ) continue;
			if ( !(0 <= i && i < r && 0 <= j && j < c)
				|| board[i][j] == 'x' ) return false;
		}
		return true;
	};

	ll cnt = 0;
	rep(i, r) rep(j, c)
	{
		if ( check(i, j) ) {
			echo(mkt(i, (j - (k - 1))));
			cnt++;
		}
	}
	return cnt;
}

int main()
{
	cin >> r >> c >> k;
	if ( !(r <=50 && c <= 50) ) abort();

	cout << solve_naive() << endl;
	return 0;
}
