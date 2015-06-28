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
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()
inline int scani() { int n; scanf("%d", &n); return n; }

using pii = pair<int,int>;
int const O = 1, X = 2;

int bs[3][3];
int cs[3][3];

pii dfs(array<int, 3 * 3>& board, int mark) {
	pii res { 0, 0 };
	bool filled = true;
	int score_ext;

	rep(i, 3) rep(j, 3) {
		if ( board[i * 3 + j] != 0 ) continue;

		board[i * 3 + j] = mark;
		pii&& p = dfs(board, O + X - mark);
		board[i * 3 + j] = 0;

		int const score = p.first - p.second;
		if ( filled
			|| (score - score_ext) * (mark == O ? 1 : -1) > 0 ) {
			score_ext = score;
			res = p;
			filled = false;
		}
	}

	if ( !filled ) {
		return res;

	} else {
		pii p { 0, 0 };
		rep(i, 3) rep(j, 3) {
			if ( i + 1 < 3 ) {
				(board[i * 3 + j] == board[(i + 1) * 3 + j] ? p.first : p.second)
					+= bs[i][j];
			}
			if ( j + 1 < 3 ) {
				(board[i * 3 + j] == board[i * 3 + (j + 1)] ? p.first : p.second)
					+= cs[i][j];
			}
		}
		return p;
	}
}


signed main() {
	rep(i, 2) rep(j, 3) {
		cin >> bs[i][j];
	}
	rep(i, 3) rep(j, 2) {
		cin >> cs[i][j];
	}

	array<int, 3 * 3> board;
	fill(all(board), 0);
	auto&& p = dfs(board, O);

	cout << p.first << endl
		<< p.second << endl;

	return 0;
}
