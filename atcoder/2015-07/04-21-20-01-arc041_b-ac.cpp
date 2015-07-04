#include <cassert>
#include <functional>
#include <bitset>
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
# define all(_X) (_X).begin(), (_X).end()
#else
# define all(_X) begin(_X), end(_X)
#endif
#ifdef _LOCAL
# include "for_local.h"
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair

static int const dx[] = { 1, 0, -1, 0 }, dy[] = { 0, 1, 0, -1 };
static int const INF = 1<<29;

int n, m;
int b[500+3][500+3];
int a[500 + 3][500 + 3];

signed main() {
	scanf("%d %d", &n, &m);
	rep(i, n) {
		rep(j, m) {
			char c;
			scanf(" %c", &c);
			b[i][j] = c - '0';
		}
	}

	repi(i, 1, n - 1) repi(j, 1, m - 1) {
		int s = INF;
		rep(d, 4) {
			s = min(s, b[i + dy[d]][j + dx[d]]);
		}
		if ( s == 0 ) continue;

		a[i][j] += s;
		rep(d, 4) {
			b[i + dy[d]][j + dx[d]] -= s;
		}
	}

	rep(i, n) {
		rep(j, m) {
			cout << a[i][j];
		}
		cout << endl;
	}


	return 0;
}
