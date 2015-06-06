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

int n, q;
int par[100000 + 5];
void init() {
	rep(i, n) par[i] = i;
}
int root(int i) {
	return ( par[i] == i ) ? i
		: (par[i] = root(par[i]));
}
void unite(int i, int j) {
	i = root(i);
	j = root(j);
	if ( i != j ) {
		par[i] = j;
	}
}
bool same(int i, int j) {
	return root(i) == root(j);
}

signed main() {
	cin >> n >> q;
	init();
	rep(i, q) {
		int p, a, b;
		cin >> p >> a >> b;
		if ( p == 0 ) {
			unite(a, b);
		} else {
			printf("%s\n", (same(a, b) ? "Yes" : "No"));
		}
	}
	return 0;
}
