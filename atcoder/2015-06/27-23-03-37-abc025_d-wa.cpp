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


static ll const MOD = 1000000007;
static int const INF = 1<<29;
using pii = pair<int,int>;

array<int, 5*5> board;
array<pii, 5*3> cons[2]; //[0] 横制約, [1] 縦制約

bool monotone(array<int, 3> const& ns) {
	return (ns[0] < ns[1] && ns[1] < ns[2])
		|| (ns[2] < ns[1] && ns[1] < ns[0]);
}

bool verify(vector<int> const& perm) {
	array<int, 5*5> bs = board;
	int k = 0;
	rep(i, 5) rep(j, 5) {
		if ( bs[i * 5 + j] == 0 ) bs[i * 5 + j] = perm[k++];
	}

	array<int, 3> ns;
	rep(i, 3) rep(j, 5) {
		rep(l, 3) ns[l] = bs[(i + l) * 5 + j];
		if ( monotone(ns) ) return false;
	}
	rep(i, 5) rep(j, 3) {
		rep(l, 3) ns[l] = bs[i * 5 + (j + l)];
		if ( monotone(ns) ) return false;
	}
	return true;
}

signed main() {
	rep(i, 5) rep(j, 5) {
		cin >> board[i * 5 + j];
	}

	set<int> nums;
	rep(i, 25) {
		nums.insert(i + 1);
	}
	rep(i, 5 * 5) {
		nums.erase(board[i]);
	}

	if ( nums.size() < 9 ) {
		vector<int> perm(all(nums));
		int comb = 0;
		do {
			if ( verify(perm) ) comb++;
		} while ( next_permutation(all(perm)) );
		cout << comb << endl;
	}
	return 0;
}
