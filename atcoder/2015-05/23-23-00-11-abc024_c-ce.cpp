#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
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
typedef unsigned int uint; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()
int main() { void mymain(); mymain(); return 0; }

int n, d, k;
vector<pair<int,int>> ss;

bool active(int i, int s) {
	return ss[i].first <= s && s <= ss[i].second;
}

using pii= pair<int,int>;

int solve(int s, int t) {
	pii p = mkp(s, s);
	rep(i, d) {
		auto&& q = ss[i];
		if ( !(p.second < q.first || q.second < p.first) ) {
			p.first = min(p.first, q.first);
			p.second = max(p.second, q.second);
		}
		echo(p);
		if ( p.first <= t && t <= p.second ) return i + 1;
	}
}

void mymain() {
	cin >> n >> d >> k;
	rep(i, d) {
		int l, r; cin >> l >> r;
		ss.push_back({l, r});
	}
	rep(_, k) {
		int s, t;
		cin >> s >> t;
		cout << solve(s, t) << endl;
	}
}
