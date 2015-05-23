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

bool dfs(int m, int s, int t) { //[0,m]日目の移動を使ってt→sに移動可能
	if ( s == t ) return true;
	if ( active(m, t) ) {
		if ( active(m, s) ) { //echo("day " << (m+1) << ": " << s << "→" << t);
		return true; }
		rep(i, m) {
			if ( dfs(i, s, t) ) return true;
			if ( ss[i].second >= ss[m].first ) {
				int mid = max(ss[i].first, ss[m].first);
			if ( active(m, mid) && dfs(i, s, mid) ) {
				//echo("day " << (m+1) << ": " << mid << "→" << t);
				return true;
			}
			}
		}
		return false;
	} else {
		return ( m > 0 ) && dfs(m - 1, s, t);
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

		rep(m, d) {
			if ( active(m, t) && dfs(m, s, t) ) {
				cout << (m + 1) << endl; break;
			}
		}
	}
}
