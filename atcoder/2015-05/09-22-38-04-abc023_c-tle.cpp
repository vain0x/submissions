#include <random>
#include <memory>
#include <functional>
#include <algorithm>
#include <array>
#include <vector>
#include <queue>
#include <map>
#include <set>
#include <string>
#include <iostream>
#include <cstdio>
#include <ctime>
#include <cmath>
#include <climits>
#include <cassert>
#include <bitset>

#ifdef _LOCAL
# include "for_local.h"
#else
# define echo(...) ((void)0)
#endif

using namespace std;
using uint = unsigned int;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (unsigned int _I = (_Init); _I < (_N); ++ (_I))
#define rep(_I, _N) for(unsigned int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) begin(_X), end(_X)
#define rall(_X) rbegin(_X), rend(_X)
#define mkp make_pair
#define mkt make_tuple
#define empb emplace_back
int main() { void mymain(); std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }



int r, c,  n;
long long k;
set<pair<uint, uint>> candies;
vector<pair<uint, uint>> i_cnts;
vector<pair<uint, uint>> j_cnts;
vector<uint> ics;

auto less_first = [](pair<uint, uint> const& x, pair<uint, uint> const& y) {
		return x.first < y.first;
	};


void mymain() {

	cin >> r >> c >> k >> n;

	i_cnts.resize(r);
	j_cnts.resize(c);
	rep(p, n) {
		int i, j;
		cin >> i >> j; --i; --j;

		candies.emplace(mkp(i,j));
		i_cnts[i].first ++;
		j_cnts[j].first ++;
	}

	rep(i, r) { i_cnts[i].second = i; }
	//rep(j, c) { j_cnts[j].second = j; }

	sort(all(i_cnts), less_first);
	//sort(all(j_cnts), less_first);

	ics.resize(r);
	rep(i, r) {
		ics[i] = (i_cnts[i].first);
	}

	ull cnt = 0;
	rep(j, c) {
		uint const jc = j_cnts[j].first;
		
		auto lb = lower_bound(all(ics), k - jc);
		int i = distance(ics.begin(), lb);
		for (; i < r; ++ i) {
			uint const ic = ics[i];
			bool ex = candies.count(mkp(i_cnts[i].second, j)) != 0;

			if ( ic + jc == k ) {
				if ( !ex ) cnt ++;
			} else if ( ic + jc == k + 1 ) {
				if ( ex ) cnt++;
			} else break;
		}
	}

	cout << (cnt)
		<< endl;
}