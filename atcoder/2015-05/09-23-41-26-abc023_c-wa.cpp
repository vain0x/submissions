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


int r, c, n;
long long k;
vector<pair<uint, uint>> candies;
vector<uint> ics, jcs; //[i]:(行i、列i)の飴の個数
vector<uint> cis, cjs; //[i]:飴がi個ある(行、列)の個数

void mymain() {

	cin >> r >> c >> k >> n;

	candies.resize(n);
	ics.resize(r);
	jcs.resize(c);
	rep(p, n) {
		int i, j;
		cin >> i >> j; --i; --j;

		candies[p] = mkp(i, j);
		ics[i] ++;
		jcs[j] ++;
	}

	//起点マスの飴に関する補正
	int cnt1 = 0;
	rep(i, n) {
		int p = ics[candies[i].first] + jcs[candies[i].second];
		if ( p == k ) cnt1 --;
		else if ( p == k + 1 ) cnt1 ++;
	}

	//以降起点マスの飴は重複して数えてよい
	cis.resize(n + 1);
	cjs.resize(n + 1);
	rep(i, r) { cis[ics[i]] ++; }
	rep(j, c) { cjs[jcs[j]] ++; }

	int cnt2 = 0;
	rep(p, k+1) {
		cnt2 += cis[p] * cjs[k - p];
	}

	cout << (cnt2 + cnt1) << endl;
}
