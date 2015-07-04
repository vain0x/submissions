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

int n, l;
int rabbits[100000+3];

//連続した同じ向きのうさぎごとに分割したもの
vector<pair<bool, vector<int>>> lines;
bool prevGoesLeft;

vector<pair<int,int>> sizes;

void addRabit(bool goesLeft, int x)
{
	if ( goesLeft != prevGoesLeft ) {
		lines.push_back({ goesLeft, { x } });
		prevGoesLeft = goesLeft;
	} else {
		lines.back().second.push_back(x);
	}
}

ll HasenVor(int idx)
{
	auto& ln = lines[idx].second;

	ll cnt = 0;
	int dest; //うさぎのジャンプ先

	if ( lines[idx].first ) {
		//go left

		if ( idx == 0 ) {
			dest = ln[0];
		} else {
			dest = lines[idx - 1].second.back() + 1;
		}

		for ( auto& r : ln ) {
			cnt += (r - dest);
			r = dest;
			++dest;
		}

	} else {
		//go right

		if ( idx == lines.size() - 1 ) {
			dest = ln.back();
		} else {
			dest = lines[idx + 1].second[0] - 1;
		}

		for ( int i = ln.size() - 1; i >= 0; --i ) {
			auto& r = ln[i];
			cnt += (dest - r);
			r = dest;
			--dest;
		}
	}
	return cnt;
}

signed main()
{

	//左番兵
	prevGoesLeft = false;
	addRabit(true, 0);

	cin >> n >> l;
	rep(i, n) {
		int x;//1-indexed
		string s;
		cin >> x >> s;
		bool goesLeft = (s == "L");
		addRabit(goesLeft, x );
	}

	//右番兵
	addRabit(false, l + 1);

	//うさぎ隊列を長いものから動かしていく
	rep(i, lines.size()) {
		sizes.emplace_back(lines[i].second.size(), i);
	}
	sort(all(sizes));

	ll cnt = 0;
	for ( int idx = sizes.size() - 1; idx >= 0; --idx ) {
		cnt += HasenVor(sizes[idx].second);
	}

	cout << cnt << endl;

	return 0;
}
