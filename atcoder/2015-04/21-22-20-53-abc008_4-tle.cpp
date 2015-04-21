#include <random>
#include <utility>
#include <memory>
#include <functional>
#include <algorithm>
#include <deque>
#include <queue>
#include <array>
#include <vector>
#include <map>
#include <set>
#include <string>
#include <cstdio>
#include <ctime>
#include <climits>
#include <cassert>
#include <iostream>
using namespace std;
using ull = unsigned long long;
static unsigned long long const divisor = 1000000007ull;
static int const IntMax = std::numeric_limits<int>::max(), IntMin = std::numeric_limits<int>::min();
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
#define rrep(_I, _N) for(int _I = (_N) - 1; (_I) >= 0; --(_I))
#define all(_X) (_X).begin(), (_X).end()
#define rall(_X) (_X).rbegin(), (_X).rend()
#define lmbc(_Prms, _Body) ([&] _Prms { return (_Body); })
#define lmbp(_Prms, _Body) ([ ] _Prms { return (_Body); })
#define mkp make_pair
#define mkt make_tuple
#define pub push_back
template<typename T = int>std::vector<T> read_values(int n) { std::vector<T> v; v.reserve(n); T t; for ( int i = 0; i < n; ++i ) { std::cin >> t; v.push_back(t); } return std::move(v); }
template<typename TInt>static std::function<TInt()>
	make_rand(TInt minval, TInt maxval) { return std::bind(std::uniform_int_distribution<TInt>(minval, maxval), std::mt19937(static_cast<ull>(std::time(0)))); }

#ifdef _LOCAL
# include "for_local.h"
#else
# define echo(...) ((void)0)
#endif

static const int dx[4] = { 1, 0, -1, 0 }, dy[4] = { 0, 1, 0, -1 };

int n; //<= 30
int mx[30], my[30]; //装置の位置

ull calc(long ms, int x1, int y1, int x2, int y2) {
	if ( ms == 0 || x2 - x1 <= 0 || y2 - y1 <= 0 ) return 0;
	ull cnt = 0;
	ull cnt_base = (x2 - x1) + (y2 - y1) - 1;

	//invoke #k
	rep(k, n) {
		if ( (ms & (1<<k)) == 0 ) continue;
		int kx = mx[k], ky = my[k];
		if (!(x1 <= kx && kx < x2 && y1 <= ky && ky < y2)) continue;
		if ( ms == (1<<k) ) return cnt_base;

		array<ull, 4> cnts;

		long div[4] {};
		rep(i, n) {
			if ( i == k || ((ms & (1<<i)) == 0) ) continue;

			int ix = mx[i], iy = my[i];
			int j =
				+ (ix < kx ? 1 : 0)
				+ (iy < ky ? 2 : 0);
			div[j] |= 1 << i;
		}

		rep(j, 4) {
			cnts[j] = calc(div[j],
				((j & 1) != 0 ? x1 : kx + 1), ((j & 2) != 0 ? y1 : ky + 1),
				((j & 1) != 0 ? kx : x2),     ((j & 2) != 0 ? ky : y2));
			//echo("div " << j << ": " << cnts[j]);
		}

		ull const cnt1 = cnt_base + cnts[0] + cnts[1] + cnts[2] + cnts[3];
		cnt = max(cnt, cnt1);
	}
	//echo("calc " << mkt(ms,x1,x2,y1,y2) << "-> " << cnt);
	return cnt;
}

//abc008.contest.atcoder.jp/tasks/abc008_4
void mymain()
{
	int w, h; //<= 10^6
	cin >> w >> h >> n;
	rep(i, n) {
		int x, y; cin >> x >> y;
		x--; y--;
		mx[i] = x; my[i] = y;
	}

	ull m = calc((1 << n) - 1, 0, 0, w, h);
	cout << m << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
