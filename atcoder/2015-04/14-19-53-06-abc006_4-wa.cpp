//http://abc006.contest.atcoder.jp/
#include <random>
#include <memory>
#include <functional>
#include <algorithm>
#include <deque>
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
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
using ull = unsigned long long;
static unsigned long long const divisor = 1000000007ull;
static int const IntMax = std::numeric_limits<int>::max(), IntMin = std::numeric_limits<int>::min();
#define all(_X) (_X).begin(), (_X).end()
#define rall(_X) (_X).rbegin(), (_X).rend()
#define lmbc(_Prms, _Body) ([&] _Prms { return (_Body); })
#define lmbp(_Prms, _Body) ([ ] _Prms { return (_Body); })
#define mkp make_pair
#define pub push_back
template<typename T = int>std::vector<T> read_values(int n) { std::vector<T> v; v.reserve(n); T t; for(int i = 0; i < n; ++i) { std::cin >> t; v.push_back(t); } return std::move(v); }

static int const nmax= 30000;


void mymain()
{
	int n; cin >> n;
	auto cs = read_values(n);
	struct uni_t{ int len; int lastval; };
	vector<uni_t> us;
	us.reserve(n);
	
	rep(i, n) {
		int const c = cs[i];
		uni_t* p = nullptr;
		for ( auto&& uni : us ) {
			if ( c >= uni.lastval ) { uni.len++; uni.lastval = c; p = &uni; }
		}
		if ( !p ) {
			us.pub({ 1, c });
		}
	}
	int maxlen = 0;
	for ( auto&& uni : us ) {
		if ( maxlen < uni.len) maxlen = uni.len;
	}
	cout << (n - maxlen) << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
