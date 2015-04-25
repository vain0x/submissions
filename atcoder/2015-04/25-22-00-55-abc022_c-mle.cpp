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
#define mkp make_pair
#define mkt make_tuple
#define pub push_back
template<typename T = int>std::vector<T> read_values(int n) { std::vector<T> v; v.reserve(n); T t; for ( int i = 0; i < n; ++i ) { std::cin >> t; v.push_back(t); } return std::move(v); }

#ifdef _LOCAL
# include "for_local.h"
#else
# define echo(...) ((void)0)
#endif

int edge_code(int u, int v) { return max(u,v)  | (min(u, v) << 16); }

struct info_t {
	int v; ull len;
	set<int> trail;
};

struct mygreater {
	bool operator()(info_t const& lhs, info_t const& rhs) {
		return ((lhs.len) > (rhs.len));
	}
};

multimap<int,pair<int,ull>> gs;
priority_queue<info_t, vector<info_t>, mygreater> qs;

void put(int u, int v, ull len) {
	gs.emplace(u, mkp(v, len));
	gs.emplace(v, mkp(u, len));
}

void mymain()
{
	int n, m;
	cin >> n >> m;
	rep(i, m) {
		int u, v, len;
		cin >> u >> v >> len;
		put(u, v, len);
	}
	ull len = 0;

	qs.push({ 1, 0, {} });
	while ( !qs.empty() ) {
		auto x = qs.top();
		qs.pop();
		
		if ( x.v == 1 && x.len > 0 ) {
			len =  x.len;
			break;
			//if ( len == 0 || x.len < len ) len = x.len;
			//continue;
		}

		auto ran = gs.equal_range(x.v);
		for ( auto it = ran.first; it != ran.second; ++it ) {
			auto& e = it->second;
			int v2 = e.first;
			int h = edge_code(x.v, v2);

			if ( x.trail.count(h) == 0 ) {
				auto t2 = x.trail;
				t2.insert(h);

				qs.push({v2, x.len + e.second, t2});
			}
		}
	}

	if ( len == 0 ) {
		cout << -1 << endl;
	} else {
		cout << len << endl;
	}
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
