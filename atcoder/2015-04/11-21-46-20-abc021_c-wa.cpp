//#include "stdafx.h"
#include <random>
#include <memory>
#include <functional>
#include <algorithm>
#include <array>
#include <vector>
#include <map>
#include <set>
#include <string>
#include <iostream>
#include <cstdio>
#include <ctime>
#include <climits>
#include <cassert>

using namespace std;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (int _I = (_Init); _I < (_N); ++ _I)
#define rep(_I, _N) repi(_I, 0, _N)
#define all(_X) (_X).begin(), (_X).end()
#define rall(_X) (_X).rbegin(), (_X).rend()
#define lmbc(_Prms, _Body) ([&] _Prms { return (_Body); })
#define lmbp(_Prms, _Body) ([ ] _Prms { return (_Body); })
#define mkp make_pair
#define pub push_back
template<typename TInt>static std::function<TInt()> make_rand(TInt minval, TInt maxval) { return std::bind(std::uniform_int_distribution<TInt>(minval, maxval), std::mt19937(static_cast<ull>(std::time(0)))); }
template<typename T = int>std::vector<T> read_values(int n) { std::vector<T> v; v.reserve(n); T t; rep(i, n) { std::cin >> t; v.push_back(t); } return std::move(v); }

static auto const divisor = 1000000007ull;
template<ull Max = 30*30> ull combi(int n, int r) { if ( n < 0 || r < 0 ) { return 0; } if (!(n * r * (n - r))) { return 1; } assert(n <= Max && r <= Max); static ull t[Max][Max] {}; return t[n][r] ? t[n][r] : (t[n][r] = (t(n - 1, r) + t(n - 1, r - 1)) % divisor); }


void mymain()
{
	int n, a, b, m;
	cin >> n >> a >> b >> m;
	a--;b--;
	multimap<int, int> g;
	rep(i, m) {
		int x,y;
		cin >> x >> y; x--;y--;
		g.insert(mkp(x,y));
		g.insert(mkp(y,x));
	}

	struct verinf { bool reached; int minlen; int cnt; };
	vector<verinf> r;
	r.resize(n, verinf {false, numeric_limits<int>::max(), 0});
	r[a] = { true, 0, 1 };

	repi(d, 1, numeric_limits<int>::max()) {
		rep(i, n) {
			if ( r[i].minlen != d - 1 ) continue;
			
			auto ran = g.equal_range(i);
			for ( auto e = ran.first; e != ran.second; ++e ) {
				auto& dest = r[e->second];
				if ( dest.minlen > d ) {
					dest.minlen = d;
					dest.reached = true;
					//経路数はdestに至る長さd-1の最短経路数の総和
					dest.cnt = 0;
					auto ran2 = g.equal_range(e->second);
					for ( auto& e2 = ran2.first; e2 != ran2.second; ++e2 ) {
						dest.cnt += ( r[e2->second].minlen == (d - 1) ) ? r[e2->second].cnt : 0;
					}
				}
			}
		}
		if ( r[b].reached ) break;
	}
	
	cout << r[b].cnt << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
