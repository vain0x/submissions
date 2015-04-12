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
static unsigned long long const divisor = 1000000007ull;
static int const IntMax = std::numeric_limits<int>::max(), IntMin = std::numeric_limits<int>::min();
#define repi(_I, _Init, _N) for (int _I = (_Init); _I < (_N); ++ _I)
#define rep(_I, _N) repi(_I, 0, _N)
#define all(_X) (_X).begin(), (_X).end()
#define rall(_X) (_X).rbegin(), (_X).rend()
#define lmbc(_Prms, _Body) ([&] _Prms { return (_Body); })
#define lmbp(_Prms, _Body) ([ ] _Prms { return (_Body); })
#define mkp make_pair
#define pub push_back
template<typename T = int>std::vector<T> read_values(int n) { std::vector<T> v; v.reserve(n); T t; for(int i = 0; i < n; ++i) { std::cin >> t; v.push_back(t); } return std::move(v); }

vector<vector<int>> d;
map<tuple<int,int,int,int>,int> dp;

int calc(int px, int py, int sx, int sy) {
	if ( sx == 1 && sy == 1 ) {return d[px][py];}
	auto& r = dp[make_tuple(px, py, sx, sy)];
	if ( r ) return r;

	if ( sx > 1 ) {
		int s = sx/2;
		return r = calc(px, py, s, sy) + calc(px + s, py, sx - s, sy);
	} else if ( sy > 1 ) {
		int s = sy/2;
		return r = calc(px, py, sx, s) + calc(px, py + s, sx, sy - s);
	}
}

void mymain()
{
	int n;
	cin >> n;
	d.reserve(n);
	rep(i, n) {
		d.pub(read_values(n));
	}
	
	int q; cin >> q;
	auto ps = read_values(q);

	rep(s, q) {
		int const p = ps[s];

		//round robin...terrible!
		int max = 0;
		repi(sx, 1, n+1) {
			repi(sy, 1, n+1) {
				if ( sx * sy > p ) continue;
				rep(px, n - sx + 1) {
					rep(py, n - sy + 1) {
						int const sum = calc(px, py, sx, sy);
						if ( sum > max ) max = sum;
					}
				}
			}
		}
		cout << max << endl;
	}
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
