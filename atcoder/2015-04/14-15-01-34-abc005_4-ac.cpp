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
#include <iostream>
#include <cstdio>
#include <ctime>
#include <climits>
#include <cassert>

using namespace std;
using ull = unsigned long long;
static unsigned long long const divisor = 1000000007ull;
static int const IntMax = std::numeric_limits<int>::max(), IntMin = std::numeric_limits<int>::min();
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
#define all(_X) (_X).begin(), (_X).end()
#define rall(_X) (_X).rbegin(), (_X).rend()
#define lmbc(_Prms, _Body) ([&] _Prms { return (_Body); })
#define lmbp(_Prms, _Body) ([ ] _Prms { return (_Body); })
#define mkp make_pair
#define pub push_back
template<typename T = int>std::vector<T> read_values(int n) { std::vector<T> v; v.reserve(n); T t; for(int i = 0; i < n; ++i) { std::cin >> t; v.push_back(t); } return std::move(v); }

vector<vector<int>> ds;
map<tuple<int,int,int,int>,int> dp;

void mymain()
{
	int n; cin >> n;
	
	//sums[i][j]: 格子点(i,j)より左上のマス全体を使うおいしさ
	int sums[51][51] {};
	rep(i, n) rep(j, n) {
		int d; cin >> d;
		sums[i+1][j+1] = sums[i][j+1] + sums[i+1][j] - sums[i][j] + d;
	}

	int q; cin >> q;
	auto ps = read_values(q);
	rep(i, q) {
		int p = ps[i];

		int m = 0;
		repi(w, 1, min(p, n) + 1) {
			int const h = min(n, p / w);

			rep(i, n - h + 1) rep(j, n - w + 1) {
				//位置i,jから大きさw,hの枠を使う
				m = max(m, sums[i + h][j + w] - sums[i + h][j] - sums[i][j + w] + sums[i][j]);
			}
		}
		cout << m << endl;
	}
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); std::exit(0); return 0; }
