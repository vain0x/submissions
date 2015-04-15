#include <random>
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

struct { bool valids; int d; } maze[51][51];

static int const dx[4] = { 1, 0, -1, 0 }, dy[4] = { 0, 1, 0, -1 };

//http://abc007.contest.atcoder.jp/
void mymain()
{
	int r, c, sy, sx, gy, gx;
	cin >> r >> c >> sy >> sx >> gy >> gx;
	sy --; sx --; gy --; gx --;
	rep(i, r) rep(j, c) {
		char ch;
		cin >> ch;
		maze[i][j] = { (ch == '.'), IntMax };
	}

	queue<pair<int,int>> q;
	q.push(mkp(sy, sx));
	maze[sy][sx].d = 0;
	for (;;) {
		auto it = q.front();
		rep(k, 4) {
			int i = it.first + dy[k];
			int j = it.second + dx[k];
			auto& m = maze[i][j];
			if ( !m.valids ) continue;
			if ( m.d != IntMax ) continue;
			
			m.d = maze[it.first][it.second].d + 1;
			if ( i == gy && j == gx ) {
				cout << m.d << endl;
				return;
			}
			q.push(mkp(i, j));
		}
		q.pop();
	}

}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
