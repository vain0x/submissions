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
#define rrep(_I, _N) for (int _I = (_N) - 1; (_I) >= 0; --(_I))
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

ull figureLength(ull x) { 
	assert(x > 0);
	return 1 + ull(log<ull>(x) / log(10));
}
int figureAt(ull x, int idx) {
	while ( idx > 0 ) { idx --; x /= 10; }
	return x % 10;
}
static int const cntLegals[11] = { 0, 1, 2, 3, 4, 4, 5, 6, 7, 8, 8 };
ull cntIllegals(int fl, ull x) {
	//xと同じ桁数で、x未満である違法数の個数
	//ただし x = 10^n のとき fl = n
	ull cnt = 0;
	bool ill = false;
	rrep(i, fl) {
		int z = (i == fl - 1 ? 1 : 0);
		int n = figureAt(x, i);
		if ( n == 0 && z == 1 ) n = 10;
		cnt += (n - z) * pow<ull>(10, i)
			- (ill ? 0 : ((cntLegals[n] - z) * pow<ull>(10-2, i)));
		if ( n == 4 || n == 9 ) { ill = true; }
	}
	return cnt;
}

//http://abc007.contest.atcoder.jp/
void mymain()
{
	ull a, b; cin >> a >> b;
	int const a_fl = figureLength(a),
		b_fl = figureLength(b);

	ull cnt = 0;
	repi(i, a_fl + 1, b_fl) {
		cnt += cntIllegals(i, pow<ull>(10, i));
	}
	if ( a_fl == b_fl ) {
		cnt += cntIllegals(b_fl, b + 1) - cntIllegals(a_fl, a);
	} else {
		cnt += cntIllegals(a_fl, pow<ull>(10, a_fl)) - cntIllegals(a_fl, a);
		cnt += cntIllegals(b_fl, b + 1);
	}
	cout << cnt << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
