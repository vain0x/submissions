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

ull figureLength(ull x) { 
	return (x == 0 ? 0 : 1 + ull(log(static_cast<long double>(x)) / log(10)));
}
static int const cntLegals[11] = { 0, 1, 2, 3, 4, 4, 5, 6, 7, 8, 8 };
ull cntIllegals(int fl, ull x) {
	//xと同じ桁数で、x未満である違法数の個数
	//ただしx=10^nのときfl=n
	ull cnt = 0;
	repi(i, 1, fl+1) {
		int z = (i == fl ? 1 : 0);

		ull mask = pow<ull>(10,i);
		int n = (x % mask) * 10 / mask;
		if ( n == 0 && z == 1 ) n = 10;
		cnt += (n - z) * pow<ull>(10, i-1)
			- ((cntLegals[n] - z) * pow<ull>(10-2, i-1));
	}
	return cnt;
}

//http://abc007.contest.atcoder.jp/
void mymain()
{
	ull a, b; cin >> a >> b;
	int a_fl = figureLength(a), b_fl = figureLength(b);
	ull cnt = 0;
	repi(i, a_fl, b_fl) {
		cnt += cntIllegals(i, pow<ull>(10,i));
	}
	cnt += cntIllegals(b_fl, b + 1);
	cnt -= cntIllegals(a_fl, a);
	cout << cnt << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
