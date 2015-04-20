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

#ifdef _LOCAL
# include "module/for_local.h"
#else
# define echo(...) ((void)0)
#endif

double combif(int n, int r) {
	if ( !(0 <= r && r <= n) ) return 0.0;
	if ( r * 2 > n ) { return combif(n, n - r); }

	static double mm[100 + 1][(100 + 1) / 2] {};
	if ( mm[n][r] != 0 ) { return mm[n][r]; }
	double c = 1;
	rep(i, r) { c = c * (n - i) / (r - i); }
	return (mm[n][r] = c);
}

int n;
vector<int> cs;

int cntDivs(int i) {
	int const c = cs[i];
	int divs = 0;
	rep(j, n) {
		if ( i == j ) continue;
		if ( c % cs[j] == 0 ) { divs++; }
	}
	return divs;
}

//abc008.contest.atcoder.jp/tasks/abc008_3
void mymain()
{
	cin >> n;
	cs = read_values(n);
	
	double ex = 0;
	rep(i, n) {
		int divs = cntDivs(i);

		rep(j, n) {
			double p = 0; //cs[i]が左からj番目に配置されたときに表である確率 (j=0のとき1)
			double c_n_j = combif(n - 1, j);
			
			rep(k0, n) {
				int k = k0 * 2;
				if ( !(k <= j) ) break;

				//cs[i]の左側にあるj個の数のうち、ちょうどk個が約数である確率
				// combi(divs, k) * combi(n-1 - divs, j - k) / combi(n-1, j)
				double p0 = combif(divs, k) * combif((n - 1) - divs, j - k) / c_n_j;
				//echo( mkt(cs[i], i, j,  p0) );
				p += p0;
			}

			ex += p / n; //左からj番目に配置される確率
		}
	}

	cout << ex << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
