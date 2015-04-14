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

//http://abc006.contest.atcoder.jp/
//http://www.slideshare.net/chokudai/abc006
//満点解法の1つ
void mymain()
{
	int n; cin >> n;
	auto cs = read_values(n);

	vector<int> is; //[i]: 長さiの増加部分列の末尾カードの最小値
	is.reserve(n + 2);
	is.pub(IntMax);

	rep(i, n) {
		int const c = cs[i];

		auto it = lower_bound(all(is), c); //cより大きい最初の要素
		//cを連結できる最長の部分列の最後のカードがit[-1]、それより1長い部分列の末尾が高々cであるということ
		if ( it == is.end() ) {
			is.pub(c);
		} else {
			if ( c < *it ) *it = c;
		}
	}

	cout << (n - is.size()) << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
