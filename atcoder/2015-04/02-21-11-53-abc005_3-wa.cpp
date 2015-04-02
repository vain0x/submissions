//abc005.contest.atcoder.jp/tasks/abc005_3
#include <iostream>
#include <climits>
#include <cstdio>
#include <ctime>
#include <cassert>
#include <vector>
#include <array>
#include <map>
#include <set>
#include <string>
#include <algorithm>
#include <random>
#include <functional>

using namespace std;
using ull = unsigned long long;
#define _cin_charm_ std::ios::sync_with_stdio(false); std::cin.tie(0);
#define repi(_I, _Init, _N) for (int _I = (_Init); _I < (_N); ++ _I)
#define rep(_I, _N) repi(_I, 0, _N)
#define all(_X) (_X).begin(), (_X).end()
#define rall(_X) (_X).rbegin(), (_X).rend()

#define mkp make_pair
#define pub push_back
template<typename T = int>
static std::vector<T> read_values(int n) {
	std::vector<T> v; v.reserve(n);
	T tmp;
	rep(i, n) { cin >> tmp; v.pub(tmp); }
	return std::move(v);
}

int main()
{
	_cin_charm_;

	int t, n;
	cin >> t >> n;
	auto a = read_values(n);
	int m; cin >> m;
	auto b = read_values(m);
	
	bool result = true;
	int k = 0;
	rep(i, m) {
		int const s = b[i];
		int j = k;
		for (; j < n; ++j ) {
			if ( s - a[j] <= t ) {
				k = j + 1; break;
			}
		}
		if ( j == n ) { result = false; break; }
	}

	cout << (result ? "yes" : "no") << endl;
}