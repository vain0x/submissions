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
template<typename T = int>std::vector<T> read_values(int n) { std::vector<T> v; v.reserve(n); T t; for(int i = 0; i < n; ++i) { std::cin >> t; v.push_back(t); } return std::move(v); }

static auto const divisor = 1000000007ull;

template<ull P>
struct ResidueField {//static_assert(P: primal);
	static ull pow(ull x, int n) {
		x %= P;
		ull r = 1;
		ull p = x;
		while (n != 0) {
			if ( n & 1 ) {
				r = (r * p) % P; n &= ~1;
			} else {
				p = (p * p) % P; n >>= 1;
			}
		}
		return r;
	}
	static ull inverse(ull x) { return pow(x, P - 2); }

	template<ull Max>
	static ull fact(int n) {
		static array<ull, Max> t = { 1, 1 };
		for ( int i = 2; i <= n; ++i ) { if ( t[i] == 0 ) { t[i] = i * t[i - 1] % P; } }
		return t[n];
	}
	template<ull Max>
	static ull fact_inv(int n) {
		static array<ull, Max> t = { 1, 1 };
		if ( t[n] == 0 ) {
			t[n] = inverse(fact<Max>(n));
			for ( int i = n - 1; i >= 2; --i ) { t[i] = (t[i + 1] * (i + 1)) % P; }
		}
		return t[n];
	}
	template<ull Max>
	static ull combi(int n, int r) {
		if ( n < r ) { return 0; }
		if ( r == 0 ) { return 1; }
		return (fact<Max>(n) * fact_inv<Max>(n - r) % P) * fact_inv<Max>(r) % P;
	}
};

void mymain()
{
	int n, k;
	cin >> n >> k;
	cout << (ResidueField<divisor>::combi<200001>(n + k - 1, k)) << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
