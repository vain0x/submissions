#if 1

#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <string>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#ifndef ONLINE_JUDGE //POJ
# include <random>
# include <array>
# define mkt make_tuple
# define empb emplace_back
#endif
using namespace std;
typedef unsigned int uint; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(unsigned int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(unsigned int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()
#define scani(_V) std::scanf("%d", &_V)
#define printi(_V) std::printf("%d", static_cast<int>(_V))
int main() { void mymain(); /*std::ios::sync_with_stdio(false); std::cin.tie(0);*/ mymain(); return 0; }
static ull const divisor = 1000000007;

template<ull P>
struct ResidueField {
	using Num = unsigned long long;
	static Num pow(Num x, int n) {
		x %= P;
		Num r = 1;
		Num p = x;
		while ( n != 0 ) {
			if ( (n & 1) != 0 ) {
				r = (r * p) % P; n ^= 1;
			} else {
				p = (p * p) % P; n >>= 1;
			}
		}
		return r;
	}
	static Num inverse(Num x) { static_assert(P >= 2, ""); return pow(x, P - 2); }

	template<Num Max>
	static Num fact(int n) {
		static array<Num, Max> t = { 1, 1 };
		for ( int i = 2; i <= n; ++i ) { if ( t[i] == 0 ) { t[i] = i * t[i - 1] % P; } }
		return t[n];
	}
	template<Num Max>
	static Num fact_inv(int n) {
		static array<Num, Max> t = { 1, 1 };
		if ( t[n] == 0 ) {
			t[n] = inverse(fact<Max>(n));
			for ( int i = n - 1; i >= 2; --i ) {
				t[i] = (t[i + 1] * (i + 1)) % P;
			}
		}
		return t[n];
	}
	template<Num Max>
	static Num combi(int n, int r) {
		if ( n < r ) { return 0; }
		if ( r == 0 ) { return 1; }
		return (fact<Max>(n) * fact_inv<Max>(n - r) % P) * fact_inv<Max>(r) % P;
	}
};

void mymain() {
	int n, k;
	scani(n); scani(k);

	ull r = 0;
	if ( n <= k ) {
		int d = k / n;
		int m = k % n;
		r = ResidueField<divisor>::combi<500>(n, m);
	} else {
		r = ResidueField<divisor>::combi<500 + 100>(n + k - 1, k);
	}
	cout << r << endl;
}

#endif
