#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <cstring>
#include <string>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# define mkt make_tuple
#endif
#ifdef _LOCAL
# include "for_local.h"
#else
# define ifdebug if (false)
# define echo ((void)0)
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()
#define mkp make_pair
inline int scani() { int n; scanf(" %d", &n); return n; }

ull const MOD = 1000000007;

template<typename T, typename Fun>
T exp_by_squaring(T const& x, int n, Fun const& f, T const& unit)
{
	if ( n == 0 ) return unit;
	if ( n == 1 ) return x;
	T z = unit;
	T w = x;
	while ( n != 0 ) {
		if ( (n & 1) != 0 ) {
			z = f(z, w); n ^= 1;
		} else {
			w = f(w, w); n >>= 1;
		}
	}
	return std::move(z);
}

template<typename Int>
int exp_by_squaring(Int x, int n)
{
	return exp_by_squaring(x, n, [](Int lhs, Int rhs) {return lhs * rhs; }, Int(1));
}

using Int = unsigned long long;
struct FF
{
	static ull const Mod = MOD;
	static Int unit() { return 1; }
	static Int inject(Int x) { return (x % Mod + Mod) % Mod; }
	static Int add(Int x, Int y) { return inject(inject(x) + inject(y)); }
	static Int sub(Int x, Int y) { return inject(inject(x) - inject(y)); }
	static Int mul(Int x, Int y) { return inject(inject(x) * inject(y)); }
	static Int div(Int x, Int y) { return mul(x, inv(y)); }
	static Int pow(Int x, int n) { return exp_by_squaring(x, n, mul, unit()); }
	static Int inv(Int x) { return pow(x, Mod - 2); }

	template<Int Max>
	static Int fact(int n)
	{
		static array<Int, Max> t = { 1, 1 };
		for ( int i = 2; i <= n; ++i ) {
			if ( t[i] == 0 ) { t[i] = mul(i, t[i - 1]); }
		}
		return t[n];
	}

	//(n!)^-1
	template<Int Max>
	static Int fact_inv(int n)
	{
		static array<Int, Max> t = { 1, 1 };
		if ( t[n] == 0 ) {
			t[n] = inv(fact<Max>(n));
			for ( int i = n - 1; i >= 2; --i ) {
				t[i] = mul(t[i + 1], (i + 1));
			}
		}
		return t[n];
	}
	template<Int Max>
	static Int combi(int n, int r)
	{
		if ( n < r ) { return 0; }
		if ( r == 0 ) { return 1; }
		return (fact<Max>(n) * fact_inv<Max>(n - r) % Mod) * fact_inv<Max>(r) % Mod;
	}
};

ull const n_max = 100000;
ull n;
vector<int> as;
vector<ull> dp;
vector<int> ls;

int solve()
{
	ls.resize(n + 1, 0);
	dp.resize(n + 1, 0);

	rep(i, n)
	{
		ls[as[i]] ++;
	}

	if (as[0] != 0 || ls[0] > 1) return 0;
	dp[0] = 1;

	rep(k, n)
	{
		dp[k + 1] =
			ls[k + 1] == 0 ? dp[k] :
			FF::mul(
				FF::mul(
					dp[k],
					FF::pow(2, FF::combi<n_max>(ls[k + 1], 2))
				),
				FF::pow(FF::sub(FF::pow(2, ls[k]), 1), ls[k + 1])
			);
	}
	return dp[n];
}

signed main()
{
	cin >> n;
	as.resize(n);
	rep(i, n)
	{
		cin >> as[i];
	}
	cout << solve() << endl;
	return 0;
}
