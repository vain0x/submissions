#include <iostream>
#include <cstdio>
#include <string>
#include <cstring>
#include <algorithm>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <queue>
#include <functional>
#include <cmath>
#include <memory>
#include <numeric>
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# include <unordered_map>
# include <unordered_set>
# define mkt make_tuple
#endif
#ifdef _LOCAL
# include "local/local.hpp"
#else
# define assert(...) ((void)0)
# define ifdebug if (false)
# define echo(...) ((void)0)
#endif
using namespace std;
typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()
#define IF
#define THEN ?
#define ELSE :

namespace {
template<class T> inline void hash_combine(size_t& seed, T const& value) { seed ^= std::hash<T>()(value)+0x9e3779b9 + (seed << 6) + (seed >> 2); }
template<class T, size_t Index = std::tuple_size<T>::value - 1> struct HashValueImpl { static void apply(size_t& seed, T const& tuple) { HashValueImpl<T, Index - 1>::apply(seed, tuple); hash_combine(seed, std::get<Index>(tuple)); } };
template<class T> struct HashValueImpl<T, 0> { static void apply(size_t& seed, T const& tuple) { hash_combine(seed, std::get<0>(tuple)); } };
}
namespace std {
template <typename ... TT> struct hash<tuple<TT...>> { size_t operator()(const tuple<TT...>& t) const { size_t seed = 0; HashValueImpl<tuple<TT...> >::apply(seed, t); return seed; } };
}

using mul_t = map<ll, ll>;

mul_t factorize(ll n)
{
	assert(n >= 1);
	if ( n == 1 ) return mul_t {};

	mul_t fs;
	ll p = 2;
	for ( ; p * p <= n; ++ p ) {
		if ( n % p == 0 ) {
			ll& m = fs[p];
			m = 0;
			while ( n % p == 0 ) {
				m++;
				n /= p;
			}
		}
	}
	if ( n != 1 ) {
		fs[n] = 1;
	}
	return fs;
}

ll n;
ll d;

using tup_t = tuple<int, int, int>;

bool conv(mul_t m, tup_t& t)
{
	t = mkt(m[2], m[3], m[5]);
	return ( m.size() <= 3 );
}

bool operator<=(tup_t const& l, tup_t const& r)
{
	return get<0>(l) <= get<0>(r)
		&& get<1>(l) <= get<1>(r)
		&& get<2>(l) <= get<2>(r);
}

tup_t operator+(tup_t const& l, tup_t const& r)
{
	return mkt
		( get<0>(l) + get<0>(r)
		, get<1>(l) + get<1>(r)
		, get<2>(l) + get<2>(r));
}

double solve()
{
	array<tup_t, 7> dice;
	repi(i, 1, 7)
	{
		conv(factorize(i), dice[i]);
	}

	tup_t dt;
	if ( ! conv(factorize(d), dt) ) return 0;

	double p = 0;
	unordered_map<tup_t, double> dp, dp2;
	dp[dice[1]] = 1;

	rep(i, n)
	{
		dp2.clear();

		for ( auto&& kv : dp ) {
			repi(j, 1, 7)
			{
				auto&& k2 = kv.first + dice[j];
				double p2 = kv.second / 6;

				if ( dt <= k2 ) {
					p += p2;
				} else {
					dp2[move(k2)] += p2;
				}
			}
		}

		swap(dp, dp2);
	}

	for ( auto&& kv : dp ) {
		if ( dt <= kv.first ) {
			p += kv.second;
		}
	}
	return p;
}

int main()
{
	cin >> n >> d;

	printf("%.16f\n", solve());
	return 0;
}
