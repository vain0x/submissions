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

static int const MOD = 1000000007;

namespace {
template<class T> inline void hash_combine(size_t& seed, T const& value) { seed ^= std::hash<T>()(value)+0x9e3779b9 + (seed << 6) + (seed >> 2); }
template<class T, size_t Index = std::tuple_size<T>::value - 1> struct HashValueImpl { static void apply(size_t& seed, T const& tuple) { HashValueImpl<T, Index - 1>::apply(seed, tuple); hash_combine(seed, std::get<Index>(tuple)); } };
template<class T> struct HashValueImpl<T, 0> { static void apply(size_t& seed, T const& tuple) { hash_combine(seed, std::get<0>(tuple)); } };
}
namespace std {
template <typename ... TT> struct hash<tuple<TT...>> { size_t operator()(const tuple<TT...>& t) const { size_t seed = 0; HashValueImpl<tuple<TT...> >::apply(seed, t); return seed; } };
}

// メモ化
#define MEMOIZE(_Args, _Expr) do { auto&& _args = (std::make_tuple _Args); static std::map<typename std::remove_reference<decltype(_args)>::type, typename std::remove_reference<decltype(_Expr)>::type> _memo; auto&& _it = _memo.lower_bound(_args); if ( _it == _memo.end() || _it->first != _args ) { return _memo.emplace_hint(_it, std::move(_args), (_Expr))->second; } else { return _it->second; } } while(false)

string n;
int d;

ll solve(int i = 0, bool is_less_than = false, int acc = 0)
{
	if ( i == n.size() ) {
		return IF acc % d == 0 THEN 1 ELSE 0;
	}

	auto&& f = [&] () {
		int const nd = n[i] - '0';
		ll k = 0;
		rep(d, 10)
		{
			if ( is_less_than || d <= nd ) {
				k = (k + solve(i + 1, is_less_than || (d < nd), acc + d)) % MOD;
			}
		}
		return k;
	};

	MEMOIZE((i, is_less_than, acc)
		, f());
}

int main()
{
	cin >> d >> n;
	cout << solve() - 1 << endl;
	return 0;
}
