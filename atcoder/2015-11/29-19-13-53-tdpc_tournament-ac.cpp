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

// メモ化
#define MEMOIZE(_Args, _Expr) do { auto&& _args = (std::make_tuple _Args); static std::map<typename std::remove_reference<decltype(_args)>::type, typename std::remove_reference<decltype(_Expr)>::type> _memo; auto&& _it = _memo.lower_bound(_args); if ( _it == _memo.end() || _it->first != _args ) { return _memo.emplace_hint(_it, std::move(_args), (_Expr))->second; } else { return _it->second; } } while(false)

auto prob(int r1, int r2) -> double
{
	return 1 / (1 + pow<double>(10, double(r2 - r1) / 400));
}

int main()
{
	int k;
	cin >> k;

	vector<int> rs(1<<k);
	rep(i, 1 << k)
	{
		cin >> rs[i];
	}

	// ps[i]=p: i番目がいまのラウンドに勝ち上がっている確率
	vector<double> ps(1<<k, 1), ps2(1<<k);

	auto&& print = [&] () {
		rep(i, 1 << k)
		{
			printf("%.16f\n", ps[i]);
		}
	};

	rep(l, k)
	{
		int const w = 1 << l;
		ifdebug { printf("round %d\n", l); print(); };

		for ( int m = 0; m < 1 << k; m += 2 * w ) {
			repi(i, m, m + w)
			{
				double& q = ps2[i];
				q = 0;
				repi(j, m + w, m + 2 * w)
				{
					q += ps[i] * ps[j] * prob(rs[i], rs[j]);
				}
			}
			repi(j, m + w, m + 2 * w)
			{
				double& q = ps2[j];
				q = 0;
				repi(i, m, m + w)
				{
					q += ps[i] * ps[j] * prob(rs[j], rs[i]);
				}
			}
		}
		ps.swap(ps2);
	}

	print();
	return 0;
}
