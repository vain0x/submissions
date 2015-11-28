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

int an, bn;
vector<int> as, bs;

auto myadd(pair<int, int> p, int n, bool b) -> pair<int, int>
{
	return make_pair
		( p.first  + (b ? 0 : n)
		, p.second + (b ? n : 0));
}

auto mymax(pair<int, int> l, pair<int, int> r, bool b) -> pair<int, int>
{
	if ( b ) {
		return (l.second >= r.second ? l : r);
	} else {
		return (l.first >= r.first ? l : r);
	}
}

pair<int, int> dfs(int ak, int bk, bool b)
{
	auto&& take_a = [&] () {
		return myadd(dfs(ak + 1, bk, !b), as[ak], b);
	};
	auto&& take_b = [&] () {
		return myadd(dfs(ak, bk + 1, !b), bs[bk], b);
	};

	if ( ak == an && bk == bn ) {
		return make_pair(0, 0);
	}

	MEMOIZE((ak, bk, b)
		, ((ak == an) ? take_b() :
			(bk == bn) ? take_a() :
				mymax(take_a(), take_b(), b)
			));
}

int main()
{
	cin >> an >> bn;
	as.resize(an);
	bs.resize(bn);
	rep(i, an)
	{
		cin >> as[i];
	}
	rep(j, bn)
	{
		cin >> bs[j];
	}

	cout << dfs(0, 0, false).first << endl;

	return 0;
}
