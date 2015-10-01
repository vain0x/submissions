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
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# include <unordered_map>
# define mkt make_tuple
template<typename T> using V = std::vector<T>;
#endif
#ifdef _LOCAL
# include "for_local.h"
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
	template<class T> inline void hash_combine(size_t& seed, T const& value) { seed ^= hash<T>()(value)+0x9e3779b9 + (seed << 6) + (seed >> 2); }
	template<class T, size_t Index = std::tuple_size<T>::value - 1> struct HashValueImpl { static void apply(size_t& seed, T const& tuple) { HashValueImpl<T, Index - 1>::apply(seed, tuple); hash_combine(seed, get<Index>(tuple)); } };
	template<class T> struct HashValueImpl<T, 0> { static void apply(size_t& seed, T const& tuple) { hash_combine(seed, get<0>(tuple)); } };
}
namespace std {
	template <typename ... TT> struct hash<tuple<TT...>> { size_t operator()(const tuple<TT...>& t) const { size_t seed = 0; HashValueImpl<tuple<TT...> >::apply(seed, t); return seed; } };
}

//メモ化
#define MEMOIZE(_Args, _Expr) do { auto&& _args = (std::make_tuple _Args); static std::map<typename std::remove_reference<decltype(_args)>::type, typename std::remove_reference<decltype(_Expr)>::type> _memo; auto&& _it = _memo.lower_bound(_args); if ( _it == _memo.end() || _it->first != _args ) { return _memo.emplace_hint(_it, std::move(_args), (_Expr))->second; } else { return _it->second; } } while(false)

static ll const MOD = 1000000007;

// 問題要約
// 制限時間3秒。
// n, m ～10^5
// 長さ n の数列 fs... (∈m) が与えられる。
// 連続部分列に区切る方法の総数を求める。ただし各部分列は重複元を持たない。

int n, m;
V<int> fs;

ll calc(int l, int r)
{
	assert(l <= r);
	switch ( r - l ) {
		case 0: //
		case 1: return 1;
		case 2: return (fs[l] != fs[l + 1] ? 2 : 1);
		default: break;
	}

	static auto&& f = [](int l, int r) {
		ll cnt = 0;
		set<int> s;
		repi(i, l, r)
		{
			if ( s.count(fs[i]) != 0 ) break;

			s.emplace(fs[i]);
			cnt += calc(i + 1, r);
			cnt %= MOD;
		}
		return cnt;
	};
	MEMOIZE((l, r), f(l, r));
}

int main()
{
	cin >> n >> m;
	fs.resize(n);
	rep(i, n)
	{
		cin >> fs[i];
		fs[i]--;
	}

	cout << calc(0, n) << endl;
	return 0;
}
