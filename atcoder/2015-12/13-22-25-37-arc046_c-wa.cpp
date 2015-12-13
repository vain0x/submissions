#include <iostream>
#include <cstdio>
#include <string>
#include <cstring>
#include <algorithm>
#include <numeric>
#include <vector>
#include <list>
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
	template<class T> inline void hash_combine(size_t& seed, T const& value) { seed ^= std::hash<T>()(value) + 0x9e3779b9 + (seed << 6) + (seed >> 2); }
	template<class T, size_t Index = std::tuple_size<T>::value - 1> struct HashValueImpl { static void apply(size_t& seed, T const& tuple) { HashValueImpl<T, Index - 1>::apply(seed, tuple); hash_combine(seed, std::get<Index>(tuple)); } };
	template<class T> struct HashValueImpl<T, 0> { static void apply(size_t& seed, T const& tuple) { hash_combine(seed, std::get<0>(tuple)); } };
}
namespace std {
	template <typename ... TT> struct hash<tuple<TT...>> { size_t operator()(const tuple<TT...>& t) const { size_t seed = 0; HashValueImpl<tuple<TT...> >::apply(seed, t); return seed; } };
}

static int const INF = 1 << 30;

int source, sink;
int n, m, nv;

// i→j: iの条件を満たす最初の異性j
vector<int> gr;

//i から j への有向辺
using e_t = tuple<int, int>;

// これに入ってる辺はキャパシティ 0
// 入ってない辺はキャパシティ 1
unordered_set<e_t> capa;

vector<bool> used;

//点vから辿れる各辺についてfを行う
//fが正の数を返したら中断
template<typename Fun>
int each(int v, Fun&& f)
{
	if ( v == source ) {
		//各男性
		rep(i, n) {
			int r = f(i);
			if ( r > 0 ) return r;
		}

	} else if ( v == sink ) {
		//

	} else if ( n <= v && v < n + m ) {
		return f(sink);

	} else {
		//各女性、ただし条件を満たす組み合わせの間にだけ辺がある
		repi(j, gr[v] - n, m) {
			if ( gr[n + j] <= v ) {
				int r = f(n + j);
				if ( r > 0 ) return r;
			}
		}
	}
	return 0;
}

int dfs(int v, int t, int f)
{
	if ( v == t ) return f;
	used[v] = true;
	return each(v, [&](int u) {
		if ( !used[u] && capa.count(mkt(v, u)) == 0) {
			int const d = dfs(u, t, min(f, 1));
			if ( d > 0 ) {
				capa.emplace(v, u);
				capa.erase(mkt(u, v));
				return d;
			}
		}
	});
}

int max_flow(int s, int t)
{
	int flow = 0;
	for ( ;;) {
		used.resize(nv, false);
		int const f = dfs(s, t, INF);
		if ( f == 0 ) return flow;
		flow += f;
	}
}

int main()
{
	cin >> n >> m;
	nv = n + m + 2;
	source = n + m;
	sink = n + m + 1;

	vector<pair<int, int>> men(n), women(m);
	rep(i, n)
	{
		cin >> men[i].first >> men[i].second;
	}
	rep(i, m)
	{
		cin >> women[i].first >> women[i].second;
	}
	sort(all(men));
	sort(all(women));

	gr.resize(n + m);
	rep(i, n) {
		gr[i] = n + distance(women.begin(), lower_bound(all(women), make_pair(men[i].second, 0)));
	}
	rep(j, m) {
		gr[n + j] = distance(men.begin(), lower_bound(all(men), make_pair(women[j].second, 0)));
	}

	cout << max_flow(source, sink) << endl;
	return 0;
}
