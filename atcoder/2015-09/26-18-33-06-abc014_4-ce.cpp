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

int main()
{
	int n;
	cin >> n;

	vector<vector<int>> gr(n);

	auto&& add_edge = [&](int x, int y) {
		gr[x].push_back(y);
		gr[y].push_back(x);
	};

	rep(i, n - 1)
	{
		int x, y;
		cin >> x >> y;
		--x; --y;
		add_edge(x, y);
	}

	//木の根
	int const r = rand() % n;

	//ノードの深さ
	vector<int> depth(n, 0);

	//pars[u][i] はノード u のより 2^i 浅い位置にある祖先ノード
	vector<vector<int>> pars(n, {});

	// 深さ優先探索
	{
		//ブランチ
		vector<int> br(n, -1);

		vector<bool> done(n, false);
		stack<int> sk;
		sk.push(r);

		while ( ! sk.empty() ) {
			int const v = sk.top();
			sk.pop();
			if ( done[v] ) continue;
			done[v] = true;

			int const d = depth[v];
			br[d] = v;

			for ( int u : gr[v] )
			{
				if ( done[u] ) continue;
				sk.push(u);

				depth[u] = d + 1;

				// 2^k 世代上の祖先を記録
				for ( int m = 1; m <= d + 1; m = m << 1) {
					pars[u].push_back(br[d + 1 - m]);
				}
			}
		}
	}

	int q;
	cin >> q;

	// O(n * q)
	rep(_, q)
	{
		int a, b;
		cin >> a >> b; --a; --b;
		array<int, 2> x = { { a, b } };
		array<int, 2> dep = { { depth[x[0]], depth[x[1]] } };
		int l = 1;

		// 深さを合わせる
		rep(i, 2)
		{
			int j = 1 - i;
			while ( dep[i] > dep[j] ) {
				repi(k, 1, 32) {
					if ( dep[i] - dep[j] >= (1 << k) ) continue;
					--k;
					x[i] = pars[x[i]][k];
					l += dep[i] - depth[x[i]];
					dep[i] = depth[x[i]];
					break;
				}
			}
		}
		while ( x[0] != x[1] ) {
			// 一致しない最も遠い祖先まで戻る
			repi(k, 1, 32) {
				if ( dep[0] >= (1 << k) && pars[x[0]][k] != pars[x[1]][k] ) continue;
				--k;
				rep(i, 2)
				{
					x[i] = pars[x[i]][k];
					l += dep[i] - depth[x[i]];
					dep[i] = depth[x[i]];
				}
				break;
			}
		}
		cout << l << endl;
	}
	return 0;
}
