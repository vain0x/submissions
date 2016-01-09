#include "bits/stdc++.h"
#include <unordered_map>
#ifdef _LOCAL
# include "local/local.hpp"
#else
# define assert(...) ((void)0)
# define ifdebug if (false)
# define echo(...) ((void)0)
#endif
using namespace std;
typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(auto _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(auto _I = decltype(_N)(0); (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

int n;
ll w_max;
vector<ll> vv, vw;

template<typename T, typename U>
auto min_assign(T& l, U&& r) -> T&
{
	return l = min(l, std::forward<U>(r));
}

template<typename T, typename U>
auto max_assign(T& l, U&& r) -> T&
{
	return l = max(l, std::forward<U>(r));
}

// {w, v}: 重さ w 、価値 v のナップサック (品物の組み合わせ) を表す。
using napsack_t = pair<ll, ll>;

// [ml, mr) の品物からなるナップサックを全列挙し、
// 重さ (first) に関して辞書順に整列した状態で返す。
// ただし、空でない非最適な(他のナップサックより重いにもかかわらず価値が低い)ナップサックは無視する。
auto napsack_all(size_t ml, size_t mr)
-> vector<napsack_t>
{
	auto m = mr - ml;
	auto ps = vector<napsack_t>(1 << m);
	rep(bs, 1 << m)
	{
		rep(i, m)
		{
			if ( bs >> i & 1 ) {
				ps[bs].first  += vw[ml + i];
				ps[bs].second += vv[ml + i];
			}
		}
	}
	sort(all(ps));

	// 非最適なものを消していく
	int iw = 1;
	repi(i, 1U, ps.size())
	{
		if ( ps[iw - 1].second < ps[i].second ) {
			ps[iw] = ps[i];
			iw ++;
		}
	}
	ps.resize(iw);
	return ps;
}

// O(n * 2^(n/2))
ll napsack_n()
{
	size_t n1 = n / 2, n2 = n - n1;
	auto ps1 = napsack_all(0, n1);
	auto ps2 = napsack_all(n1, n);
	reverse(all(ps1));

	ll ma_v = 0;

	size_t i2 = 0;
	rep(i1, ps1.size())
	{
		auto w1 = ps1[i1].first;

		// w1 はループの進行について単調減少なので、(w_max - w1) は単調増加。
		auto w2_max = w_max - w1;

		// ps2[ ].first は単調増加なので、w2_max を超える直前または末尾で最大になる。(そのとき価値も最大)
		// また、w2_max は単調増加なので、次のループでも (ps2[i2].first <= w2_max) が成り立つ。
		while ( i2 + 1 < ps2.size() && ps2[i2 + 1].first <= w2_max ) {
			i2 ++;
		}

		// 2つのナップサックの和を候補に入れる
		if ( w1 + ps2[i2].first <= w_max ) {
			max_assign(ma_v, ps1[i1].second + ps2[i2].second);
		}
	}
	return ma_v;
}

// O(n * w_max)
ll napsack_nw()
{
	// ループ i のときの dp[w]
	// = ([0, i) の品物からなる重さ w のナップサックの最大価値)
	auto dp = vector<ll>(static_cast<size_t>(w_max + 1), 0);
	rep(i, n)
	{
		auto vwi = static_cast<size_t>(vw[i]);
		for ( auto w = static_cast<size_t>(w_max); w >= vwi; w -- ) {
			max_assign(dp[w], dp[w - vwi] + vv[i]);
		}
	}
	return *max_element(all(dp));
}

// O(n * max vv)
ll napsack_nv()
{
	// ループ i のときの dp[v]
	// = ([0, i) の品物からなる価値 v のナップサックの最小重量)

	auto v_sum = accumulate(all(vv), 0LL);
	auto dp = vector<ll>(static_cast<size_t>(v_sum + 1), w_max + 1);
	dp[0] = 0;
	rep(i, n)
	{
		auto vvi = static_cast<size_t>(vv[i]);
		for ( auto v = dp.size() - 1; v >= vvi; v -- ) {
			min_assign(dp[v], dp[v - vvi] + vw[i]);
		}
	}

	ll ma_v = 0;
	rep(v, dp.size())
	{
		if ( dp[v] <= w_max ) {
			max_assign(ma_v, static_cast<ll>(v));
		}
	}
	return ma_v;
}

ll solve()
{
	cin >> n >> w_max;
	vv.resize(n);
	vw.resize(n);

	auto vv_small = true, vw_small = true;
	rep(i, n)
	{
		cin >> vv[i] >> vw[i];

		if ( vv[i] > 1000 ) { vv_small = false; }
		if ( vw[i] > 1000 ) { vw_small = false; }
	}

	if ( vv_small ) return napsack_nv();
	if ( vw_small ) return napsack_nw();
	assert(n <= 30);
	return napsack_n();
}

int main()
{
	cout << solve() << endl;
	return 0;
}
