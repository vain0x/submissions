#include "bits/stdc++.h"
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
#define rep(_I, _N) for(auto _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

int n;
ll w_max, v_max;
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


ll napsack_exp(int i = 0, ll acc_w = 0)
{
	if ( i == n ) {
		return 0;
	}
	auto ma_v = napsack_exp(i + 1, acc_w);
	if ( acc_w + vw[i] <= w_max ) {
		max_assign(ma_v, napsack_exp(i + 1, acc_w + vw[i]) + vv[i]);
	}
	return ma_v;
}

ll napsack_nw()
{
	//ループ i のときの dp[w]
	//= ([0, i)の品物からなる重さwのナップサックの最大価値)
	auto dp = vector<ll>(w_max + 1, 0);
	rep(i, n)
	{
		auto vwi = vw[i];
		for ( ll w = w_max; w >= vwi; w -- ) {
			max_assign(dp[w], dp[w - vwi] + vv[i]);
		}
	}

	return *max_element(all(dp));
}

ll napsack_nv()
{
	//ループ i のときの dp[v]
	//= ([0, i)の品物からなる価値vのナップサックの最小重量)

	auto v_sum = accumulate(all(vv), 0LL);
	auto dp = vector<ll>(v_sum + 1, w_max + 1);
	dp[0] = 0;
	rep(i, n)
	{
		auto vvi = vv[i];
		for ( ll v = v_sum; v >= vvi; v -- ) {
			min_assign(dp[v], dp[v - vvi] + vw[i]);
		}
	}

	ll ma_v = 0;
	rep(v, v_sum + 1)
	{
		if ( dp[v] <= w_max ) {
			max_assign(ma_v, ll { v });
		}
	}
	return ma_v;
}

ll solve()
{
	cin >> n >> w_max;
	vv.resize(n);
	vw.resize(n);

	rep(i, n)
	{
		cin >> vv[i] >> vw[i];
		max_assign(v_max, vv[i]);
	}

	if ( n <= 30 ) { return napsack_exp(); }
	if ( v_max <= 1000 ) { return napsack_nv(); }

	// assert(forall i. vw[i] <= 1000);
	return napsack_nw();
}

int main()
{
	cout << solve() << endl;
	return 0;
}
