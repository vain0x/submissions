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
#define rep(_I, _N) for(auto _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

int n;
ll w_max;
vector<ll> vv, vw;

// 半々列挙
ll napsack_fast()
{
	int n1 = n / 2, n2 = n - n1;

	// 前半列挙
	auto vwv = vector<pair<ll, ll>>{};
	rep(bs, 1 << n1)
	{
		ll w1 = 0, v1 = 0;
		rep(i, n1) {
			if ( bs & (1 << i) ) {
				w1 += vw[i]; v1 += vv[i];
			}
		}
		vwv.emplace_back(w1, v1);
	}

	// 極大でない元を除去する
	// (極大でないような選び方は絶対に使用されないので使わない)
	// ただし空ナップサック (0, 0) は残す。
	sort(all(vwv));
	int iw = 1; // 書き込み末尾
	rep(i, vwv.size())
	{
		if ( vwv[iw - 1].second < vwv[i].second ) {
			vwv[iw] = vwv[i];
			iw++;
		}
	}
	vwv.resize(iw);

	// 後半列挙
	ll ma_v = 0;
	rep(bs, 1 << n2)
	{
		ll w2 = 0, v2 = 0;
		rep(i, n2) {
			if ( bs & (1 << i) ) {
				w2 += vw[n1 + i];
				v2 += vv[n1 + i];
			}
		}
		if ( w2 > w_max ) continue;

		// 重さを超限する最初の要素
		auto it = lower_bound(all(vwv), make_pair(w_max - w2 + 1, 0LL));
		--it; // ....の1つ前
		ma_v = max(ma_v, v2 + it->second);
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
	}
	return napsack_fast();
}

int main()
{
	cout << solve() << endl;
	return 0;
}
