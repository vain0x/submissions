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

static ll const MOD = 1000000007;

#define mkp make_pair

// 問題要約
// 制限時間3秒。
// n, m ～10^5
// 長さ n の数列 fs... (∈m) が与えられる。
// 連続部分列に区切る方法の総数を求める。ただし各部分列は重複元を持たない。

int n, m;
V<int> fs;

//[i]=j ⇔ fs[i]の次に出現が[j]
V<int> nex;

//calcのメモ
V<ll> dp;

ll calc(int l)
{
	switch ( n - l ) {
		case 0: //
		case 1: return 1;
		case 2: return (fs[l] != fs[l + 1] ? 2 : 1);
		default: break;
	}
	if ( dp[l] >= 0 ) return dp[l];

	ll cnt = 0;
	int r = n;
	for (int i = l; i < r; ++i )
	{
		cnt += calc(i + 1);
		cnt %= MOD;

		r = min(r, nex[i]);
	}
	echo(V<int>(fs.begin() + l, fs.begin() + r));
	echo(mkt(l, cnt));
	dp[l] = cnt;
	return cnt;
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

	dp.resize(n, -1);
	nex.resize(n, n);
	
	V<int> li(n, -1); // [i]=j: fs[i]の最後の出現が[j]
	rep(i, n)
	{
		if ( li[fs[i]] >= 0 ) {
			nex[li[fs[i]]] = i;
		}
		li[fs[i]] = i;
	}

	cout << calc(0) << endl;
	return 0;
}
