// http://www.slideshare.net/chokudai/abc017

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

V<int> dp;

ll calc()
{
	dp.resize(n + 1, 0);
	dp[0] = 1;

	int l = 0, r = 0;
	int acc = 1;
	set<int> ss;

	while ( l < n ) {
		while ( r < n && ss.count(fs[r]) == 0 ) {
			ss.emplace(fs[r]);
			++r;
			dp[r] = acc;
			acc *= 2;
		}
		echo(V<int>(fs.begin() + l, fs.begin() + r));

		if ( r == n ) break;
		do {
			ss.erase(fs[l]);
			acc -= dp[l];
			++l;
		} while ( fs[l - 1] != fs[r] );
	}
	return dp[n];
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

	cout << calc() << endl;
	return 0;
}
