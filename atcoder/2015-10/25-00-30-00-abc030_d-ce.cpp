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

int n, a;
string str_k; // 入力 K の文字列表現
vector<int> bs;

// 10進数文字列 s の mod で割った余りを計算する
ll str_mod(string const& s, ll mod)
{
	ll rem = 0;
	int i = 0;
	while ( s[i] == '0' ) ++i; // Skip redundant zeros

	for (; i < s.length(); ++ i ) {
		char c = s[i];
		assert(isdigit(c));

		// Horner 法による冪級数計算
		rem = (rem * 10 + (c - '0')) % mod;
	}
	return rem;
}

// 単語 i から始めて、k ステップ後に見ている単語の番号
int simulate(int i, ll k)
{
	for ( ; k != 0; --k ) {
		i = bs[i];
	}
	return i;
}

int solve()
{
	if ( str_k.length() <= 8 ) {
		ll k = atoll(str_k.c_str());
		return simulate(a, k);
	} else {
		// steps[i]=s: aから始めて s ステップ目で単語 i をチェックする
		vector<int> steps(n, -1);
		int step = 0;
		auto&& find_cycle_length = [&]() {
			for ( int j = a, step = 0;; j = bs[j], ++step ) {
				if ( steps[j] >= 0 ) {
					return step - steps[j];
				}
				steps[j] = step;
			}
		};

		// 巡回部分の長さ
		int cycle_len = find_cycle_length();

		// 余りをとることで巡回を省略する
		ll k = str_mod(str_k, cycle_len);

		// 剰余が小さいと巡回部分に到達しないまま終わってしまうことがある。
		// (いま与えられた k は十分に大きいので、必ず巡回部分で停止するはず。)
		// k >= n なら巡回部分に確実に入る。
		// k に cycle_len の倍数を加えても剰余は変わらない。
		k += (n / cycle_len + 1) * cycle_len;
		assert(k >= n);

		return simulate(a, k);
	}
}

int main()
{
	cin >> n >> a >> s;
	--a;
	bs.resize(n);
	rep(i, n)
	{
		cin >> bs[i];
		--bs[i];
	}

	cout << (solve() + 1) << endl;
	return 0;
}
