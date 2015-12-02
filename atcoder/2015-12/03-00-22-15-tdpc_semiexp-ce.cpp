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
#include <numeric>
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
#define IF
#define THEN ?
#define ELSE :

static ll const MOD = 1000000007;
int n, k_max;

struct DP
{
	list<ll> v;
	ll sum;

	void update(int i)
	{
		// この挿入により添字が増えて、連続停車数が1ずつ増加
		if (i == 0 || i == n - 1) {
			v.push_front(0);
		}
		else {
			v.push_front(sum);
			sum += sum;
			sum %= MOD;
		}

		// k_max 以上連続で停車している場合を除去
		ll knt = v.back();
		v.pop_back();
		sum += MOD - knt;
		sum %= MOD;
	}
};

ll calc()
{
	DP dp;
	dp.v.resize(k_max, 0);
	dp.v.front() = 1;
	dp.sum = 1;

	rep(i, n)
	{
		dp.update(i);
	}

	return dp.sum;
}

int main()
{
	cin >> n >> k_max;
	cout << calc() << endl;
	return 0;
}
