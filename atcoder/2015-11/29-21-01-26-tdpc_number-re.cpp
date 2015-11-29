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
#include <memory>
#include <numeric>
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
#define IF
#define THEN ?
#define ELSE :

static int const MOD = 1000000007;
static int const maxDigits = 100000+1;
static int const maxDiv = 100+1;

string n;
int divisor;

array<array<array<ll, maxDigits + 1>, 2>, maxDiv> memo;

ll solve(int i = 0, bool is_less_than = false, int acc = 0)
{
	if ( i == n.size() ) {
		return IF acc % divisor == 0 THEN 1 ELSE 0;
	}

	ll& cnt = memo[i][(is_less_than ? 1 : 0)][acc];
	if ( cnt >= 0 ) {
		return cnt;
	}
	cnt = 0;

	int const nd = n[i] - '0';
	rep(d, 10)
	{
		if ( is_less_than || d <= nd ) {
			cnt += solve(i + 1, is_less_than || (d < nd), (acc + d) % divisor);
			cnt %= MOD;
		}
	}
	return cnt;
}

int main()
{
	cin >> divisor >> n;

	rep(i, memo.size()) rep(j, memo[i].size())
	{
		fill(all(memo[i][j]), -1);
	}

	cout << (solve() + MOD - 1) % MOD << endl;
	return 0;
}
