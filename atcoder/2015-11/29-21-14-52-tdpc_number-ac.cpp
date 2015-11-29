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

vector<vector<ll>> dp, dp_next;

ll solve()
{
	dp.resize(2, vector<ll>(divisor, 0));
	dp_next.resize(2, vector<ll>(divisor, 0));
	dp[0][0] = 1;

	rep(i, n.size())
	{
		rep(lt, 2) rep(acc, divisor)
		{
			int const nd = n[i] - '0';
			rep(d, 10)
			{
				if ( lt != 0 || d <= nd ) {
					int lt2 = IF lt != 0 || (d < nd) THEN 1 ELSE 0;
					ll& cnt2 = dp_next[lt2][(acc + d) % divisor];
					cnt2 += dp[lt][acc];
					cnt2 %= MOD;
				}
			}
		}

		swap(dp, dp_next);
		rep(lt, 2) { fill(all(dp_next[lt]), 0); }
	}

	ll cnt = (dp[0][0] + dp[1][0]);
	cnt += MOD - 1;
	cnt %= MOD;
	return cnt;
}

int main()
{
	cin >> divisor >> n;
	cout << solve() << endl;
	return 0;
}
