#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <cstring>
#include <string>
#include <stack>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
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
# define ifdebug if (false)
# define echo(...) ((void)0)
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()
#define mkp make_pair
inline int scani() { int n; scanf(" %d", &n); return n; }

ll n, h, a, b, c, d, e;

/*
食事を抜く日は最後にまとめることで、「各日での満腹度が正」という条件を「最終日の満腹度が正」まで弱くできる。

線形計画問題 (P1)
min (a * x + c * y)
	{ x + y + z == n
	, b * x + d * y - e * z + h >= 1    (満腹度の条件)
	, x, y, z >= 0
	}

計算量に余裕があるので各 x∈[0, n] を固定して考える。

min (c * y)
	{ y + z == n - x
	, d * y - e * z >= 1 - h - b * x
	, y, z >= 0
	}

y = n - x - z とおいて、z を [0, n - x] の範囲で最大化。

	d * (n - x - z) - e * z >= 1 - h - b * x
	-(d + e) * z >= -d * (n - x) + (1 - h) - b * x
	z <= (d * (n - x) - (1 - h) + b * x) / (d + e)
//*/

ll solve()
{
	ll mi = 1LL << 62;
	rep(x, n + 1)
	{
		ll const h2 = h + a * x;
		ll const z =
			max(0LL, min(n - x,
				(b * x + d * (n - x) - (1 - h)) / (d + e)
			));
		ll const y =
			n - x - z;

		mi = min(mi, a * x + c * y);

		echo(mkt(x, y, z));
		assert(y >= 0 && z >= 0 && n == x + y + z);
	}
	return mi;
}

signed main()
{
	cin >> n >> h >> a >> b >> c >> d >> e;
	cout << solve() << endl;
	return 0;
}
