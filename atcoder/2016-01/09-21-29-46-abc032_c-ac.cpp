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
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

ll solve()
{
	ll n, k;
	cin >> n >> k;

	auto ss = vector<ll>(n);
	rep(i, n)
	{
		cin >> ss[i];
		if ( ss[i] == 0 ) return n;
	}


	ll acc = 1;
	ll len = 0;
	ll l = 0, r = 0;
	while ( l <= r ) {
		while ( r < n && acc * ss[r] <= k ) {
			acc *= ss[r];
			r ++;
		}
		if ( l == r ) break;

		assert(acc <= k);
		echo("len = " << (r - l) << "; acc = " << acc << "; " << vector<ll>(ss.begin() + l, ss.begin() + r));
		len = max(len, r - l);

		acc /= ss[l];
		l ++;
	}
	return len;
}

int main()
{
	cout << solve() << endl;
	return 0;
}
