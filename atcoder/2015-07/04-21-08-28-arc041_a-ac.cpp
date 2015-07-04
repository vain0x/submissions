#include <cassert>
#include <functional>
#include <bitset>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <cstring>
#include <string>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#ifndef ONLINE_JUDGE //POJ
# include <complex>
# include <random>
# include <array>
# define mkt make_tuple
# define empb emplace_back
# define all(_X) (_X).begin(), (_X).end()
#else
# define all(_X) begin(_X), end(_X)
#endif
#ifdef _LOCAL
# include "for_local.h"
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair



signed main() {
	int x, y, k;
	cin >> x >> y >> k;

	int m = min(y, k);
	cout << (x - (k - m) + m) << endl;
	return 0;
}
