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

int n, m;
V<int> ls, rs, ss;

int main()
{
	cin >> n >> m;
	for ( auto p : { &ls, &rs, &ss } ) p->resize(n);
	rep(i, n)
	{
		cin >> ls[i] >> rs[i] >> ss[i];
		ls[i] --;
	}

	V<int> ds;
	ds.resize(m + 1, 0);

	int total = 0;
	rep(i, n)
	{
		ds[ls[i]] -= ss[i];
		ds[rs[i]] += ss[i];

		total += ss[i];
	}

	int ma = 0;
	int cur_score = total;
	rep(j, m)
	{
		cur_score += ds[j];
		ma = max(ma, cur_score);
	}
	cout << ma << endl;

	return 0;
}
