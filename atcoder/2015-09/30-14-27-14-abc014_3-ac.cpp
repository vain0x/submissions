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

vector<int> ds;
int const M_MAX = 1000000 + 1;

int main()
{
	ds.resize(M_MAX);

	int n;
	cin >> n;
	rep(i, n)
	{
		int a, b;
		cin >> a >> b;
		++b;

		ds[a] ++;
		if ( b < M_MAX ) {
			ds[b] --;
		}
	}

	int s = 0;
	int ma = 0;
	rep(i, M_MAX)
	{
		s += ds[i];
		ma = max(ma, s);
	}
	cout << ma << endl;
	return 0;
}
