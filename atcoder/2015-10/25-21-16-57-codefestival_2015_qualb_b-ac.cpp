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


int main()
{
	int n, m;
	cin >> n >> m;
	map<int, int> fs;
	rep(i, n)
	{
		int a;
		cin >> a;
		fs[a] ++;
	}

	int ma = 0;
	int result;
	for ( auto& kv : fs ) {
		if ( ma < kv.second ) {
			tie(result, ma) = kv;
		}
	}
	if ( ma > (n / 2) ) {
		cout << result << endl;
	} else {
		cout << '?' << endl;
	}

	return 0;
}
