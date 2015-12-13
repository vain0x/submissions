#include <iostream>
#include <cstdio>
#include <string>
#include <cstring>
#include <algorithm>
#include <numeric>
#include <vector>
#include <list>
#include <map>
#include <set>
#include <stack>
#include <queue>
#include <functional>
#include <cmath>
#include <memory>
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

/*
bool is_zorome(ll n)
{
	int d = n % 10;
	n /= 10;

	while ( n != 0 ) {
		if ( n % 10 != d ) return false;
		n /= 10;
	}
	return true;
}

int main()
{
	ll n = 1;
	rep(i, 50) {
		while ( ! is_zorome(n) ) { ++ n; }
		cout << n << endl;
		++n;
	}
	return 0;
}
//*/

int zorome_seq[] =
	{ 1
	, 2
	, 3
	, 4
	, 5
	, 6
	, 7
	, 8
	, 9
	, 11
	, 22
	, 33
	, 44
	, 55
	, 66
	, 77
	, 88
	, 99
	, 111
	, 222
	, 333
	, 444
	, 555
	, 666
	, 777
	, 888
	, 999
	, 1111
	, 2222
	, 3333
	, 4444
	, 5555
	, 6666
	, 7777
	, 8888
	, 9999
	, 11111
	, 22222
	, 33333
	, 44444
	, 55555
	, 66666
	, 77777
	, 88888
	, 99999
	, 111111
	, 222222
	, 333333
	, 444444
	, 555555
	};

int main()
{
	int n;
	cin >> n;
	--n;
	cout << zorome_seq[n] << endl;
	return 0;
}
