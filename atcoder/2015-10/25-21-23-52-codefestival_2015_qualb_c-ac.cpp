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


using Q = priority_queue<int>;
Q room, rsv;
int n, m;

bool solve()
{
	if ( n < m ) return false;
	rep(i, m)
	{
		int a = room.top(); room.pop();
		int b = rsv.top(); rsv.pop();
		echo(mkt(a, "<-", b));
		if ( a < b ) return false;
	}
	return true;
}

int main()
{
	cin >> n >> m;
	rep(i, n)
	{
		int a; cin >> a; room.push(a);
	}
	rep(i, m)
	{
		int b; cin >> b; rsv.push(b);
	}

	cout << (solve() ? "YES" : "NO") << endl;

	return 0;
}
