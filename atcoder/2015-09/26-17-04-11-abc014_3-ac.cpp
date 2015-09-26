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

int highestOneBit(int s)
{
	for ( int i = 0; i <= 4; ++i ) { s |= (s >> (1 << i)); }
	return (s - ((unsigned int)(s) >> 1));
}


struct SegTree
{
	struct Node
	{
		int s;
		int x;
	};
	int n;
	vector<Node> v;

	void init(int m)
	{
		n = highestOneBit(m) << 1;
		v.resize(n << 1, {});
	}
	ll query(int l, int r)
	{
		return query_(1, l, r, 0, n);
	}

	ll query_(int k, int l, int r, int nl, int nr)
	{
		if ( r <= nl || nr <= l || r <= l ) return 0;
		if ( l <= nl && nr <= r ) {
			return v[k].x * (nr - nl) + v[k].s;
		} else {
			int const nm = nl + (nr - nl) / 2;
			auto&& sl = query_(k * 2 + 0, l, r, nl, nm);
			auto&& sr = query_(k * 2 + 1, l, r, nm, nr);
			return v[k].x * (min(r, nr) - max(l, nl)) + sl + sr;
		}
	}
	void add(int val, int l, int r)
	{
		add_(1, l, r, 0, n, val);
	}
	void add_(int k, int l, int r, int nl, int nr, int val)
	{
		if ( r <= nl || nr <= l || r <= l ) return;
		if ( l <= nl && nr <= r ) {
			v[k].x += val;
		} else {
			int const nm = nl + (nr - nl) / 2;
			add_(k * 2 + 0, l, r, nl, nm, val);
			add_(k * 2 + 1, l, r, nm, nr, val);
			v[k].s =
				+ query_(k * 2 + 0, nl, nm, nl, nm) 
				+ query_(k * 2 + 1, nm, nr, nm, nr);
		}
	}

	void resolve() { resolve_(1, 0, n); }
	void resolve_(int k, int nl, int nr)
	{
		if ( nr - nl <= 1 ) return;
		
		int const nm = nl + (nr - nl) / 2;
		add_(k * 2 + 0, nl, nm, nl, nm, v[k].x);
		add_(k * 2 + 1, nm, nr, nm, nr, v[k].x);
		v[k].x = 0;
		resolve_(k * 2 + 0, nl, nm);
		resolve_(k * 2 + 1, nm, nr);
	}

	int at(int i)
	{
		return v[n + i].x;
	}
};

static int const M_MAX = 1000000+1;

int main()
{
	int n;
	cin >> n;

	SegTree st;
	st.init(M_MAX);
	rep(i, n)
	{
		int a, b;
		cin >> a >> b;
		b ++;

		st.add(1, a, b);
	}

	st.resolve();

	int ma = 0;
	rep(i, M_MAX)
	{
		ma = max(ma, st.at(i));
	}
	cout << ma << endl;
	
	return 0;
}
