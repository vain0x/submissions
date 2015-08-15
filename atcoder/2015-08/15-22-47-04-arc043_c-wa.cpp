#include <cassert>
#include <functional>
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
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# define mkt make_tuple
#endif
#ifdef _LOCAL
# include "for_local.h"
#else
# define ifdebug if (false)
# define echo ((void)0)
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()
#define mkp make_pair
inline int scani() { int n; scanf(" %d", &n); return n; }

#include <memory>

template<typename T>
struct BinaryIndexedTree
{
	vector<T> v_;
	BinaryIndexedTree(int n, T const& unit) : v_(n + 1, unit) {}
	template<typename Fun>
	void modify(int k, Fun&& f)
	{
		for ( int i = k + 1; i < static_cast<int>(v_.size()); i += i & -i ) {
			f(v_[i]);
		}
	}
	template<typename Fun, typename State>
	State fold(int m, State s, Fun&& f)
	{
		for ( int i = m; i > 0; i -= i & -i ) {
			s = f(s, v_[i]);
		}
		return std::move(s);
	}

	//for operator+
	template<typename Value>
	void add(int k, Value&& v) { modify(k, [&](T& t) { t += v; }); }
	template<typename = void>
	decltype(std::declval<T>() + std::declval<T>()) sum(int m) { return fold(m, 0, [&](T const& acc, T const& x) { return acc + x; }); }
};

int n;
vector<int> bs, cs;

ll dist(vector<int> const& vs)
{
	int n = vs.size();
	ll cnt = 0;
	BinaryIndexedTree<int> ws(n, 0);
	rep(i, n)
	{
		cnt += i - ws.sum(vs[i]);
		ws.add(vs[i], 1);
	}
	return cnt;
}

bool solve()
{
	int d = dist(bs);
	if ( d & 1 ) return false;

	cs = bs;
	int cnt = d / 2;

	rep(j, n - 1) repi(i, j, n - 1)
	{
		if ( cs[i] > cs[i + 1] ) {
			swap(cs[i], cs[i + 1]);
			--cnt;
			if ( cnt == 0 ) return true;
		}
	}
	return false;
}

signed main()
{
	cin >> n;
	bs.resize(n);

	vector<int> as_inv(n);
	rep(i, n)
	{
		int a;
		cin >> a; --a;
		as_inv[a] = i;
	}
	rep(i, n)
	{
		int b;
		cin >> b; --b;
		bs[i] = as_inv[b];
	}

	if ( solve() ) {
		rep(i, n)
		{
			if ( i != 0 ) cout << ' ';
			cout << (cs[i] + 1);
		}
		cout << endl;
	} else {
		cout << "-1" << endl;
	}

	return 0;
}
