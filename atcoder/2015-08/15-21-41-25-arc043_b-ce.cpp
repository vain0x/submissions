#if 1
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

template<typename T>
T heightInt(T n)
{
	if ( n == 1 ) return 1;
	T h = 0;
	for ( ; n > static_cast<T>(1 << h); ++h );
	return h;
}

template<typename T, typename Semigroup>
class SegTree
{
	using uint = unsigned int;
	std::vector<T> v_;
	uint n_;
	uint h_;
public:
	void init(int n)
	{
		n_ = n;
		h_ = heightInt<uint>(n);
		v_.resize((1 << h_) * 2 - 1, Semigroup::unit());
	}
private:
	int nodeIndexFromArrayIndex(int k) const
	{
		//assert(0 <= k && static_cast<uint>(k) < n_);
		return ((1 << h_) - 1) + k;
	}
public:
	T const& at(int k) const { return v_[nodeIndexFromArrayIndex(k)]; }
	void update(int k, T const& x)
	{
		uint i = nodeIndexFromArrayIndex(k);
		v_[i] = x;

		while ( i != 0 ) {
			uint const parent = (i - 1) / 2;
			uint const child = parent * 2 + 1;
			v_[parent] = Semigroup::operate(v_[child], v_[child + 1]);
			i = parent;
		}
	}
public:
	T query(uint begin, uint end)
	{
		if ( begin >= end ) return Semigroup::unit();
		return queryRec(0, 0, 1 << h_, begin, end);
	}
private:
	T queryRec(uint i, uint nodeBeg, uint nodeEnd, uint queryBeg, uint queryEnd)
	{
		assert(nodeBeg <= queryBeg && queryBeg < queryEnd && queryEnd <= nodeEnd);
		uint const nodeMid = nodeBeg + (nodeEnd - nodeBeg + 1) / 2;
		if ( nodeBeg == queryBeg && nodeEnd == queryEnd ) {
			return v_[i];
		} else if ( queryEnd <= nodeMid ) {
			return queryRec(i * 2 + 1, nodeBeg, nodeMid, queryBeg, queryEnd);
		} else if ( nodeMid <= queryBeg ) {
			return queryRec(i * 2 + 2, nodeMid, nodeEnd, queryBeg, queryEnd);
		} else {
			assert(queryBeg < nodeMid && nodeMid < queryEnd);
			auto const& lhs = queryRec(i * 2 + 1, nodeBeg, nodeMid, queryBeg, nodeMid);
			auto const& rhs = queryRec(i * 2 + 2, nodeMid, nodeEnd, nodeMid, queryEnd);
			return Semigroup::operate(lhs, rhs);
		}
	}

public:
#ifdef _LOCAL
	void print(uint w)
	{
		std::cout << "[" << std::endl;
		uint d = 1;
		for ( size_t i = 0; i < v_.size(); ++i ) {
			if ( i != 0 ) { if ( i == (1 << d) - 1 ) { std::cout << ";" << std::endl; ++d; } else std::cout << ","; }
			std::cout << v_[i];
		}
		std::cout << "]" << std::endl;
	}
#else
	void print(int w) {}
#endif
};
template<typename T>
struct AddSemigroup
{
	static T operate(T const& lhs, T const& rhs) { return (lhs + rhs) % MOD; }
	static T unit() { return 0; }
};

int const MOD = 1000000007;

int n;
vector<ll> ds;
vector<ll> ds_prev;

//dp[i]: 問題の難しさがds[i]であるような選び方の数
//vector<ll> dp;
SegTree<ll, AddSemigroup<ll>> dp;

int solve()
{
	dp.init(n);
	rep(i, n)
	{
		dp.update(i, 1);
	}

	repi(j, 1, 4)
	{
		for ( int i = n - 1; i >= 0; --i ) {
			dp.update(i,
				dp.query(0, ds_prev[i])
			);
		}
	}
	ll total = dp.query(0, n);
	return total;
}

signed main()
{
	cin >> n;
	ds.resize(n);
	rep(i, n)
	{
		cin >> ds[i];
	}
	sort(all(ds));

	rep(i, n)
	{
		ll k = distance(
			begin(ds),
			upper_bound(all(ds), ds[i] / 2)
			);
		ds_prev.push_back(k);
	}

	cout << solve() << endl;
	return 0;
}
#endif
