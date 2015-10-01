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
typedef complex<double> Point;

int n;
Point l1, l2;
vector<Point> ps;

// 2線分の交点
bool cross_point(Point& result, Point p1, Point p2, Point q1, Point q2)
{
	assert(p1 != p2);
	auto&& f = [&](Point q) { return (q - p1) / (p2 - p1); };
	auto&& g = [&](Point q) { return q * (p2 - p1) + p1; };

	auto&& r1 = f(q1);
	auto&& r2 = f(q2);
	if ( !(r1.imag() * r2.imag() <= 0 && r1 != r2) ) return false;

	auto&& dr = r2 - r1;
	auto&& s = Point
		( (dr.imag() == 0 ? 0 : r1.real() - r1.imag() * dr.real() / dr.imag())
		, (dr.real() == 0 ? 0 : r1.imag() - r1.real() * dr.imag() / dr.real()));
	if ( !(0 <= s.real() && s.real() <= 1) ) return false;

	result = g(s);
	return true;
}

int main()
{
	int l1x, l1y, l2x, l2y;
	cin >> l1x >> l1y >> l2x >> l2y >> n;
	l1 = Point(l1x, l1y);
	l2 = Point(l2x, l2y);
	ps.resize(n);

	rep(i, n)
	{
		int x, y;
		cin >> x >> y;
		ps[i] = Point(x, y);
	}
	ps.push_back(ps[0]);

	Point r;
	int k = 0;

	rep(i, n )
	{
		if ( cross_point(r, l1, l2, ps[i], ps[i + 1]) ) {
			ifdebug {
				echo(mkt(ps[i], ps[i+1], r));
			}
			k++;
		}
	}
	assert(k % 2 == 0);

	cout << (k / 2 + 1) << endl;
	return 0;
}
