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

ll parse(string s)
{
	ll r = 0;
	r += (atoi(s.substr(0, 2).c_str()) - 21) * 3600;
	r += (atoi(s.substr(3, 2).c_str())) * 60;
	r += atoi(s.substr(6, 2).c_str());
	r *= 1000;
	r += atoi(s.substr(9, 3).c_str());
	return r;
}


vector<int> solve2(int N, vector<int> S, vector<int> E)
{
	vector<int> res;

	int stepmin = -10000, stepmax = 7210000;
	for ( int i = 0; i < N; ++i ) {
		if ( S[i] >= E[i] ) {
			stepmin = max(stepmin, S[i]);
			stepmax = min(stepmax, E[i] + 1000);
		}
	}
	for ( int i = 0; i < N; ++i ) {
		if ( S[i] >= E[i] ) {
			res.push_back(E[i] + 1000 - S[i]);
			continue;
		}
		int s, e;
		if ( S[i] <= stepmin - 1000 ) {
			s = S[i];
		} else if ( S[i] >= stepmax ) {
			s = S[i] + 1000;
		} else {
			res.push_back(-1);
			continue;
		}
		if ( E[i] <= stepmin - 1000 ) {
			e = E[i];
		} else if ( E[i] >= stepmax ) {
			e = E[i] + 1000;
		} else {
			res.push_back(-1);
			continue;
		}
		res.push_back(e - s);
	}
	return std::move(res);
}

vector<int> solve1(int n, vector<int> ss, vector<int> es)
{
	int can_l = -1, can_r = 1LL << 29;
	rep(i, n)
	{
		if ( ss[i] > es[i] ) {
			can_l = max(can_l, ss[i]);
			can_r = min(can_r, es[i] + 1000);
		}
	}

	vector<int> res;
	bool uniq = (can_l <= can_r);
	rep(i, n)
	{
		if ( !uniq ) break;
		if ( ss[i] > es[i] ) {
			res.push_back(1000 + es[i] - ss[i]);
		} else if (
			can_l < ss[i] && ss[i] < can_r
			|| ss[i] <= can_l && can_l < es[i] + 1000 && es[i] < can_r
			) {
			res.push_back(-1);
		} else if ( ss[i] <= can_l && can_r <= es[i] ) {
			res.push_back(es[i] - ss[i] + 1000) ;
		} else {
			res.push_back(es[i] - ss[i]) ;
		}
	}
	return std::move(res);
}

inline int calcTime(int HH, int MM, int SS, int mmm)
{
	return (((HH - 21) * 60 + MM) * 60 + SS) * 1000 + mmm;
}

signed main()
{ 
	autoTest([]() {
		int n = rnd<99>() + 1;
		int jump = rnd<1000>();

		vector<int> ss[2];
		rep(i, n)
		{
			int HH = 21;//flipCoin() ? 21 : 22;
			int MM = 0;//rnd<60>();
			int SS = rnd<5>();//rnd<60>();
			int mmm = rnd<1000>();
			int t = calcTime(HH, MM, SS, mmm);
			int span = rnd<2000>();

			ss[0].push_back(t);
			ss[1].push_back(t + span + (t < jump && jump < t + span ? -1000 : 0));
		}
		
		return mkt(n, ss[0], ss[1]);
	}, solve1, solve2, 100);

	return 0;
}
