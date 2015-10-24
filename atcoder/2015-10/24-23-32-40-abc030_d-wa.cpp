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

ll str_rem(string s, ll mod)
{
	ll rem = 0;
	int i = 0;
	while ( s[i] == '0' ) ++i;

	for (; i < s.length(); ++ i ) {
		char c = s[i];
		assert(isdigit(c));
		rem = (rem * 10 + (c - '0')) % mod;
	}
	return rem;
}

int main()
{
	int n, a;
	string s;
	cin >> n >> a >> s;
	--a;
	vector<int> bs(n);
	rep(i, n)
	{
		cin >> bs[i];
		--bs[i];
	}

	ll k = atoll(s.c_str());

	vector<int> cis(n, -n - 1);
	vector<vector<int>> cycles;
	set<int> visited;
	rep(i, n)
	{
		for ( int j = i; j < n; ) {
			if ( visited.count(j) == 0 ) {
				visited.insert(j);
				cis[j] = -i - 1;
				j = bs[j];

			} else {
				if ( cis[j] != (-i - 1) ) break;

				cycles.push_back({});
				auto& c = cycles.back();
				int ci = cycles.size() - 1;
				int j0 = j;

				//j0以降は巡回
				do {
					c.push_back(j);
					cis[j] = ci;
					j = bs[j];
				} while ( j != j0 );
				break;
			}
		}
	}

	{
		int i = a;

		ll k0;
		if ( s.length() < 10 ) { // k が小さい場合は 巡回部分には入らないことがある
			k0 = atoll(s.c_str());
		} else {
			k0 = n;
		}

		//巡回に入るまで動かす
		while ( k0 > 0 && cis[i] < 0 ) {
			i = bs[i];
			--k0;
		}

		if ( k0 > 0 ) {
			assert(cis[i] >= 0);
			auto& cyc = cycles[cis[i]];
			k = str_rem(s, cyc.size());
			k = (k - (n - k0)) % cyc.size(); // 巡回部分に入るまでを加味する
			while ( k != 0 ) {
				i = bs[i]; --k;
			}
		}
		cout << (i + 1) << endl;
	}
	return 0;
}
