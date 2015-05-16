#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <string>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#ifndef ONLINE_JUDGE //POJ
# include <random>
# include <array>
# define mkt make_tuple
# define empb emplace_back
#endif
using namespace std;
typedef unsigned int uint; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(unsigned int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(unsigned int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()
#define scani(_V) std::scanf("%d", &_V)
#define printi(_V) std::printf("%d", static_cast<int>(_V))
int main() { void mymain(); /*std::ios::sync_with_stdio(false); std::cin.tie(0);*/ mymain(); return 0; }



void mymain() {
	int a, b;
	scani(a); scani(b);

	int m = -9999;
	repi(i, 1, 10) {
		m = max<int>( m, i * 100 + (a % 100) - b );
	}
	repi(i, 1, 10) {
		m = max<int>(m, a - (i * 100 + (b % 100)));
	}
	cout << m << endl;
}
