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
#define scani(_V) (void)std::scanf("%d", &_V)
#define printi(_V) std::printf("%d", static_cast<int>(_V))
int main() { void mymain(); /*std::ios::sync_with_stdio(false); std::cin.tie(0);*/ mymain(); return 0; }

int change1(int x, int i, int idx) {
	static int const base[] = { 1, 10, 100, 1000 };
	return x - (((x / base[idx]) % 10) * base[idx]) + i * base[idx];
}

void mymain() {
	int a, b;
	scani(a); scani(b);

	int m = -9999;
	rep(idx, 3) {
		rep(i, 10) {
			if ( i == 0 && idx == 2 )continue;
			m = max<int>(m, change1(a, i, idx) - b);
		}
	}
	rep(idx, 3) {
		rep(i, 10) {
			if ( i == 0 && idx == 2 )continue;
			m = max<int>(m, a - change1(b, i, idx));
		}
	}
	cout << m << endl;
}
