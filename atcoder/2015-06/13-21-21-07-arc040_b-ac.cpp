#include <cmath>
#include <cstring>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
using namespace std;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

int const INF = 1<<29;

signed main() {
	int n, r;
	string s;
	cin >> n >> r >> s;

	int t = 0;
	int i = n-1;
	int p = INF;
	while ( i >= 0 ) {
		if ( s[i] == '.' ) {
			int j = i;
			i = max(0, i - r + 1);
			repi(k, i, j + 1) {
				s[k] = 'o';
			}
			t += 1 + (p == INF ? 0 : p - i);
			p = i;
			if ( i == 0 ) break;
		} else {
			--i;
		}
	}
	if ( p != INF ) t += p;

	cout << t << endl;
	return 0;
}
