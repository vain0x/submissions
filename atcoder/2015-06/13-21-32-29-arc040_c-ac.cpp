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

int n;
string ss[100];

void invoke(int r, int c) {
	repi(j, 0, c+1) {
		ss[r][j] = 'o';
	}
	if ( r + 1 < n ) {
		repi(j, c, n) {
			ss[r + 1][j] = 'o';
		}
	}
}

signed main() {
	cin >> n;
	rep(i, n) {
		cin >> ss[i];
	}

	int  t = 0;
	rep(i, n) {
		int j = n - 1;
		for ( ; j >= 0; --j ) {
			if ( ss[i][j] == '.' ) {
				invoke(i, j);
				++t;
				break;
			}
		}
	}
	cout << t << endl;

	return 0;
}
