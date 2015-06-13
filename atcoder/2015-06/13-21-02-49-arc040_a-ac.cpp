#include <cmath>
#include <cstring>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
using namespace std;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

signed main() {
	int g = 0;
	string s;
	int n;
	cin >> n;
	rep(i, n) {
		cin >> s;
		rep(j, n) {
			switch ( s[j] ) {
				case 'R': ++g; break;
				case 'B': --g; break;
			}
		}
	}
	cout << (g == 0 ? "DRAW" : (g > 0 ? "TAKAHASHI" : "AOKI")) << endl;


	return 0;
}
