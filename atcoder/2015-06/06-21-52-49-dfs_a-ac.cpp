#include <cassert>
#include <functional>
#include <ctime>
#include <cmath>
#include <climits>
#include <cstring>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
using namespace std;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

int const dx[] = { 1, 0, -1, 0 }, dy[] = { 0, 1, 0, -1 };
 
int h, w;
string cs[500 + 3];
bool dfs(int i, int j) {
	if ( cs[i][j] == 'g' ) return true;
	if ( cs[i][j] == '#' ) return false;
	cs[i][j] = '#';
	rep(d, 4) {
		if ( dfs(i + dx[d], j + dy[d]) ) return true;
	}
	return false;
}
 
signed main() {
	cin >> h >> w;
	cs[0] = cs[h + 1] = string(w + 2, '#');
	rep(i, h) {
		string s;
		cin >> s;
		cs[i + 1] = "#" + s + "#";
	}
	repi(i, 1, h + 1) repi(j, 1, w + 1) {
		if ( cs[i][j] == 's' ) {
			bool ok = dfs(i, j);
			printf("%s\n", (ok ? "Yes" : "No"));
			break;
		}
	}
	return 0;
}
