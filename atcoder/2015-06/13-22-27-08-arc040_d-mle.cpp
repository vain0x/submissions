#include <cmath>
#include <cstring>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#include <queue>
#include <set>
using namespace std;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define mkt make_tuple
#define all(_X) (_X).begin(), (_X).end()

int const INF = 1<<29;

int si = INF, sj = INF;
int len;

int n;
string ss[400+1];

using pii = pair<int,int>;
struct Dat { pii p; bool vert; set<pii> ts; };

int dx[] = { 1, -1, 0, 0 }, dy[] = { 0, 0, 1, -1 };

bool dfs(int i, int j, bool vert, set<pii> ts) {
	if ( ts.size() == len ) return true;
	ts.insert(mkp(i, j));
		
	int c = (vert ? 0 : 2);
	repi(k, c, c + 2) {
		int i2 = i + dx[k], j2 = j + dy[k];
		if ( ss[i2][j2] == '#'
			|| ts.count(mkp(i2, j2)) != 0) continue;
		if ( dfs(i2, j2, !vert, ts) ) return true;
	}
	return false;
}

signed main() {

	cin >> n;
	rep(i, n) {
		string s;
		cin >> s;

		ss[i+1] = "#" + s + "#";
	}
	ss[0] = ss[n+1] = string(n + 3, '#');

	len = 0;
	repi(i, 1, n+1) repi(j, 1,n+1) {
		if ( ss[i][j] == '.' ) ++len;
		if ( ss[i][j] == 's' ) {si = i; sj = j;}
	}

	bool ok = false;
	rep(i, 2) {
		if ( dfs(si, sj, i == 0, {}) ) { ok = true; break; }
	}
	cout << (ok ? "POSSIBLE" : "IMPOSSIBLE") << endl;

	return 0;
}
