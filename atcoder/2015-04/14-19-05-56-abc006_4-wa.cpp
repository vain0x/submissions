//http://abc006.contest.atcoder.jp/
#include <iostream>
#include <vector>

using namespace std;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)

void mymain()
{
	int n; cin >> n;
	
	int maxval = 0;
	int cnt = 0;
	rep(i, n) {
		int c; cin >> c;
		if ( maxval > c ) { cnt++; } else { maxval = c; }
	}
	cout << cnt << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
