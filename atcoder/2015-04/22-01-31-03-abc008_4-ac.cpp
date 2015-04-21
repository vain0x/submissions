#include <iostream>
#include <algorithm>
#include <cmath>
#include <map>
#include <utility>

using namespace std;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)

int n; //<= 30
int w, h; //<= 10^6
int mx[30+2], my[30+2]; //machines; 1-based; with sentinels

int dfs(int x1i, int x2i, int y1i, int y2i)
{
	auto arg = make_tuple(x1i, x2i, y1i, y2i);

	static map<tuple<int, int, int, int>, int> memo;
	auto&& it = memo.find(arg);
	if ( it != memo.end() ) return it->second;

	int const
		x1 = mx[x1i], x2 = mx[x2i],
		y1 = my[y1i], y2 = my[y2i];
	int const k0 = (x2 - x1) + (y2 - y1) - 3;

	int k = 0;
	int gm = 0;
	repi(i, 1, n+1) {
		int const x = mx[i], y = my[i];
		if ( !(x1 < x && x < x2 && y1 < y && y < y2) ) continue;
		int const lm
			= dfs(x1i, i, y1i, i)
			+ dfs(x1i, i, i, y2i)
			+ dfs(i, x2i, y1i, i)
			+ dfs(i, x2i, i, y2i)
			+ k0;
		gm = max(gm, lm);
	}
	
	memo.emplace(arg, gm);
	return gm;
}

int main() {
	std::ios::sync_with_stdio(false); std::cin.tie(0);


	cin >> w >> h >> n;
	repi(i, 1, n + 1) {
		cin >> mx[i] >> my[i];
	}
	mx[0] = my[0] = 0;
	mx[n + 1] = w + 1;
	my[n + 1] = h + 1;

	printf("%d\n", dfs(0, n + 1, 0, n + 1));


	return 0;
}
