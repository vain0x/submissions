#include <iostream>
using namespace std;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)

int n; //<= 30
int mx[30], my[30]; //machines;1-based

int calc(long ms, int x1, int y1, int x2, int y2) {
	if ( ms == 0 || x2 <= x1 || y2 <= y1 ) return 0;
	int cnt = 0;
	int cnt_base = (x2 - x1) + (y2 - y1) - 1;

	//invoke #k
	rep(k, n) {
		if ( (ms & (1 << k)) == 0 ) continue;
		if ( (ms >> k) == 0 ) break;
		int const kx = mx[k], ky = my[k];

		long div[4] {};
		rep(i, n) {
			if ( i == k || ((ms & (1<<i)) == 0) ) continue;

			div[ ((mx[i] < kx ? 1 : 0) + (my[i] < ky ? 2 : 0)) ] |= 1 << i;
		}
		
		int const cnt1 =
			+ calc(div[0], kx + 1, ky + 1, x2, y2)
			+ calc(div[1],     x1, ky + 1, kx, y2)
			+ calc(div[2], kx + 1,     y1, x2, ky)
			+ calc(div[3],     x1,     y1, kx, ky);
		if ( cnt1 > cnt ) cnt = cnt1;
	}
	return cnt_base + cnt;
}

int main() {
	std::ios::sync_with_stdio(false); std::cin.tie(0);


	int w, h; //<= 10^6
	cin >> w >> h >> n;
	rep(i, n) {
		cin >> mx[i] >> my[i];
		//mx[i]--; my[i]--;
	}

	printf("%d\n", calc((1 << n) - 1, 1, 1, w + 1, h + 1));


	return 0;
}