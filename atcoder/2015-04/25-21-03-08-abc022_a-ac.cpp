#include <iostream>
using namespace std;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)

int main() {
	std::ios::sync_with_stdio(false); std::cin.tie(0);


	int n, s, t, w;
	cin >> n >> s >> t >> w;
	int cnt = 0;
	if ( s <= w && w <= t ) cnt++;
	rep(i, n-1) {
		int a;
		cin >> a;
		w += a;

		if ( s <= w && w <= t ) cnt++;
	}
	cout << cnt << endl;

	return 0;
}