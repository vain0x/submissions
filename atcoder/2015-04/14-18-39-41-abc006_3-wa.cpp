#include <iostream>
using namespace std;

#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)

void mymain()
{
	int n, m;
	cin >> n >> m;
	rep(x, n) {
		int y = 4 * n - m - 2 * x;
		int z = n - x - y;
		if ( x >= 0 && y >= 0 && z >= 0 && x + y + z == n && 2 * x + 3 * y + 4 * z == m ) {
			cout << x << ' ' << y << ' ' << z << endl;
			return;
		}
	}
	cout << "-1 -1 -1" << endl;
}
int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
