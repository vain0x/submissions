#include <iostream>
#include <set>
using namespace std;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)

int main() {
	std::ios::sync_with_stdio(false); std::cin.tie(0);


	int n; cin >> n;
	set<int> fs;

	int cnt = 0;
	rep(i, n) {
		int k;
		cin >> k;

		if ( fs.find(k) != fs.end() ) {
			cnt++;
		}
		fs.emplace(k);
	}
	cout << cnt << endl;
	
	return 0;
}