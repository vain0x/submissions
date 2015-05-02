#include <iostream>
#include <algorithm>
#include <vector>
#include <functional>

using namespace std;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
#define all(_X) begin(_X), end(_X)
#define pub push_back

void mymain()
{
	int n; cin>>n;
	vector<int> v;
	rep(i, n) {
		int a; cin>>a;
		v.pub(a);
	}
	sort(all(v), std::greater < int > {});

	ull first =0, second = 0;
	rep(i, n) {
		if ( (i & 1) == 0 ) {
			first += v[i];
		} else {
			//second += v[i];
		}
	}
	cout << first << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); std::cout.precision(14); mymain(); return 0; }
