#include <iostream>
using namespace std;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)

//http://www.slideshare.net/chokudai/abc006
//部分点2の解法
void mymain()
{
	int n; cin >> n;
	auto cs = read_values(n);

	vector<int> dp; //[i]: i番目のカードが末尾であるような部分列の最長
	dp.resize(n);

	int res = 0; //整列部分列の最長
	rep(i, n) {
		int m = 1;
		rep(j, i) {
			if ( cs[j] < cs[i] ) {
				m = max(m, dp[j] + 1);
			}
		}
		dp[i] = m;
		res = max(m, res);
	}
	cout << (n - res) << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
