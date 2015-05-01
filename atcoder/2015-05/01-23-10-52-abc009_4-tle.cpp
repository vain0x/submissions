#include <iostream>
using namespace std;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
using uint = unsigned int;

uint k;//<=100
uint m;//<=10^9
uint as[100+1];
uint cs[100+1];

//O((m+1 - k)*k)= O(m*k): TLE
uint calc()
{
	repi(i, k, m + 1) {
		uint a = 0;
		rep(j, k) {
			a ^= cs[j] & as[(i + (k - j - 1)) % k];
		}
		as[(i % k)] = a;
	}
	return as[m % k];
}

//http://abc009.contest.atcoder.jp/tasks/abc009_4
void mymain()
{
	cin >> k >> m;
	--m;
	rep(i, k) { cin >> as[i]; }
	rep(i, k) { cin >> cs[i]; }
	cout << calc() << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); std::cout.precision(14); mymain(); return 0; }
