#include <algorithm>
#include <vector>
#include <iostream>
#include <cstdio>
#include <cstring>

using namespace std;
using ull = unsigned long long;
#define rep(_I, _N) for(unsigned int _I = 0; (_I) < (_N); ++ (_I))
int main() { void mymain(); std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }

ull n;
ull hs[100000 + 1];
ull ss[100000 + 1];
ull us[100000 + 1];

void mymain() {
	cin >> n;

	ull maxh = 0;
	ull maxs = 0;
	rep(i, n) {
		cin >> hs[i] >> ss[i];
		maxh = max(maxh, hs[i]);
		maxs = max(maxs, ss[i]);
	}

	ull low = maxh; //達成不可能な最大数
	ull high = maxh + maxs * n + 1; //達成可能な最小数
	rep(_, 50) {
		ull m = (high + low) / 2;

		memset(us, 0, (n + 1) * sizeof(ull));
		rep(i, n) {
			ull u = min(n, (m - hs[i]) / ss[i]); //t[i]はu以下でなければいけない
			us[u] ++;
		}

		bool ok = true;
		ull u = 0;
		rep(i, n) {
			u += us[i];
			if ( u > i + 1 ) { ok = false; break; }
		}
		if ( ok ) {
			high = m;
		} else {
			low = m;
		}
		if ( high - low <= 1 ) break;
	}

	cout << high << endl;
}
