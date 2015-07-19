#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#include <cstring>
using namespace std;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

int const MOD = 10007;

int main()
{
	int n;
	cin >> n;
	--n;

	vector<int> as = { 0, 0, 1 };
	for ( int i = 3; i < n + 1; ++i ) {
		as.push_back(
			((as[i - 1] + as[i - 2]) % MOD + as[i - 3]) % MOD
		);
	}
	cout << as[n] << endl;
	return 0;
}
