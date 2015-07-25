#include <string>
#include <vector>
#include <array>
#include <map>
#include <unordered_map>
#include <queue>
#include <algorithm>
#include <iostream>
#include <cstdio>
#include <cstring>
using namespace std;
using ull = unsigned long long; using ll = long long;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) begin(_X), end(_X)


int main()
{

	int n, m;
	cin >> n >> m;
	vector<int> bs(m);
	rep(i, m)
	{
		cin >> bs[m - i - 1];
	}

	vector<bool> fixed(n, false);
	rep(i, m)
	{
		int b = bs[i] - 1;
		if ( !fixed[b] ) {
			fixed[b] = true;
			cout << (b + 1) << endl;
		}
	}
	rep(i, n)
	{
		if ( !fixed[i] ) {
			cout << (i + 1) << endl;
		}
	}


	return 0;
}
