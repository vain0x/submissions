#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#include <cstring>
using namespace std;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

int main()
{
	int n;
	cin >> n;

	int m = 1<<29;
	rep(i, n)
	{
		int t;
		cin >> t;
		m = min(m, t);
	}
	cout << m << endl;
	return 0;
}
