#include <cassert>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
using namespace std;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

int main()
{
	int n;
	cin >> n;
	cout << (n == 12 ? 1 : (n + 1)) << endl;
	return 0;
}
