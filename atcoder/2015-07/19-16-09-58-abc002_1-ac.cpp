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
	int x, y;
	cin >> x >> y;
	cout << max(x, y) << endl;

	return 0;
}
