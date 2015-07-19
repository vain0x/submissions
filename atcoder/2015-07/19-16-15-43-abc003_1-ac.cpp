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
	double s = 0;
	rep(i, n)
	{
		s += double(10000) * (i+1) / n;
	}
	printf("%.16f\n", s);
	return 0;
}
