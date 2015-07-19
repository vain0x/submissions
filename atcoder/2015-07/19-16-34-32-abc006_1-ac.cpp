#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#include <cstring>
using namespace std;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

bool solve(int n)
{
	if ( n % 3 == 0 ) return true;
	while ( n > 0 ) {
		if ( (n % 10) % 3 == 0 ) return true;
		n /= 10;
	}
	return false;
}
int main()
{
	int n;
	cin >> n;
	cout << (solve(n) ? "YES":"NO") << endl;
	return 0;
}
