#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#include <cstring>
using namespace std;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

void solve(int m)
{

	if ( m < 100 ) {
		printf("00");
	} else if ( m <= 5000 ) {
		printf("%02d", (m / 100));
	} else if ( m <= 30000 ) {
		printf("%02d", (m + 50000) / 1000);
	} else if ( m <= 70000 ) {
		printf("%02d", (m/1000 - 30)/5 + 80);
	} else if ( m > 70000 ) {
		printf("89");
	}
}

int main()
{
	int m;
	cin >> m;
	solve(m);
	printf("\n");
	return 0;
}
