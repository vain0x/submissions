#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#include <cstring>
using namespace std;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

char board[4][4];

int main()
{
	rep(i, 4) rep(j, 4)
	{
		cin >> board[i][j];
	}
	rep(i, 4)
	{
		rep(j, 4)
		{
			if ( j != 0 ) cout << ' ';
			cout << board[3 - i][3 - j];
		}
		cout << endl;
	}

	return 0;
}
