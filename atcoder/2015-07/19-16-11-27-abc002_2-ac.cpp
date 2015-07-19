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
	string s, t;
	cin >> s;
	rep(i, s.size())
	{
		switch ( s[i] ) {
			case 'a': case 'i': case 'u': case 'e': case 'o':
				break;
			default:
				t.push_back(s[i]);
		}
	}
	cout << t << endl;

	return 0;
}
