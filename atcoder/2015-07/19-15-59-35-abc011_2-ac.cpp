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
	string s;
	cin >> s;
	rep(i, s.size())
	{
		s[i] = (i == 0)
			? toupper(s[i])
			: tolower(s[i]);
	}
	cout << s << endl;
	return 0;
}
