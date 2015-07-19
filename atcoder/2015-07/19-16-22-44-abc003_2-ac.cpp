#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#include <cstring>
using namespace std;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))

bool replacable(char c)
{
	static char const* s = "@atcoder";
	rep(i, strlen(s)) {
		if ( c == s[i] ) return true;
	}
	return false;
}

int main()
{
	string s,t;
	cin >> s >> t;

	bool ok = true;
	rep(i, s.size())
	{
		if ( s[i] == t[i]
			|| s[i] == '@' && replacable(t[i])
			|| t[i] == '@' && replacable(s[i]) ) {
			continue;
		}
		ok = false;
		break;
	}
	cout << (ok ? "You can win" : "You will lose") << endl;
	return 0;
}
