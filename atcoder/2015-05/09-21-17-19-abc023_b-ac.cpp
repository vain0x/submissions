#include <iostream>
#include <cstdio>
#include <string>
#include <algorithm>

using namespace std;

#define rep(_I, _N) for(unsigned int _I = 0; (_I) < (_N); ++ (_I))

int main() { void mymain(); std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }

int n;
string s;

int calc() {
	if ( (n % 2) == 0 ) return -1;

	char c0 = s[0] - 'a';
	rep(i, n/2){
		if ( s[i] != 'a' + (c0 + i) % 3) return -1;
		char const lhs = s[i];
		char const rhs = s[n - i - 1];
		switch ( lhs ) {
			case 'a': if ( rhs != 'c' ) return -1; else break;
			case 'b': if ( rhs != 'b' ) return -1; else break;
			case 'c': if ( rhs != 'a' ) return -1; else break;
		}
	}
	if ( s[n / 2] != 'b' ) return -1;

	return (n + 1) / 2 - 1;
}

void mymain() {
	cin >> n >> s;

	cout << calc() << endl;
}
