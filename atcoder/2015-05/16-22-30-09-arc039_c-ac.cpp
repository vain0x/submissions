#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <string>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#ifndef ONLINE_JUDGE //POJ
# include <random>
# include <array>
# define mkt make_tuple
# define empb emplace_back
#endif
using namespace std;
typedef unsigned int uint; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(unsigned int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(unsigned int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()
#define scani(_V) (void)std::scanf("%d", &_V)
#define printi(_V) std::printf("%d", static_cast<int>(_V))
int main() { void mymain(); /*std::ios::sync_with_stdio(false); std::cin.tie(0);*/ mymain(); return 0; }

static int const dx[] = { 1, 0, -1, 0 }, dy[] = { 0, 1, 0, -1 };
struct way {
	pair<int,int> next[4];
};
map<pair<int,int>, way> m;

int readdir() {
	char c;
	scanf("%c", &c);
	switch ( c ) {
		case 'R': return 0;
		case 'U': return 1;
		case 'L': return 2;
		case 'D': return 3;
	}
}
pair<int, int> p = { 0, 0 };

pair<int,int> explore(pair<int, int>& q, int d) {
	auto&& it = m.find(q);
	if ( it != m.end() ) {
		return it->second.next[d] = explore(it->second.next[d], d);
	} else {
		return q;
	}
}

void explore() {
	int& x = p.first;
	int& y = p.second;
	way w;
	rep(d, 4) {
		auto q = mkp(x + dx[d], y + dy[d]);
		w.next[d] = explore(q, d);
	}
	m.emplace(p, w);
}

void mymain() {
	int k;
	scani(k);
	explore();
	char crlf;
	scanf("%c", &crlf);

	rep(i, k) {
		int d = readdir();
		p = m.at(p).next[d];
		explore();
	}
	cout << p.first << " " << p.second << endl;
}
