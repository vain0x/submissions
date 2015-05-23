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
#ifdef _LOCAL
# include "for_local.h"
#endif
using namespace std;
typedef unsigned int uint; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()
#define scani(_V) (void)std::scanf("%d", &_V)
#define printi(_V) std::printf("%d", static_cast<int>(_V))
int main() { void mymain(); /*std::ios::sync_with_stdio(false); std::cin.tie(nullptr);*/ mymain(); return 0; }

int n, g, e;
vector<int> ps;
vector<pair<int, int>> edges;

//ビットセット ptn が表す枝を全部切り落としたとき、
//点 0 と連結している p 点の数(パスワードを変えるべき女の子の人数)
int calc(int ptn) {
	vector<bool> visited (n, false);
	queue<int> que;

	que.push(0);
	visited[0] = true;

	int cnt = 0;
	while ( !que.empty() ) {
		int v = que.front(); que.pop();
		rep(i, e) {
			if ( (ptn & (1 << i)) != 0 ) continue;
			if ( edges[i].first == v || edges[i].second == v ) {
				for ( auto&& u : { edges[i].first, edges[i].second } ) {
					if ( !visited[u] ) {
						que.push(u);
						visited[u] = true;
						if ( find(all(ps), u) != ps.end() ) ++cnt;
					}
				}
			}
		}
	}
	return cnt;
}

void mymain() {
	cin>> n >> g >> e;
	rep(i, g) { int p; cin >> p; ps.push_back(p); }
	rep(i, e) {
		int a, b; cin >> a >> b;
		edges.empb(a, b);
	}

	if ( e > 12 ) return;
	int m = g;
	rep(ptn, 1 << e) {
		int cntPops = 0;
		rep(i, e) { if ( ptn & (1 << i) ) ++cntPops; }
		m = min(m, calc(ptn) + cntPops);
	}
	cout << m << endl;
}
