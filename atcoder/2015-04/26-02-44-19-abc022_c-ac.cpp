#include <map>
#include <algorithm>
#include <climits>
#include <iostream>
using namespace std;
using ull = unsigned long long;
static int const IntMax = std::numeric_limits<int>::max();
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
#define mkp make_pair

int n, m;
int const infinite = IntMax/2; //overflow対策

multimap<int,pair<ull,int>> gs;

int dp[300][300];

void mymain()
{
	cin >> n >> m;

	rep(i, n) rep(j, n) { dp[i][j] = (i == j ? 0 : infinite); }

	rep(i, m) {
		int u, v, len;
		cin >> u >> v >> len;
		u--; v--;
		gs.emplace(u, mkp(len, v));
		gs.emplace(v, mkp(len, u));

		//init dp
		if ( u != 0 && v != 0 ) {
			dp[u][v] = dp[v][u] = len;
		}
	}

	//家に繋がる道以外の道の間の経路の最短を求めておく
	//Warshall-Froyd Algorithm
	rep(k, n) rep(i, n) rep(j, n) {
		dp[i][j] = dp[j][i] = min(dp[i][j], dp[i][k] + dp[k][j]);
	}

	ull len = infinite;

	//家から出る道2つと、その間の最短経路で閉路を作る
	auto ran = gs.equal_range(0);
	int s = distance(ran.first, ran.second);
	auto e1 = ran.first;
	rep(i, s) {
		auto e2 = e1;
		repi(j, i + 1, s) {
			++e2;
			int u = e1->second.second;
			int v = e2->second.second;
			len = min(len, e1->second.first + e2->second.first + dp[u][v]);
		}
		++e1;
	}

	if ( len == infinite ) {
		cout << -1 << endl;
	} else {
		cout << len << endl;
	}
}
int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
