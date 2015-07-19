//解説通り (http://http://www.slideshare.net/chokudai/abc010-35598499)
//女の子の点 ps[i] は普通の点とみなし、新しい点(n+i)を端点として加え、その監視対象(n+i)とする。
//各監視対象(n+i)からシンク(n+g)への辺を追加する。
//高橋くん(0)からシンク(n+g)への道がなくならせるために取り除くべき辺の数の最小値、つまり最小カットを求める。

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
using namespace std;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

int const INF = 1<<29;

int n, g, e;

struct edge { int to, cap, rev; };
vector<vector<edge>> G;
vector<bool> used;

//ford-fulkerson法

void add_edge(int from, int to, int cap)
{
	G[from].push_back(edge { to, cap, G[to].size() });
	G[to].push_back(edge { from, 0, G[from].size() - 1 });
}
int dfs(int v, int t, int f)
{
	if ( v == t ) return f;
	used[v] = true;
	rep(i, G[v].size())
	{
		edge& e = G[v][i];
		if ( !used[e.to] && e.cap > 0 ) {
			int const d = dfs(e.to, t, min(f, e.cap));
			if ( d > 0 ) {
				e.cap -= d;
				G[e.to][e.rev].cap += d;
				return d;
			}
		}
	}
	return 0;
}
int max_flow(int s, int t)
{
	used.resize(G.size());

	int flow = 0;
	for ( ;; ) {
		fill(all(used), false);

		int const f = dfs(s, t, INF);
		if ( f == 0 ) return flow;
		flow += f;
	}
}

int main()
{
	cin >> n >> g >> e;

	G.resize(n + g + 1);

	rep(i, g)
	{
		int p;
		cin >> p;

		add_edge(p, n + i, 1); //監視点への辺
		add_edge(n + i, n + g, 1); //シンクへの辺
	}
	rep(i, e)
	{
		int a, b;
		cin >> a >> b;
		add_edge(a, b, 1);
		add_edge(b, a, 1);
	}

	int const f = max_flow(0, n + g);
	cout << f << endl;
	return 0;
}
