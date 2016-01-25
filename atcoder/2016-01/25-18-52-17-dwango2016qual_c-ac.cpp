#include "bits/stdc++.h"
#ifdef _LOCAL
#include "local/local.hpp"
#else
#undef assert
#define assert(...) ((void)0)
#define ifdebug if (false)
#define echo(...) ((void)0)
#endif
using namespace std;
typedef long long ll;
typedef unsigned long long ull;
#define repi(_I, _B, _E) for (auto _I = (_B); (_I) < (_E); ++(_I))
#define rep(_I, _N) for (auto _I = 0; (_I) < (_N); ++(_I))
#define all(_X) (_X).begin(), (_X).end()

// 経過時間, 駅ID
using qe_t = pair<ll, int>;

// 経過時間昇順で出ていく優先度つきキュー
using pq_t = priority_queue<qe_t, vector<qe_t>, greater<qe_t>>;

struct Edge
{
	qe_t next;
	qe_t term;
};

// gr[ni]: 駅niから両隣の駅への有向辺の列
vector<vector<Edge>> gr;

int src, dst;

template<typename Fun>
auto dijkstra(int v0, Fun&& f)
-> vector<ll>
{
	auto dist = vector<ll>(gr.size(), -1);
	auto q = pq_t {};
	q.emplace(0, v0);

	while ( ! q.empty() ) {
		auto qe = q.top();
		q.pop();

		if ( dist[qe.second] >= 0 ) continue;
		dist[qe.second] = qe.first;
		echo("dist[" << qe.second << "] = " << qe.first);

		for ( auto&& e : gr[qe.second] ) {
			f(q, qe, e);
		}
	}
	return dist;
}

void read()
{
	int n, m;
	cin >> n >> m >> src >> dst;
	gr.resize(n);
	rep(ri, m)
	{
		int l;
		cin >> l;
		auto ss = vector<int>(l);
		rep(j, l)
		{
			cin >> ss[j];
		}

		auto ts = vector<ll>(l - 1);
		rep(j, l - 1)
		{
			cin >> ts[j];
		}

		auto us = vector<ll>(l, 0); // [i]: 駅iから始点までにかかるコスト
		auto acc = 0LL;
		rep(j, l - 1)
		{
			acc += ts[j];
			us[j + 1] = acc;
		}

		repi(i, 0, l - 1)
		{
			gr[ss[i]].push_back(Edge {
				{ ts[i], ss[i + 1] }
				, { us.back() - us[i], ss.back() }
			});
		}
		repi(i, 1, l)
		{
			gr[ss[i]].push_back(Edge {
				{ ts[i - 1], ss[i - 1] }
				, { us[i], ss.front() }
			});
		}
	}
}

auto solve() -> ll
{
	// r_dist[i]: 駅iからdstへの最短経路の距離
	// 寝過ごした後の移動探索のために使う
	auto r_dist = dijkstra(dst, [&] (pq_t& q, qe_t& qe, Edge& e) {
		q.emplace(qe.first + e.next.first, e.next.second);
	});

	// 二分探索
	// 寝過ごしを考慮しても mid 時間でsrc→dstへの経路があるか？を調べる
	auto lb = -1LL;
	auto ub = 1LL << 60;
	while (ub - lb >= 2)
	{
		auto mid = (ub - lb) / 2 + lb;

		// (重要なのは経路の有無)
		auto s_dist
			= dijkstra(src, [&] (pq_t& q, qe_t& qe, Edge& e) {
			// 寝過ごしても間に合う？
			if ( qe.first + e.term.first + r_dist[e.term.second] <= mid ) {
				q.emplace(qe.first + e.next.first, e.next.second);
			}
		});

		(s_dist[dst] < 0 ? lb : ub) = mid;
	}
	echo(ub);
	return ub;
}

int main()
{
	read();
	cout << solve() << endl;
	return 0;
}
