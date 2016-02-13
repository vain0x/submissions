#include "bits/stdc++.h"
#ifdef _LOCAL
# include "local/local.hpp"
#else
# undef assert
# define assert(...) ((void)0)
# define ifdebug if (false)
# define echo(...) ((void)0)
#endif
using namespace std;
typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for (auto _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for (auto _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

auto nico_gen() -> vector<ll>
{
	auto v = vector<ll>{};

	auto init =[&] (ll k) {
		assert(k == 2 || k == 5);
		while ( k >= 0 ) {
			v.push_back(k);
			if ( k % 10 == 2 ) {
				k = k * 10 + 5;
			} else {
				k = k * 10 + 2;
			}
		}
	};
	init(2);
	init(5);
	sort(all(v));
	return v;
}

auto nicos = nico_gen();

struct qe
{
	ll l, d, r;
};

struct myless
{
	bool operator()(qe const& l, qe const& r)
	{
		return l.r > r.r;
	}
};

auto bfs(ll l, ll d) -> ll
{
	if ( d == l ) return 1;

	using q_t = priority_queue<qe, vector<qe>, myless>;
	auto q = q_t {};
	q.emplace(qe { l, d, 0 });

	for ( auto r = 2;; r ++ ) {
		while ( ! q.empty() ) {
			auto e = q.top();
			q.pop();

			for ( auto&& m : nicos ) {
				auto l_next = e.l * m;
				if ( l_next > e.d ) break;
				if ( l_next == e.d ) return e.r + 1;
				if ( e.d % l_next != 0 ) continue;
				q.emplace(qe { l_next, e.d, e.r });
			}
			q.emplace(qe { e.l, e.d - e.l, e.r + 1 });
		}
	}
}

auto solve(ll n) -> int
{
	return bfs(1, n);
}

int main()
{
	ll n;
	cin >> n;
	cout << solve(n) << endl;
	return 0;
}
