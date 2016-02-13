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

auto solve(ll n) -> int
{
	auto nicos = nico_gen();

	using dp_t = unordered_map<ll, ll>;
	auto q = queue<pair<ll, ll>>{};

	if ( n == 1 ) return 1;
	n --;

	q.emplace(1, n);

	for ( auto r = 2;; r ++ ) {
		auto q_next = queue<pair<ll, ll>>{};
		while ( ! q.empty() ) {
			ll l, n;
			tie(l, n) = q.front(); q.pop();

			for ( auto&& m : nicos ) {
				auto l_next = l * m;
				auto n_next = n - l_next;
				if ( n_next == 0 ) {
					return r;
				}
				if ( n_next < 0 ) break;
				q_next.emplace(l_next, n_next);
			}
		}
		q = move(q_next);
	}
}

int main()
{
	ll n;
	cin >> n;
	cout << solve(n) << endl;
	return 0;
}
