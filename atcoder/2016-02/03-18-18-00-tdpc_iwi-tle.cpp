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

static auto const iwi = string { "iwi" };

int main()
{
	string s;
	cin >> s;

	using dp_t = set<string>;

	auto dp = dp_t {};
	dp.emplace(move(s));

	for (auto k = 0;; k ++) {
		auto dp_next = dp_t {};
		for ( auto&& s : dp ) {
			for ( auto iter = s.begin();; iter++ ) {
				iter = search(iter, s.end(), all(iwi));
				if ( iter == s.end() ) { break; } 
				auto t = string(s.begin(), iter) + string(iter + 3, s.end());
				dp_next.emplace(move(t));
			}
		}
		dp = move(dp_next);

		if ( dp.empty() ) {
			cout << k <<endl;
			break;
		}
	}

	return 0;
}
