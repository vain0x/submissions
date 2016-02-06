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

string s;

auto split(string const& src, string delim) -> vector<string>
{
	auto v = vector<string>{};
	auto iter = src.begin();
	auto iter_prev = src.begin();
	while (iter_prev != src.end()) {
		iter = std::find_first_of(iter_prev, src.end(), all(delim));

		v.emplace_back(iter_prev, iter);

		if ( iter == src.end() ) break;
		iter_prev = iter + delim.size();
	}
	return v;
}

int main()
{
	cin >> s;
	auto terms = split(s, "+");
	auto k = 0;
	for ( auto&& term : terms ) {
		auto digits = split(term, "*");
		auto is_zero = false;
		for ( auto&& digit : digits ) {
			is_zero = is_zero || digit == "0";
		}
		if ( ! is_zero ) { k ++; }
	}
	cout << k << endl;
	return 0;
}
