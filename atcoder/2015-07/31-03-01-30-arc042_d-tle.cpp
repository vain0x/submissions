#include <string>
#include <vector>
#include <array>
#include <map>
#include <unordered_map>
#include <queue>
#include <algorithm>
#include <iostream>
#include <cstdio>
#include <cstring>
#include <cassert>
using namespace std;
using ull = unsigned long long; using ll = long long;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) begin(_X), end(_X)

template<typename T, typename Fun>
T exp_by_squaring(T const& x, ll n, Fun const& f, T const& unit)
{
	assert(n >= 0);
	if ( n == 0 ) return unit;
	if ( n == 1 ) return x;
	T z = unit;
	T w = x;
	while ( n != 0 ) {
		if ( (n & 1) != 0 ) {
			z = f(z, w); n ^= 1;
		} else {
			w = f(w, w); n >>= 1;
		}
	}
	return std::move(z);
}

ll Mod;

using Int = ll;
static Int unit() { return 1; }
static Int inject(Int x) { return (x % Mod + Mod) % Mod; }
static Int mul(Int x, Int y) { return inject(inject(x) * inject(y)); }
static Int mpow(Int x, int n) { return exp_by_squaring(x, n, mul, unit()); }
static Int inv(Int x) { return mpow(x, Mod - 2); }

//x^k = r をみたす最小の k を返す。
//O(Mod)
int babyStepGiantStep(ll x, ll r, int n, ll lb, ll ub)
{
	std::unordered_map<ll, int> t;
	ll const m = ll(std::ceil(std::sqrt(n)));
	ll xj = unit();
	for ( int j = 0; j < m; ++j ) {
		t.emplace(xj, j);
		xj = mul(xj, x);
	}
	
	ll u = mpow(inv(x), m); //= x^(-m)
	ll c = r;
	for ( ll i = 0; i < m; ++i ) {
		auto&& it = t.find(c);
		ll const k = ( it != t.end() ? i * m + it->second : -1 );
		if ( lb <= k && k < ub ) { return k; }
		c = mul(c, u);
	}
	return -1; //解なし
}

ll x, a, b;
int solve()
{
	if ( b - a > 20000000 ) {
		rep(r, b - a)
		{
			int k = babyStepGiantStep(x, r, Mod, a, b);
			if ( k >= 0 ) return r;
		}
	}

	ll mi = Mod;
	ll z = mpow(x, a);
	repi(k, a, b)
	{
		mi = min(mi, z % Mod);
		z = mul(z, x);
	}
	return mi;
}

int main()
{
	cin >> x >> Mod >> a >> b;
	++b;
	cout << solve() << endl;
	return 0;
}
