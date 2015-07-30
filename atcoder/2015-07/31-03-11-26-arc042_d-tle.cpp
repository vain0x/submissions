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

int Mod;
static int unit() { return 1; }
static int inject(ll x) { return x % Mod; }
static int mul(int x, int y) { return inject(ll(inject(x)) * ll(inject(y))); }

int mpow(int x, int n)
{
	if ( n == 0 ) return 1;
	if ( n == 1 ) return x;
	int z = 1;
	int w = x;
	while ( n != 0 ) {
		if ( (n & 1) != 0 ) {
			z = mul(z, w); n ^= 1;
		} else {
			w = mul(w, w); n >>= 1;
		}
	}
	return z;
}

static int inv(int x) { return mpow(x, Mod - 2); }

//x^k = r をみたす最小の k を返す。
//O(√Mod)
int babyStepGiantStep(int x, int r, int n, ll lb, ll ub)
{
	static std::unordered_map<int, int> t;
	t.clear();

	int const m = int(std::ceil(std::sqrt(n)));
	int xj = unit();
	for ( int j = 0; j < m; ++j ) {
		t.emplace(xj, j);
		xj = mul(xj, x);
	}
	
	int u = mpow(inv(x), m); //= x^(-m)
	int c = r;
	for ( int i = 0; i < m; ++i ) {
		auto&& it = t.find(c);
		int const k = (it != t.end() ? i * m + it->second : -1);
		if ( lb <= k && k <= ub ) { return k; }
		c = mul(c, u);
	}
	return -1; //解なし
}

ll x, a, b;
int solve()
{
	if ( b - a > (1<<24) ) {
		rep(r, b - a)
		{
			int k = babyStepGiantStep(x, r, Mod, a, b);
			if ( k >= 0 ) return r;
		}
	}

	int mi = Mod;
	int z = mpow(x, a);
	repi(k, a, b)
	{
		mi = min(mi, z);
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
