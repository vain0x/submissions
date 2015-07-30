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
static int mod(ll x) { return x % Mod; }
static int mul(int x, int y) { return mod(ll(mod(x)) * ll(mod(y))); }

int mpow(int x, int n)
{
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
int babyStepGiantStep(int x, int r, int n, int lb, int ub)
{
	static std::unordered_map<int, int> t;
	t.clear();

	int const m = int(std::ceil(std::sqrt(n)));
	int xj = 1;
	for ( int j = 0; j < m; ++j ) {
		t.emplace(xj, j);
		xj = mul(xj, x);
	}
	
	int const u = mpow(inv(x), m); //= x^(-m)
	int c = r;
	int const i_min = lb / m;
	int const i_max = ub / m + 1;
	for ( int i = i_min; i <= i_max; ++i ) {
		auto&& it = t.find(c);
		int const k = (it != t.end() ? i * m + it->second : -1);
		if ( lb <= k && k <= ub ) { return k; }
		c = mul(c, u);
	}
	return -1; //解なし
}

int x;
ll a, b;
int solve()
{
	if ( b - a > (1<<24) ) {
		for ( ll r = 0; r <= b - a; ++r )
		{
			int k = babyStepGiantStep(x, r, Mod, a, b);
			if ( k >= 0 ) return r;
		}
	}

	int mi = Mod;
	int z = mpow(x, a);
	for ( ll k = a; k <= b; ++k )
	{
		mi = min(mi, z);
		z = mul(z, x);
	}
	return mi;
}

int main()
{
	cin >> x >> Mod >> a >> b;
	cout << solve() << endl;
	return 0;
}
