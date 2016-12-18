#include <iostream>
#include <cstdio>
#include <string>
#include <cstring>
#include <algorithm>
#include <numeric>
#include <vector>
#include <list>
#include <map>
#include <set>
#include <stack>
#include <queue>
#include <functional>
#include <cmath>
#include <memory>
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# include <unordered_map>
# include <unordered_set>
# define mkt make_tuple
#endif
#ifdef _LOCAL
# include "local/local.hpp"
#else
# define assert(...) ((void)0)
# define ifdebug if (false)
# define echo(...) ((void)0)
#endif
using namespace std;
typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

ll const MOD = 1000000007;

/**
2分累乗法(Exponentiation by squaring)
(T, f, unit): 半群
@result: x^n
@order: O(log |n|)
//*/
template<typename T, typename Fun>
T exp_by_squaring(T const& x, int n, Fun&& f, T const& unit)
{
	assert(n >= 0);
	if (n == 1) return x;
	T z = unit;
	T w = x;
	while (n != 0) {
		if ((n & 1) != 0) {
			z = f(z, w); n ^= 1;
		}
		else {
			w = f(w, w); n >>= 1;
		}
	}
	return std::move(z);
}

template<typename Int>
Int exp_by_squaring(Int x, int n)
{
	return exp_by_squaring(x, n, [](Int l, Int r) { return l * r; }, Int(1));
}

template<long long Mod> struct FiniteField
{
	using Int = long long;

	static Int unit() { return 1; }
	static Int inject(Int x) { return (x % Mod + Mod) % Mod; }
	static Int add(Int x, Int y) { return inject(inject(x) + inject(y)); }
	static Int sub(Int x, Int y) { return inject(inject(x) - inject(y)); }
	static Int mul(Int x, Int y) { return inject(inject(x) * inject(y)); }
	static Int div(Int x, Int y) { return mul(x, inv(y)); }
	static Int inv(Int x) { return pow(x, Mod - 2); }

	static Int pow(Int x, int n) { return exp_by_squaring(x, n, mul, unit()); }

	template<size_t Max>
	static Int fact(int n)
	{
		static array<Int, Max> t ={ 1, 1 };
		for (int i = 2; i <= n; ++i) {
			if (t[i] == 0) { t[i] = inject(i * t[i - 1]); }
		}
		return t[n];
	}

	//(n!)^-1
	template<size_t Max>
	static Int fact_inv(int n)
	{
		static array<Int, Max> t ={ 1, 1 };
		if (t[n] == 0) {
			t[n] = inv(fact<Max>(n));
			for (int i = n - 1; i >= 2; --i) {
				t[i] = inject(t[i + 1] * (i + 1));
			}
		}
		return t[n];
	}
	template<size_t Max>
	static Int combi(int n, int r)
	{
		if (n < r) { return 0; }
		if (r == 0) { return 1; }
		return (fact<Max>(n) * fact_inv<Max>(n - r) % Mod) * fact_inv<Max>(r) % Mod;
	}
};

bool verify(int n, vector<int> const& ds)
{
	if (n % 2 == 0)
	{
		rep(i, n)
		{
			if (ds[i] != (i / 2) * 2 + 1) return false;
		}
	}
	else
	{
		rep(i, n)
		{
			if (ds[i] != ((i + 1) / 2) * 2) return false;
		}
	}
	return true;
}

ll calc(int n, vector<int> const& ds)
{
	if (!verify(n, ds))
	{
		return 0LL;
	}

	return FiniteField<MOD>::pow(2, n / 2);
}

int main()
{
	int n;
	cin >> n;
	auto ds = vector<int>(n);
	rep(i, n)
	{
		cin >> ds[i];
	}
	sort(all(ds));
	cout << calc(n, ds) << endl;
	return 0;
}
