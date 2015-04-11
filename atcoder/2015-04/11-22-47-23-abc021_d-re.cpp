//#include "stdafx.h"
#include <random>
#include <memory>
#include <functional>
#include <algorithm>
#include <array>
#include <vector>
#include <map>
#include <set>
#include <string>
#include <iostream>
#include <cstdio>
#include <ctime>
#include <climits>
#include <cassert>

using namespace std;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (int _I = (_Init); _I < (_N); ++ _I)
#define rep(_I, _N) repi(_I, 0, _N)
#define all(_X) (_X).begin(), (_X).end()
#define rall(_X) (_X).rbegin(), (_X).rend()
#define lmbc(_Prms, _Body) ([&] _Prms { return (_Body); })
#define lmbp(_Prms, _Body) ([ ] _Prms { return (_Body); })
#define mkp make_pair
#define pub push_back
template<typename TInt>static std::function<TInt()> make_rand(TInt minval, TInt maxval) { return std::bind(std::uniform_int_distribution<TInt>(minval, maxval), std::mt19937(static_cast<ull>(std::time(0)))); }
template<typename T = int>std::vector<T> read_values(int n) { std::vector<T> v; v.reserve(n); T t; rep(i, n) { std::cin >> t; v.push_back(t); } return std::move(v); }

static auto const divisor = 1000000007ull;
template<ull Max = 30*30> ull combi(int n, int r) { if ( n < 0 || r < 0 ) { return 0; } if (!(n * r * (n - r))) { return 1; } assert(n <= Max && r <= Max); static ull t[Max][Max] {}; return t[n][r] ? t[n][r] : (t[n][r] = (combi<Max>(n - 1, r) + combi<Max>(n - 1, r - 1)) % divisor); }

void mymain()
{
	int n, k;
	cin >> n >> k;
	cout << (combi<10000>(n + k - 1, k)) << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
