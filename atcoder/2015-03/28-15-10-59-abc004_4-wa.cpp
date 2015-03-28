//abc004.contest.atcoder.jp/tasks/abc004_4
#include <iostream>
#include <climits>
#include <cstdio>
#include <ctime>
#include <cassert>
#include <vector>
#include <array>
#include <map>
#include <set>
#include <string>
#include <algorithm>
#include <random>
#include <functional>

using namespace std;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (int _I = (_Init); _I < (_N); ++ _I)
#define rep(_I, _N) repi(_I, 0, _N)
#define all(_X) (_X).begin(), (_X).end()
#define rall(_X) (_X).rbegin(), (_X).rend()
#define let auto const&
#define lmbc(_Prms, _Body) ([&] _Prms { return (_Body); })
#define lmbp(_Prms, _Body) ([ ] _Prms { return (_Body); })
#define mkp make_pair
#define pub push_back

int calc(int left, int right)
{
	//左にleft個、中央と右にright個あるように移す
	if ( left < 0 ) { return (left + right) * (-left) + calc(0, left + right); }
	if ( right <= 0 ) { return calc(right, left); }
	return ((left + 1) * left / 2)
		+ ((right) * (right - 1) / 2);
}

int main()
{
	int r, g, b;
	cin >> r >> g >> b;

		//緑の領域
		int const g_left = g / 2;
		int const g_right = g - g_left;

		//赤の相対領域
		int const r_right = min(r / 2, 100 - g_left);
		int const r_left = r - r_right;

		//青の相対領域
		int const b_left = min(b / 2, 100 - g_right);
		int const b_right = b - b_left;

		int const result
			= calc(r_right, r_left)
			+ calc(g_left, g_right)
			+ calc(b_left, b_right);

		cout << result << endl;
}
