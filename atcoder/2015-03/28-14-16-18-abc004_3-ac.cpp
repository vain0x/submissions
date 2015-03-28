//abc004.contest.atcoder.jp/tasks/abc004_3
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

int main()
{
	int n;
	cin >> n;
	/*
	array<ull, 6> cards { { 1, 2, 3, 4, 5, 6 } };
	rep(i, n) {
		swap(cards[(i % 5)], cards[(i % 5) + 1]);
	}
	rep(i, 6) {
		cout << cards[i];
	}
	//*/
	n %= 30;
	int const m = (n == 0 ? 5 : ((n - 1) / 5)); //注目する数
	int k = m;
	rep(i, 6) {
		cout << (i == ((n + 4) % 5 + 1)
			? m
			: (k = (k + 1) % 6)) + 1;
	}
	cout << endl;
}
