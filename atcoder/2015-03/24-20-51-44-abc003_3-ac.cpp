#include <iostream>
#include <cstdio>
#include <vector>
#include <algorithm>

using namespace std;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (int _I = (_Init); _I < (_N); ++ _I)
#define rep(_I, _N) repi(_I, 0, _N)
#define all(_X) (_X).begin(), (_X).end()
#define rall(_X) (_X).rbegin(), (_X).rend()
#define val auto const&

int main()
{
	int count_courses, count_watchables;
	vector<int> rates;

	cin >> count_courses >> count_watchables;
	rates.reserve(count_courses);
	rep(i, count_courses) {
		int rate;
		cin >> rate;
		rates.push_back(rate);
	}

	sort(all(rates));
	double rate = 0.0;
	repi(i, count_courses - count_watchables, count_courses) {
		rate = (rate + rates[i]) / 2;
	}

	printf("%.9f\n", rate);
}