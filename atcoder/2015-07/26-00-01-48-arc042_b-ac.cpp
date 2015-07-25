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
#include <complex>
using namespace std;
using ull = unsigned long long; using ll = long long;
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) begin(_X), end(_X)

using cx = complex<double>;
int n;
cx q;
vector<cx> ps;

double dist(cx q, cx p0, cx p)
{
	q -= p0;
	p -= p0;
	return abs(imag(q * abs(p) / p));
}

int main()
{
	int x0, y0;
	cin >> x0 >> y0 >> n;
	q = cx(x0, y0);
	rep(i, n)
	{
		int x, y;
		cin >> x >> y;
		ps.emplace_back(x, y);
	}

	double mi = 1e9;
	rep(i, n)
	{
		mi = min(mi, dist(q, ps[i], ps[(i + 1) % n]));
	}

	printf("%.16f", mi);
	return 0;
}
