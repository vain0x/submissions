#include <utility>
#include <algorithm>
#include <vector>
#include <iostream>
using namespace std;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
#define mkp make_pair
using vii = vector<pair<int, int>>;

int n;
vii load() {
	vii v;
	rep(i, n) {
		int x, y;
		cin >> x >> y;
		v.emplace_back(x, y);
	}
	return v;
}

//重心と最遠点の距離 (平行移動、回転について不変な量)
double dist(vii const& a) {
	int x = 0, y = 0;
	rep(i, n) {
		x += a[i].first;
		y += a[i].second;
	}
	double bcx = double(x) / n,
		bcy = double(y) / n;

	double d = 0;
	rep(i, n) {
		d = max(d, std::hypot(a[i].first - bcx, a[i].second - bcy));
	}
	return d;
}

//http://abc022.contest.atcoder.jp/tasks/abc022_d
void mymain()
{
	cin >> n;
	auto a = load();
	auto b = load();

	cout << (dist(b) / dist(a)) << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); std::cout.precision(14); mymain(); return 0; }
