#include <iostream>
#include <vector>

using namespace std;
typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for (auto _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for (auto _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

int main()
{
	int n;
	cin >> n;
	auto ks = vector<int>(n + 1);
	rep(i, n - 1)
	{
		cin >> ks[i + 1];
	}
	ks[0] = ks[1];
	ks.back() = ks[n - 1];

	rep(i, n ) {
		cout << min(ks[i], ks[i + 1]) << endl;
	}
	return 0;
}
