#include <iostream>
using namespace std;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)

int figureLength(ull x) {
	if ( x == 0 ) return 0;
	for ( int i = 0;; ++i ) {
		if ( x < 10 ) { return i + 1; }
		x /= 10;
	}
}
int figureAt(ull x, int i) {
	while ( i != 0 ) { x /= 10; --i; }
	return x % 10;
}

static int const cntLegals[11] = { 0, 1, 2, 3, 4, 4, 5, 6, 7, 8, 8 };
static int const cntIllegals[11] = { 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2 };

//x以下の違法数の個数
ull f(ull x) {
	//[上からの固定桁数][未満確定フラグ][違法確定フラグ]
	// dp[i][f1][f2]: (i+1)桁の自然数、ただし後ろに桁を補充してfl桁にしたとき(f1=1→x未満)、(f2=1→違法)の条件を満たすもの、の個数。
	ull dp[20][2][2] {};

	dp[0][0][0] = 1;

	int fl = figureLength(x);
	rep(i, fl) {
		int const d = figureAt(x, fl - i - 1);
		bool const ill = (d == 4 || d == 9);

		dp[i + 1][0][0] = (!ill ? dp[i][0][0] : 0); //xの上からi桁に違法桁がなければ1、あれば0
		dp[i + 1][0][1] = 1 - dp[i + 1][0][0];
		dp[i + 1][1][0] //未満確定済み、違法桁なし
			= dp[i][0][0] * cntLegals[d]
			+ dp[i][1][0] * cntLegals[10];
		dp[i + 1][1][1] //未満確定済み、違法桁あり
			= dp[i][0][0] * cntIllegals[d]
			+ dp[i][0][1] * d
			+ dp[i][1][0] * cntIllegals[10]
			+ dp[i][1][1] * 10;
	}
	return dp[fl][0][1] + dp[fl][1][1];
}

void mymain()
{
	ull a, b; cin >> a >> b;
	cout << f(b) - f(a - 1) << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
