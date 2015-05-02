#include <iostream>
#include <algorithm>
#include <vector>
#include <queue>
#include <functional>

using namespace std;
using ull = unsigned long long;
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
#define rrep(_I, _N) for(int _I = (_N) - 1; (_I) >= 0; --(_I))
#define all(_X) begin(_X), end(_X)
#define mkt make_tuple
#define pub push_back

int h, w;
bool field[100+1][100+1];
bool dp[100 + 1][100+1];

int dx[] = { 1, 0, 1 };
int dy[] = { 0, 1, 1 };

void mymain()
{
	cin >> h >> w;
	rep(i, h + 1) {
		rep(j, w + 1) {
			if ( i == h || j == w ) {
				field[i][j] = false;
			} else {
				char c;
				cin >> c;
				field[i][j] = (c == '.');
			}
		}
	}

	rrep(i, h) {
		rrep(j, w) {
			bool r = false; //置かれたら負けのマス
			rep(k, 3) {
				if ( field[i + dx[k]][j + dy[k]] && dp[i + dx[k]][j + dy[k]] == false ) {
					r = true; break;
				}
			}
			dp[i][j] = r;
		}
	}
	cout << (dp[0][0] ? "First" : "Second");
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); std::cout.precision(14); mymain(); return 0; }
