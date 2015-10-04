#include <iostream>
#include <cstdio>
#include <string>
#include <cstring>
#include <algorithm>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <queue>
#include <functional>
#include <cmath>
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# include <unordered_map>
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

int r, c, k;
vector<vector<int>> skips;



int main()
{
	cin >> r >> c >> k;

	skips.resize(r, vector<int>(c + 1));
	rep(i, r)
	{
		string s;
		cin >> s;

		int next_black_j = c;
		for (int j = c - 1; j >= 0; --j )
		{
			skips[i][1 + (j)] = next_black_j;
			if ( s[j] == 'x' ) {
				next_black_j = j;
			}
		}
		skips[i][1 + (-1)] = next_black_j;
	}

	ll cnt = 0;

	for (int i0 = k - 1; i0 + k - 1 < r; ++ i0)
	{
		for( int j0 = 0; j0 + (k * 2 - 1) <= c; ) {
			int skip = 0;
			int step = 1<<29;

			rep(d, k)
			{
				rep(b, 2)
				{
					// 着目座標
					int i = i0 + (b ? 1 : -1) * d;
					int j = j0 + d;
					echo(mkt(i, j));

					// 塗り幅
					int w = ((k - d) * 2 - 1);

					// (i,j)から右に幅 w を塗ろうとしている

					// 次の黒マスを避けるために必要な最小の移動量
					int sk = skips[i][1 + (j - 1)] - j + 1;
					if ( sk <= w ) {
						skip = max(skip, sk);
						echo("skip ||= " << sk);

					} else {
						// 次の黒マスの直前までへの移動量
						int st = skips[i][1 + (j + w - 1)] - (j + w);
						step = min(step, st);
						echo("step &&= " << st);
					}

					if ( d == 0 ) break; // 符号反転なし
				}
			}

			if ( skip > 0 ) {
				j0 += skip;
			} else {
				echo("COUNT: " << mkt(i0, j0) << " ~" << step);

				cnt += step + 1;
				j0 += step + 1;

				if ( step == 0 ) break;
			}
		}
	}



	cout << cnt << endl;

	return 0;
}