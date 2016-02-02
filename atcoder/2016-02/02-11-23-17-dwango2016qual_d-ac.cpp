#include "bits/stdc++.h"
#ifdef _LOCAL
# include "local/local.hpp"
#else
# undef assert
# define assert(...) ((void)0)
# define ifdebug if (false)
# define echo(...) ((void)0)
#endif
using namespace std;
typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for (auto _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for (auto _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

struct solver
{
	int h, w;
	vector<vector<ll>> board;

	// acc[i][j]:
	// 矩形領域 [0..(i, 0)]×[0..(0, j)] の綺麗さの総和
	vector<vector<ll>> acc;

	// maxs[l][r]:
	// 矩形領域 [l, r)×J の綺麗さの最大
	vector<vector<ll>> maxs;

	void init()
	{
		board = vector<vector<ll>>(h, vector<ll>(w, 0));
		acc = vector<vector<ll>>(h + 1, vector<ll>(w + 1, 0));
		maxs = vector<vector<ll>>(h, vector<ll>(h + 1, std::numeric_limits<ll>::min()));
	}

	void build()
	{
		// DP
		rep(i, h) rep(j, w)
		{
			// 包除原理
			acc[i + 1][j + 1] =
				board[i][j] + acc[i + 1][j] + acc[i][j + 1] - acc[i][j];
		}

		rep(r, h + 1) rep(l, r)
		{
			auto j_l = 0;

			repi(j_r, 1, w + 1)
			{
				// マイナスの区画を消去
				// j_l を区間 (j_l, j_r - 1) のどこかに置くという場合は枝刈りされる
				if ( sum(l, j_l, r, j_r - 1) < 0 ) j_l = j_r - 1;

				maxs[l][r] = max(maxs[l][r], sum(l, j_l, r, j_r));
			}
		}
	}

	auto sum(int i0, int j0, int i1, int j1) -> ll
	{
		// 包除原理
		return acc[i1][j1] - acc[i0][j1] - acc[i1][j0] + acc[i0][j0];
	}

	// 目的の最大値
	// ただし、異なる矩形に属す2つの区画は必ず i 成分が異なるとする
	// (転置してからもう1度これを行えば、任意の2矩形の取りかたに関する最大値が得られる)
	auto maximize() -> ll
	{
		auto ma = std::numeric_limits<ll>::min();

		repi(k, 1, h)
		{
			auto ma1 = std::numeric_limits<ll>::min();

			// 上側の庭 (i < k) での最大値
			rep(i_l, k) repi(i_r, i_l + 1, k + 1)
			{
				ma1 = max(ma1, maxs[i_l][i_r]);
			}

			auto ma2 = std::numeric_limits<ll>::min();

			// 下側の庭 (k <= i < h) での最大値
			repi(i_l, k, h) repi(i_r, i_l + 1, h + 1)
			{
				ma2 = max(ma2, maxs[i_l][i_r]);
			}

			ma = max(ma, ma1 + ma2);
		}

		return ma;
	}

	void read()
	{
		cin >> h >> w;
		init();
		rep(i, h) rep(j, w)
		{
			cin >> board[i][j];
		}
		build();
	}

	void transpose()
	{
		auto board_bak = std::move(board);
		auto acc_bak = std::move(acc);

		swap(h, w);
		init();
		rep(i, h) rep(j, w)
		{
			board[i][j] = board_bak[j][i];
		}
		build();
	}
};

auto solve() -> ll
{
	auto s = solver {};

	s.read();
	auto ma1 = s.maximize();

	s.transpose();
	return max(ma1, s.maximize());
}

int main()
{
	cout << solve() << endl;
	return 0;
}
