#include <cassert>
#include <functional>
#include <set>
#include <ctime>
#include <cmath>
#include <climits>
#include <cstring>
#include <string>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdio>
#ifndef ONLINE_JUDGE //POJ
# include <complex>
# include <random>
# include <array>
# define mkt make_tuple
# define empb emplace_back
#endif
#ifdef _LOCAL
# include "for_local.h"
#endif
using namespace std;
typedef unsigned int uint; typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define mkp make_pair
#define all(_X) (_X).begin(), (_X).end()
inline int scani() { int n; scanf("%d", &n); return n; }


static ll const MOD = 1000000007;
static int const INF = 1<<29;
using pii = pair<int,int>;

int n; //不定セルの数
vector<int> dp;

//[index]=k: 位置 index に数字 k が書かれている
array<int, 5 * 5> board;

//[k]=index
array<int, 5 * 5> boardInv;

//[l]=index: l 番目の不定セルの位置 (昇順)
array<int, 5 * 5> pos;

//[index]=s: 位置 index の不定セルに対応する一元集合
array<int, 5 * 5> bitFromIndex;

//不定数字列と番兵
vector<int> flexnums;

int popcount(int s) {
	int k = 0;
	rep(i, 32) {
		if ( s & (1 << i) ) ++k;
	}
	return k;
}

//状態 s で数字 k を書き込む直前に、(i,j) セルが書き込み済みか否か
bool written(int s, int k, int i, int j)
{
	return (
		//固定数字が書き込み済み
		board[i * 5 + j] < k
		//書き込み済みの不定セル
		|| (s & bitFromIndex[i * 5 + j])
	);
}

bool forbidden(int s, int k, int i, int j)
{
	//両隣のうち片方だけが埋まっていたらダメ
	//どちらかが枠外ならセーフ
	if ( 1 <= i && i + 1 < 5 ) {
		bool const up = written(s, k, i - 1, j);
		bool const lo = written(s, k, i + 1, j);
		if ( up ^ lo ) return true;
	}
	if ( 1 <= j && j + 1 < 5 ) {
		bool const lef = written(s, k, i, j - 1);
		bool const rig = written(s, k, i, j + 1);
		if ( lef ^ rig ) return true;
	}
	return false;
}

//状態 s において l 番目の不定セルに数字 k が書き込まれた場合を加算
void update(int s, int k, int l) {
	int const index = pos[l];

	if ( forbidden(s, k, index / 5, index % 5) ) {
		return;
	}

	int const t = s | (1 << l);
	dp[t] = (dp[t] + dp[s]) % MOD;
}

bool verify(int s, int pc) {
	int k = (pc == 0 ? 0 : flexnums[pc - 1] + 1);
	for ( ; k < flexnums[pc]; ++k ) {
		int const index = boardInv[k];
		if ( forbidden(s, k, index / 5, index % 5) ) {
			return false;
		}
	}
	return true;
}

int solve() {
	dp.resize(1 << n + 1, 0);
	dp[0] = 1;

	rep(s, 1 << n) {
		int const pc = popcount(s);

		//固定数字を順に書き込んでいく間に禁止状態を経由しないことを確認する
		if ( !verify(s, pc) ) continue;

		int const k = flexnums[pc]; //次に書く数字
		rep(l, n) {
			if ( s & (1 << l) ) continue; //配置済み
			update(s, k, l);
		}
	}
	return dp[(1 << n) - 1];
}

signed main() {
	fill(all(boardInv), INF);

	rep(i, 5 * 5) {
		int k;
		cin >> k; --k;
		board[i] = (k >= 0 ? k : INF);

		if ( k < 0 ) { //不定セル
			bitFromIndex[i] = 1 << n;
			pos[n] = i;
			++n;
		} else { //固定セル
			boardInv[k] = i;
		}
	}
	rep(k, 5 * 5) {
		if ( boardInv[k] == INF ) {
			flexnums.push_back(k);
		}
	}
	flexnums.push_back(5 * 5);

	cout << solve() << endl;
	return 0;
}
