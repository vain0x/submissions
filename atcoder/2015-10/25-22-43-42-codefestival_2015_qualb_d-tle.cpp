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

static ull const ull_max = numeric_limits<ull>::max();
ull n;

enum color_t { White = 0, Black = 1 };

// field[x]=(c, k):
// 座標 x のマスおよびその右側は k 個連続で色 c
// ただし k == 0 のときそのマスは黒
map<ull, pair<color_t, ull>> field;

ull cnt_frag = 0;

ull proc_rec(pair<color_t, ull>& cell, ull x, ull k)
{
	ull w = cell.second;
	//移動幅
	ull w_go = min(w, k);
	//残余幅
	ull w_rest = w - w_go;
	//次の位置
	ull x_next = x + (w_go == 0 ? 1 : w_go);
	//次の位置の開始セル
	auto& cell_next = field[x_next];

	//黒マスをスキップ
	if ( cell.first == Black || w_go == 0 ) {
		return proc_rec(cell_next, x_next, k);

	} else {
		ull x_next = x + w_go;
		cell = make_pair(Black, w_go);

		//白が残る場合
		if ( w_rest > 0 ) {
			++cnt_frag;
			cell_next = make_pair(White, w_rest);
		}
		return proc_rec(cell_next, x_next, k - w_go);
	}
}

ull proc(ull x, ull k)
{
	if ( k == 0 ) return x;

	auto lb = field.lower_bound(x);
	assert(x == 0 || lb != field.begin());

	if ( lb->first == x ) {
		return proc_rec(lb->second, x, k);

	} else {
		assert( lb == field.end() || lb->first > x );
		assert(lb != field.begin());
		--lb;

		ull x0 = lb->first;
		color_t c; ull w;
		tie(c, w) = lb->second;

		assert(x < x0 + w);

		//分割
		++cnt_frag;
		ull w0 = x - lb->first;
		lb->second = make_pair(c, w0);
		return proc_rec(field[x] = make_pair(c, w - w0), x, k);
	}
}

void defrag()
{
	decltype(field) field2;

	pair<ull, pair<color_t, ull>> cur;
	bool first = true;
	for ( auto&& kv : field ) {
		if ( first ) {
			first = false; cur = kv;
		} else {
			// 同色かつ隣接している場合
			if ( kv.second.first == cur.second.first
				&& kv.first == cur.first + cur.second.second ) {
				cur.second.second += kv.second.second;
			} else {
				field2.emplace(move(cur));
				cur = kv;
			}
		}
	}
	field2.emplace(move(cur));

	cnt_frag = 0;
	field = move(field2);
}

void show_field()
{
	ifdebug {
		echo("------ show field");
		for ( auto& kv : field ) {
			echo(kv.first << "~" << (kv.first + kv.second.second)
				<< ":  " << (kv.second.first == White ? "白" : "黒" )
				);
		}
	}
}

int main()
{
	cin >> n;
	field.emplace(0, make_pair(White, ull_max));

	rep(i, n)
	{
		ull s, c;
		cin >> s >> c;
		--s;

		cout << proc(s, c) << endl;

		if ( cnt_frag > 5 ) {
			defrag();
		}
		//ifdebug show_field();
	}

	return 0;
}
