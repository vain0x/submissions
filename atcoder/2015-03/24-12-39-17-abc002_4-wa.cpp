//abc001.contest.atcoder.jp/tasks/abc002_4
#include <iostream>
#include <cstdio>
#include <vector>
#include <array>
#include <map>
#include <algorithm>

using namespace std;
using N = unsigned int;
#define repeat(_I, _N) for(N _I = 0; _I < _N; ++_I)
#define all(_X) (_X).begin(), (_X).end()

int main()
{
	N count_members, count_rels;
	vector<pair<N, N>> rels;

	cin >> count_members >> count_rels;
	repeat(i, count_rels) {
		N x, y;
		cin >> x >> y;
		rels.emplace_back(make_pair(x, y));
		rels.emplace_back(make_pair(y, x));
	}

	N first_person = 1;
	N next_person =0; //次のグループの起点
	N max_count = 1;
	for(;;) {
		vector<N> g = { first_person };
		for ( N i = 1; i <= count_members; ++i ) {

			//現時点の各グループメンバ j について i と j が知り合い、であればiは構成員になる
			bool able = all_of(all(g), [&](N j) {
				return i == j || find(all(rels), make_pair(i, j)) != rels.end();
			});
			
			if ( able ) {
				g.push_back(i);
			} else {
				if ( i > first_person && next_person == 0 ) { next_person = i; }
			}
		}

		if ( g.size() > max_count ) max_count = g.size();
		if ( next_person == 0 ) break;
		first_person = next_person; next_person = 0;
	}

	printf("%d\n", max_count);
}
