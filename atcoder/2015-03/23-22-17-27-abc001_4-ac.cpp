#include <cstdio>
#include <vector>
#include <algorithm>

using namespace std;
using N = unsigned int;
#define repeat(_I, _N) for(N _I = 0; _I < _N; ++_I)
#define all(_X) (_X).begin(), (_X).end()

int main()
{
	N count_rains;
	vector<pair<N, N>> rains;
#if 1
	scanf("%d", &count_rains);
	rains.reserve(count_rains);
	repeat(i, count_rains) {
		N begin_time, end_time;
		scanf("%d-%d", &begin_time, &end_time);
		rains.push_back({ begin_time, end_time });
	}
#else
	count_rains = 6;
	rains.assign({
		{ 1157, 1306 },
		{ 1159, 1307 },
		{ 1158, 1259 },
		{ 1230, 1240 },
		{ 1157, 1306 },
		{ 1315, 1317 }
	});
#endif

	//5分単位に丸める
	repeat(i, count_rains) {
		N& begin_time = rains[i].first;
		N& end_time = rains[i].second;
		if ( begin_time % 5 != 0 ) {
			begin_time -= begin_time % 5;
		}
		if ( end_time % 5 != 0 ) {
			end_time += 5 - (end_time % 5);
			while ( end_time % 100 >= 60 ) {
				end_time += 100 - 60;
			}
		}
	}

	vector<pair<N, bool>> time_table;
	time_table.reserve(count_rains * 2);
	for ( auto&& it : rains ) {
		time_table.push_back({ it.first, true });
		time_table.push_back({ it.second, false });
	}
	auto mycompare = [](pair<N, bool> const& lhs, pair<N, bool> const& rhs) {
		//時間の遅いほうが後
		//同じなら終わり/falseが後
		return (lhs.first < rhs.first) || (lhs.first == rhs.first && lhs.second && !rhs.second);
	};

	//タイムテーブルをまとめる
	sort(all(time_table), mycompare);

	vector<pair<N, N>> result;
	result.reserve(count_rains);
	N begin_time;
	int nest = 0;
	repeat(i, time_table.size()) {
		if ( time_table[i].second ) { //begin
			if ( nest == 0 ) { begin_time = time_table[i].first; }
			nest ++;
		} else {
			nest --;
			if ( nest == 0 ) {
				printf("%04d-%04d\n", begin_time, time_table[i].first);
			}
		}
	}

	return 0;
}
