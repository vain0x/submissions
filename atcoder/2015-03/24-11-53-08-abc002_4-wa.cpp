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
	multimap<N, N> rels;
#if 1
	cin >> count_members >> count_rels;
	repeat(i, count_rels) {
		N x, y;
		cin >> x >> y;
		rels.insert({x, y});
		rels.insert({y, x});
	}
#else
	count_members = 5;
	count_rels = 3;
	rels = multimap < N, N > { { 1, 2 }, { 2, 1 }, { 2, 3 }, { 3, 2 }, { 3, 4 }, { 4, 3 } };
#endif

	N first_person = 1;
	bool first_person_changed = false;
	N max_count = 0;
	for ( ;; ) {
		vector<N> g = { first_person };
		//first_personはそれ以前の誰とも知り合いでない
		for ( N i = first_person + 1; i <= count_members; ++i ) {
			bool able = true ;
			//議員iが現在のグループの全員と知り合い
			for ( auto&& k : g ) {
				bool ok = false;
				auto const ran = rels.equal_range(k);
				for ( auto it = ran.first; it != ran.second; ++it ) {
					if ( it->second == i ) { ok = true; break; }
				}
				if ( !ok ) { able = false; break; }
			}
			
			if ( able ) {
				g.push_back(i);
			} else {
				if ( !first_person_changed ) { first_person = i; first_person_changed = true; }
			}
		}

		if ( g.size() > max_count ) max_count = g.size();
		if ( !first_person_changed ) break;
		first_person_changed = false;
	}

	printf("%d\n", max_count);
}
