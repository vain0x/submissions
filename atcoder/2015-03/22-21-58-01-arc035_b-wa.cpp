#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

using N = unsigned int;
using BigNum = unsigned long long int;

//階乗
template<typename TNum>
TNum fact(TNum value)
{
	TNum result = value;
	for ( TNum i = 1; i < value; ++i ) {
		result *= (value - i);
	}
	return result;
}

int main() {
	N n; //問題数
	std::cin >> n;

	std::vector<BigNum> times; //各問題を解くのにかかる時間
	times.resize(n);
	for ( N i = 0; i < n; ++i ) {
		std::cin >> times[i];
	}

	std::map<BigNum, N> table_time_duplicates; // 時間と、その時間と同一時間で解ける問題の個数
	for ( N i = 0; i < times.size(); ++i ) {
		auto const time = times[i];
		auto const it = table_time_duplicates.find(time);
		if ( it != table_time_duplicates.end() ) {
			it->second++;
		} else {
			table_time_duplicates.insert({ time, 1 });
		}
	}

	//ペナルティの計算
	BigNum sum = 0;
	BigNum past_time = 0;
	for ( auto it : table_time_duplicates ) {
		for ( N i = 0; i < it.second; ++i ) {
			past_time += it.first;
			sum += past_time;
		}
	}

	//解き方の総数
	BigNum count_way_to_solve = 1;
	for ( auto it : table_time_duplicates ) {
		count_way_to_solve *= fact<BigNum>(it.second);
	}
	count_way_to_solve %= BigNum(std::pow(10, 9)) + 7;

	//出力
	std::cout << sum << std::endl
		<< count_way_to_solve << std::endl;

	return 0;
}
