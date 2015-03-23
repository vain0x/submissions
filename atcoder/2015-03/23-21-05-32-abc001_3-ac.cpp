#include <iostream>
#include <array>
#include <cmath>
#include <limits>

//extern void SolveARC025C();
using namespace std;
//using N = unsigned int;
typedef unsigned int N;

int main()
{
	N deg10;
	N dist;

	cin >> deg10 >> dist;

	N speed10 = N( round(double(dist)/60 * 10) );

	static array<N, 13> const wind_index_upperbounds = { 2, 15, 33, 54, 79, 107, 138, 171, 207, 244, 284, 326, numeric_limits<N>::max() };
	N wind_index = 0;
	for ( N i = 0; i < wind_index_upperbounds.size(); ++i ) {
		if ( speed10 <= wind_index_upperbounds[i] ) { wind_index = i; break; }
	}

	char const* dir = "";
	if ( wind_index == 0 ) {
		dir = "C";
	} else {
		static array<N, 16> const dir_upperbounds = { 1125, 3375, 5625, 7875, 10125, 12375, 14625, 16875, 19125, 21375, 23625, 25875, 28125, 30375, 32625, 34875 };
		static array<char const*, 17> dirs = { "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N" };
		N const deg100 = deg10 * 10;
		N dir_index = 0;
		for ( N i = 0; i < dir_upperbounds.size(); ++i ) {
			if ( deg100 < dir_upperbounds[i] ) { dir_index = i; break; }
		}
		dir = dirs[dir_index];
	}

	cout << dir << " " << wind_index << endl;

	return 0;
}
