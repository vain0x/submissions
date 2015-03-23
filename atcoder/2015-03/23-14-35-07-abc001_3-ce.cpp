#include <iostream>
#include <array>
#include <cmath>
#include <limits>

using namespace std;
using N = unsigned int;

int main()
{
	N deg10;
	N dist;
	cin >> deg10 >> dist;

	N speed10 = N( round(double(dist)/60 * 10) );

	static array<N, 12> const wind_index_upperbounds = { 2, 15, 33, 54, 79, 107, 138, 171, 207, 244, 284, 326 };
	N wind_index = 0;
	for ( N i = 0; i < wind_index_upperbounds.size(); ++i ) {
		if ( speed10 <= wind_index_upperbounds[i] ) { wind_index = i; break; }
	}

	char const* dir = "";
	if ( wind_index == 0 ) {
		dir = "C";
	} else {
		static array<N, 16> const dir_upperbounds = { 113, 338, 563, 788, 1013, 1238, 1463, 1688, 1913, 2138, 2363, 2588, 2813, 3038, 3263, 3488 };
		static array<char const*, 17> dirs = { "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N" };
		N dir_index = 0;
		for ( N i = 0; i < dir_upperbounds.size(); ++i ) {
			if ( deg10 < dir_upperbounds[i] ) { dir_index = i; break; }
		}
		dir = dirs[dir_index];
	}

	cout << dir << " " << wind_index;
	return 0;
}