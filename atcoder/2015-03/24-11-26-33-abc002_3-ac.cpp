#include <iostream>
#include <cstdio>
#include <vector>
#include <array>
#include <algorithm>

using namespace std;
using N = unsigned int;
#define repeat(_I, _N) for(N _I = 0; _I < _N; ++_I)
#define all(_X) (_X).begin(), (_X).end()

int main()
{
#if 1
	array<int, 3> x, y;
	repeat(i, 3) {
		cin >> x[i] >> y[i];
	}
#else
	int x[] = {-1, 3, 5};
	int y[] = {-2, 4, 6};
#endif
	repeat(i, 2){
		x[i] -= x[2];
		y[i] -= y[2];
	}
	double s = double(abs( x[0] * y[1] - x[1] * y[0] )) / 2;

	printf("%.1f\n", s);
}