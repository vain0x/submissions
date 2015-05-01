#include <random>
#include <utility>
#include <memory>
#include <functional>
#include <algorithm>
#include <deque>
#include <queue>
#include <array>
#include <vector>
#include <map>
#include <set>
#include <string>
#include <cstdio>
#include <ctime>
#include <climits>
#include <cassert>
#include <iostream>
using namespace std;
using ull = unsigned long long;
static unsigned long long const divisor = 1000000007ull;
static int const IntMax = std::numeric_limits<int>::max(), IntMin = std::numeric_limits<int>::min();
#define repi(_I, _Init, _N) for (int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
#define rrep(_I, _N) for(int _I = (_N) - 1; (_I) >= 0; --(_I))
#define all(_X) begin(_X), end(_X)
#define rall(_X) rbegin(_X), rend(_X)
#define mkp make_pair
#define mkt make_tuple
#define pub push_back
template<typename T = int>std::vector<T> read_values(int n) { std::vector<T> v; v.reserve(n); T t; for ( int i = 0; i < n; ++i ) { std::cin >> t; v.push_back(t); } return std::move(v); }

#ifdef _LOCAL
# include "for_local.h"
#else
# define echo(...) ((void)0)
#endif

int n; //<= 100
int kmax; //<= n
string s; //<:[a,z], len=n

//全候補数
//combi(n,k) * k! = n!/(k! (n-k)!) = perm(n,k)

//入力例2の方法
int const CountABs = 26;
array<int, CountABs> ab;

bool proc2_verify(int i, int k)
{
	array<int, CountABs> myab;
	copy(all(ab), begin(myab));
	repi(j, i+1, n) {
		int& a = myab[s[j] - 'a'];
		if ( a == 0 ) {
			--k;
			if ( k < 0 ) return false;
		} else {
			--a;
		}
	}
	return true;
}

string proc2()
{
	rep(i, n) {
		ab[s[i] - 'a'] ++;
	}

	int k = kmax;
	string t;
	t.resize(n);

	rep(i, n) {
		rep(j, CountABs) {
			if ( ab[j] == 0 ) continue;

			char const c = 'a' + j;
			if ( c != s[i] ) --k;
			-- ab[j];

			if ( proc2_verify(i, k) ) {
				t[i] = c;
				break;
			} else {
				if ( c != s[i] ) ++k;
				++ab[j];
			}
		}
	}
	return move(t);
}

//http://abc009.contest.atcoder.jp/tasks/abc009_c
void mymain()
{
	cin >> n >> kmax >> s;
	cout << proc2() << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); std::cout.precision(14); mymain(); return 0; }
