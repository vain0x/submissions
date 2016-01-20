#include "bits/stdc++.h"
#ifdef _LOCAL
#include "local/local.hpp"
#else
#undef assert
#define assert(...) ((void)0)
#define ifdebug if (false)
#define echo(...) ((void)0)
#endif
using namespace std;
typedef long long ll;
typedef unsigned long long ull;
#define repi(_I, _B, _E) for (auto _I = (_B); (_I) < (_E); ++(_I))
#define rep(_I, _N) for (auto _I = 0; (_I) < (_N); ++(_I))
#define all(_X) (_X).begin(), (_X).end()

int main()
{
  int n, l;
  string s;

  cin >> n >> l >> s;

  auto cur = 1;
  auto kount = 0;
  rep(i, n) {
    switch (s[i]) {
      case  '+':
        cur ++;

        if (cur > l) {
          kount ++;
          cur = 1;
        }

        break;

      case '-':
        cur --;
        break;

      default:
        assert(false);
    }
  }

  cout << kount << endl;
  return 0;
}
