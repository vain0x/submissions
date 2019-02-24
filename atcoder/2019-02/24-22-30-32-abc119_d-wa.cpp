#include <algorithm>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <map>
#include <set>
#include <vector>

#define REP(I, S, E) for (decltype(S + E) I = (S), _E = (E); I < _E; ++I)
#define REP_REV(I, S, E) for (decltype(S + E) I = (E)-1, _S = (S); I >= _S; --I)

using namespace std;
using ll = long long;

int A, B, Q;

ll S[100000];
ll T[100000];
ll X[100000];
pair<ll, int> Y[100000];
ll D[100000];

auto main() -> int {
    scanf("%d %d %d", &A, &B, &Q);

    REP(si, 0, A) { scanf("%lld", &S[si]); }
    REP(ti, 0, B) { scanf("%lld", &T[ti]); }
    REP(xi, 0, Q) { scanf("%lld", &X[xi]); }

    REP(q, 0, Q) { Y[q] = {X[q], q}; }
    sort(Y, Y + Q);

    // 位置 x からみてすぐ右にある神社の番号 (なければ A)
    auto si = 0;
    auto ti = 0;

    REP(yi, 0, Q) {
        auto x = Y[yi].first;
        auto q = Y[yi].second;

        while (si < A && S[si] < x) {
            si += 1;
        }
        while (ti < B && T[ti] < x) {
            ti += 1;
        }

        auto min_dist = 1LL << 60;
        REP(s, max(0, si - 1), min(A, si + 1)) {
            REP(t, max(0, ti - 1), min(B, ti + 1)) {
                auto ds = abs(S[s] - x);
                auto dt = abs(T[s] - x);
                auto dist = abs(S[s] - T[t]) + ds + dt - max(ds, dt);
                min_dist = min(min_dist, dist);
            }
        }

        D[q] = min_dist;
    }

    REP(q, 0, Q) { printf("%lld\n", D[q]); }
    return 0;
}
