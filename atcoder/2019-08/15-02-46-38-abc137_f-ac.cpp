#include <cstdio>
#include <cstring>
#include <iostream>
#include <map>
#include <set>
#include <vector>

#define REP(I, S, E) for (decltype(S + E) I = (S), _E = (E); I < _E; ++I)
#define REP_REV(I, S, E) for (decltype(S + E) I = (E)-1, _S = (S); I >= _S; --I)

using i64 = std::int64_t;
using std::cin;
using std::cout;
using std::endl;

template <typename T>
static auto vec_new(T const &init, std::size_t len) -> std::vector<T> {
    return std::vector<T>(len, init);
}

static auto pow(i64 x, i64 n, i64 P) -> i64 {
    x %= P;
    auto y = 1L;

    while (true) {
        REP(i, 0, 2) {
            if (n <= 0) {
                return y % P;
            }

            if (n % 2 != 0) {
                y *= x;
                n -= 1;
            }

            x *= x;
            n /= 2;
        }

        y %= P;
        x %= P;
    }
}

auto main() -> signed {
    i64 P;
    cin >> P;

    auto A = vec_new(0L, P);
    REP(i, 0, P) { cin >> A[i]; }

    auto fact = vec_new(0L, P);
    fact[0] = 1L;
    fact[1] = 1L;
    REP(n, 2, P) { fact[n] = n * fact[n - 1] % P; }

    auto fact_inv = vec_new(0L, P);
    REP(n, 0, P) { fact_inv[n] = pow(fact[n], P - 2, P); }

    auto combo = [&](i64 n, i64 k) {
        auto z = fact[n];
        z *= fact_inv[k];
        z *= fact_inv[n - k];
        return z % P;
    };

    auto B = vec_new(0L, P);
    REP(i, 0, P) {
        if (A[i] == 0) {
            continue;
        }

        REP(k, 0, P) {
            auto b = combo(P - 1, k);
            b *= pow((P - i), P - 1 - k, P);
            B[k] -= b;
        }

        B[0] += 1;
    }

    REP(i, 0, P) {
        if (i != 0) {
            cout << ' ';
        }

        cout << (B[i] % P + P) % P;
    }
    cout << endl;
}
