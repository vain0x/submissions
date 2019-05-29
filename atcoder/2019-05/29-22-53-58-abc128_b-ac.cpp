#include <cstdio>
#include <cstring>
#include <iostream>
#include <map>
#include <set>
#include <vector>
#include <algorithm>

#define REP(I, S, E) for (decltype(S + E) I = (S), _E = (E); I < _E; ++I)
#define REP_REV(I, S, E) for (decltype(S + E) I = (E)-1, _S = (S); I >= _S; --I)

using namespace std;
using ll = long long;

// T のラッパー。T に定まった順序とは逆順の大小関係を持つ。
template<typename T>
struct Rev {
    T inner_;

    auto operator <(const Rev<T>& other) const -> bool {
        return other.inner_ < inner_;
    }
};

auto main() -> int {
    int N;
    string S;
    int P;
    cin >> N;

    vector<tuple<string, Rev<int>, int>> R;
    REP(i, 0, N) {
        cin >> S >> P;
        R.emplace_back(S, Rev<int>{P}, i);
    }

    sort(begin(R), end(R));

    REP(i, 0, N) {
        cout << get<2>(R[i]) + 1 << endl;
    }
    return 0;
}
