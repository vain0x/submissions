#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define REP(I, B, E) for (int I = (B); (I) < (E); (I)++)
#define ARRAY_LEN(A) ((long long)(sizeof((A)) / sizeof(*(A))))

int compare_int(int l, int r) {
    if (l < r) {
        return -1;
    }
    if (l > r) {
        return 1;
    }
    return 0;
}

struct City {
    char S[11];
    int P;
    int i;
};

int compare_city(const City *l, const City *r) {
    int ord = strcmp(l->S, r->S);
    if (ord != 0) {
        return ord;
    }

    ord = compare_int(r->P, l->P); // 逆順
    if (ord != 0) {
        return ord;
    }

    return compare_int(l->i, r->i);
}

int compare(const void *l, const void *r) {
    return compare_city((const City *)l, (const City *)r);
}

City cities[101];

int main(void) {
    int N;
    scanf("%d", &N);

    REP(i, 0, N) {
        char S[11];
        int P;
        scanf("%10s %d", S, &P);

        strcpy(cities[i].S, S);
        cities[i].P = P;
        cities[i].i = i;
    }

    qsort(cities, N, sizeof(City), compare);

    REP(i, 0, N) {
        printf("%d\n", cities[i].i + 1);
    }
    return 0;
}
