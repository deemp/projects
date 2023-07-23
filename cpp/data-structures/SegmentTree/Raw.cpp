#include <bits/stdc++.h>
using namespace std;

#define fou(i, a, b) for (int i = a, _i = b; i <= _i; ++i)
#define fod(i, a, b) for (int i = a, _i = b; i >= _i; --i)

#define ll long long
#define eb emplace_back

const ll N = 1e5 + 3, M = 998244353;

int ST[4 * N], a[N], LZ[4 * N];

void build(int v = 1, int tl = 1, int tr = N - 1)
{
    if (tl == tr)
    {
        ST[v] = a[tl];
        return;
    }

    int tm = (tl + tr) / 2;

    build(v * 2, tl, tm);
    build(v * 2 + 1, tm + 1, tr);

    ST[v] = ST[v * 2] + ST[v * 2 + 1];
}

void push(int v, int tl, int tr)
{
    if (LZ[v] and tl != tr)
    {
        LZ[v * 2] += LZ[v];
        LZ[v * 2 + 1] += LZ[v];

        int tm = (tl + tr) / 2;

        ST[v * 2] += (tm - tl + 1) * LZ[v];
        ST[v * 2 + 1] += (tr - tm) * LZ[v];

        LZ[v] = 0;
    }
}

int query(int l, int r, int v = 1, int tl = 1, int tr = N - 1)
{
    if (tr < l or tl > r or tl > tr)
    {
        return 0;
    }

    if (l <= tl and tr <= r)
    {
        return ST[v];
    }

    push(v, tl, tr);
    int tm = (tl + tr) / 2;
    return query(l, r, v * 2, tl, tm) +
           query(l, r, v * 2 + 1, tm + 1, tr);
}

void update(int l, int r, int val, int v = 1, int tl = 1, int tr = N - 1)
{
    if (tr < l or tl > r or tl > tr)
    {
        return;
    }

    if (l <= tl and tr <= r)
    {
        ST[v] += (tr - tl + 1) * val;
        LZ[v] += val;
        return;
    }

    push(v, tl, tr);
    int tm = (tl + tr) / 2;
    update(l, r, val, v * 2, tl, tm);
    update(l, r, val, v * 2 + 1, tm + 1, tr);
}