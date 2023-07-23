#include <bits/stdc++.h>
using namespace std;

#define fou(i, a, b) for (int i = a, _i = b; i <= _i; ++i)
#define fod(i, a, b) for (int i = a, _i = b; i >= _i; --i)

#include <./prettyprinter/pretty.hpp>

#define ll long long
#define eb emplace_back

const ll N = 1e5 + 3, M = 998244353;

template <typename T>
vector<int> ZF(T &t)
{
    int n = t.size();
    vector<int> z(n, 0);

    for (int i = 1, l = 0, r = 0; i < n; ++i)
    {
        if (i <= r)
        {
            z[i] = min(z[i - l], r - i + 1);
        }
        while (i + z[i] < n and t[i + z[i]] == t[z[i]])
        {
            z[i]++;
        }
        if (r < i + z[i] - 1)
        {
            l = i;
            r = i + z[i] - 1;
        }
    }
    return z;
}

int main()
{
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    string s = "ksumksumkso";
    dbg(ZF(s));
}