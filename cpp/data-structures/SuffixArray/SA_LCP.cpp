#include <bits/stdc++.h>
using namespace std;

#define fou(i, a, b) for (int i = a, _i = b; i <= _i; ++i)
#define fod(i, a, b) for (int i = a, _i = b; i >= _i; --i)

template <typename T>
vector<int> sort_cyclic_shifts(const T(&t))
{
    int n = t.size();
    int alphabet = 300;

    vector<int> p(n), pn(n), c(n), cn(n), from(max(n, alphabet) + 1);

    fou(i, 0, n - 1)
    {
        p[i] = i;
        c[i] = t[i];
        from[c[i] + 1]++;
    }

    fou(i, 1, from.size() - 1)
    {
        from[i] += from[i - 1];
    }

#define modn(a, n) (a >= n ? a - n : a)
    for (int i = 0; i < n; i = max(i * 2, 1))
    {
        // suffixes in p are sorted lexicographically
        // from stores starts of classes
        // if multiple of same class
        // the suffix with the least second part goes first in pn
        // pn stores suffixes sorted by 2 parts

        fou(j, 0, n - 1)
        {
            int cur = modn(p[j] - i + n, n);
            pn[from[c[cur]]++] = cur;
        }

        int classes = 0;
        from[0] = 0;
        fou(j, 0, n - 1)
        {
            if (j and
                (c[pn[j]] != c[pn[j - 1]] or
                 c[modn(pn[j] + i, n)] != c[modn(pn[j - 1] + i, n)]))
            {
                ++classes;
                from[classes] = j;
            }

            cn[pn[j]] = classes;
        }

        swap(p, pn);
        swap(c, cn);
    }
#undef modn

    return p;
}

template <typename T>
vector<int> sort_suffixes(T(&t))
{
    t.push_back(0);
    vector<int> sa = sort_cyclic_shifts(t);
    t.pop_back();
    sa.erase(begin(sa));
    return sa;
}

vector<int> kasai(string &s, vector<int> &sa)
{
    int n = s.size(), k = 0;
    vector<int> lcp(n, 0);
    vector<int> c(n, 0);

    for (int i = 0; i < n; i++)
        c[sa[i]] = i;

    for (int i = 0; i < n; i++, k ? k-- : 0)
    {
        if (c[i] == n - 1)
        {
            k = 0;
            continue;
        }
        int j = sa[c[i] + 1];
        while (i + k < n && j + k < n && s[i + k] == s[j + k])
            k++;
        lcp[c[i]] = k;
    }
    return lcp;
}

int main()
{
    string s;
    cin >> s;

    vector<int> sa = sort_suffixes(s);
    for (int i : sa)
    {
        cout << s.substr(i) << "\n";
    }

    vector<int> lcp = kasai(s, sa);
}