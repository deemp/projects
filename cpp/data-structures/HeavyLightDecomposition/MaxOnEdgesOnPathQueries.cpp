// There are weights of edges into parent instead of values in the nodes
// 1st node on heavy path is excluded
// Don't forget to use vertex values and include 1st node

#include <bits/stdc++.h>
using namespace std;

#define fou(i, a, b) for (int i = a, _i = b; i <= _i; ++i)
#define fod(i, a, b) for (int i = a, _i = b; i >= _i; --i)

#include <./prettyprinter/pretty.hpp>

#define ll long long
#define eb emplace_back

const ll N = 103, M = 998244353;

vector<vector<pair<int, ll>>> g(N);
vector<int> parent, depth, heavy, head, pos, parent_weight;

int dfs(int v = 1)
{
    int sz = 1;
    int max_sub_size = 0;
    depth[v] = depth[parent[v]] + 1;

    for (pair<int, int> e : g[v])
    {
        int child = e.first, weight = e.second;

        if (child != parent[v])
        {
            parent[child] = v;
            parent_weight[child] = weight;

            int sub_size = dfs(child);

            sz += sub_size;

            if (sub_size > max_sub_size)
            {
                max_sub_size = sub_size;
                heavy[v] = child;
            }
        }
    }

    return sz;
}

int cur_pos = 1;
void decompose(int v = 1, int h = 1)
{
    head[v] = h;
    pos[v] = cur_pos++;

    if (heavy[v] != 0)
    {
        decompose(heavy[v], h);
    }

    for (pair<int, int> e : g[v])
    {
        int child = e.first, weight = e.second;

        if (child != parent[v] and child != heavy[v])
        {
            decompose(child, child);
        }
    }
}

vector<int> MAX(2 * N + 3);
void build_MAX()
{
    fou(i, 1, N)
    {
        MAX[pos[i] + N] = parent_weight[i];
    }
    fod(i, N, 1)
    {
        MAX[i] = max(MAX[i * 2], MAX[i * 2 + 1]);
    }
}

void update_MAX(int x, int y, int w)
{
    if (depth[x] > depth[y])
    {
        swap(x, y);
    }

    int p = pos[y] + N;
    for (MAX[p] = w; p > 1; p >>= 1)
    {
        MAX[p >> 1] = max(MAX[p], MAX[p ^ 1]);
    }
}

int query_MAX(int l, int r)
{
    int ans = 0;
    if (l > r)
    {
        return 0;
    }
    for (l += N, r += N + 1; l < r; l >>= 1, r >>= 1)
    {
        if (l & 1)
        {
            ans = max(ans, MAX[l++]);
        }
        if (r & 1)
        {
            ans = max(ans, MAX[--r]);
        }
    }
    return ans;
}

int query(int x, int y)
{
    int ret = 0;

    for (; head[x] != head[y]; y = parent[head[y]])
    {
        if (depth[head[x]] > depth[head[y]])
        {
            swap(x, y);
        }

        int cur_heavy_path_max = query_MAX(pos[head[y]], pos[y]);
        ret = max(ret, cur_heavy_path_max);
    }

    if (depth[x] > depth[y])
    {
        swap(x, y);
    }

    //    dbgm(pos[x], pos[y]);
    int last_heavy_path_max = query_MAX(pos[x] + 1, pos[y]);
    ret = max(ret, last_heavy_path_max);

    return ret;
}

template <class T>
void init(T &g)
{
    int n = g.size();
    parent = vector<int>(n);
    depth = vector<int>(n);
    heavy = vector<int>(n);
    head = vector<int>(n);
    pos = vector<int>(n);
    parent_weight = vector<int>(n);

    cur_pos = 1;
    dfs();
    decompose(1, 1);

    build_MAX();
}

int main()
{
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    //    #ifdef __WIN32
    //    freopen("test.txt", "r", stdin);
    //    #endif

    int n;
    cin >> n;

    fou(i, 1, n - 1)
    {
        int u, v, w;
        cin >> u >> v >> w;
        g[u].eb(v, w);
        g[v].eb(u, w);
    }

    init(g);

    //    dbg(parent, 1, n);
    //    dbg(head,1,n);
    dbg(pos, 1, n);

    int arr[N];
    fou(i, 1, n)
    {
        arr[pos[i]] = i;
    }
    dbg(arr, 1, n);
    dbg(parent_weight, 1, n);
    //    dbgm(query_MAX(1,2), query_MAX(2,3));

    //    cout << query_MAX(pos[2], pos[14]);
    //    dbg()

    while (true)
    {
        int t, x, y, w;
        cin >> t >> x >> y;
        if (t == 2)
        {
            cin >> w;
            update_MAX(x, y, w);
        }
        else
        {
            cout << query(x, y) << endl;
        }
    }
}