#include <cstdio>

template <typename TreeType, typename LazyType, size_t LEN>
class SegmentTree
{
public:
    vector<TreeType> tree;
    vector<LazyType> lazy;

    SegmentTree()
    {
        tree.resize(4 * LEN);
        lazy.resize(4 * LEN);
    }

    TreeType combine(TreeType lhs, TreeType rhs)
    {
        /*combines two tree values*/
        return lhs + rhs;
    }

    void update_vertex(int vert, LazyType val, int len)
    {
        /*updates node with lazy val (depending on len)*/
        if (val)
        {
            lazy[vert] ^= 1;
            tree[vert] = len - tree[vert];
        }
    }

    void build(TreeType (&arr)[LEN], int v = 1, int tl = 1, int tr = LEN - 1)
    {
        if (tl == tr)
        {
            tree[v] = arr[tl];
            return;
        }

        int tm = (tl + tr) / 2;

        build(arr, v * 2, tl, tm);
        build(arr, v * 2 + 1, tm + 1, tr);

        tree[v] = combine(tree[v * 2], tree[v * 2 + 1]);
    }

    void push(int v, int tl, int tr)
    {
        if (lazy[v] and tl != tr)
        {
            int tm = (tr + tl) / 2;

            update_vertex(2 * v, lazy[v], tm - tl + 1);
            update_vertex(2 * v + 1, lazy[v], tr - tm);

            lazy[v] = LazyType();
        }
    }

    TreeType query(int l, int r, int tl = 1, int tr = LEN - 1, int v = 1)
    {
        if (r < tl or tr < l or tr < tl)
        {
            return TreeType();
        }

        if (l <= tl and tr <= r)
        {
            return tree[v];
        }

        push(v, tl, tr);

        int tm = (tl + tr) / 2;

        return combine(query(l, r, tl, tm, v * 2), query(l, r, tm + 1, tr, v * 2 + 1));
    }

    void update_segment(int l, int r, LazyType val, int tl = 1, int tr = LEN - 1, int v = 1)
    {
        if (r < tl or tr < l or tr < tl)
        {
            return;
        }

        if (l <= tl and tr <= r)
        {
            update_vertex(v, val, tr - tl + 1);
            return;
        }

        push(v, tl, tr);

        int tm = (tl + tr) / 2;

        update_segment(l, r, val, tl, tm, v * 2);
        update_segment(l, r, val, tm + 1, tr, v * 2 + 1);

        if (tl != tr)
        {
            tree[v] = combine(tree[v * 2], tree[v * 2 + 1]);
        }
    }
};