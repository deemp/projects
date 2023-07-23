#include <stdio.h>
#include <string>
#include <bitset>

using namespace std;

template <typename T>
int SIZE(T(&t))
{
    return t.size();
}

template <typename T, size_t N>
int SIZE(T (&t)[N])
{
    return N;
}

string to_string(char t)
{
    return "'" + string({t}) + "'";
}

string to_string(bool t)
{
    return t ? "true" : "false";
}

string to_string(const string &t, int x = 0, int y = 1e9)
{
    x = min(x, SIZE(t));
    y = min(y, SIZE(t) - 1);

    string ret = "";
    for (; x <= y; ++x)
    {
        ret += t[x];
    }
    return '"' + ret + '"';
}

string to_string(const char *t)
{
    string ret(t);
    return to_string(ret);
}

template <size_t N>
string to_string(const bitset<N> &t, int x = 0, int y = 1e9)
{
    x = min(x, SIZE(t));
    y = min(y, SIZE(t) - 1);

    string ret = "";
    for (; x <= y; ++x)
    {
        ret += t[x] + '0';
    }
    return '"' + ret + '"';
}

template <typename T, typename... Coords>
string to_string(const T(&t), int x = 0, int y = 1e9, Coords... C);

template <typename T, typename S>
string to_string(const pair<T, S> &t)
{
    return "(" + to_string(t.first) + ", " + to_string(t.second) + ")";
}

template <typename T, typename... Coords>
string to_string(const T(&t), int x, int y, Coords... C)
{
    x = min(x, SIZE(t));
    y = min(y, SIZE(t) - 1);

    string ret = "[";
    auto e = begin(t);
    advance(e, x);
    for (; x <= y; ++x)
    {
        ret += to_string(*e, C...) + (x != y ? ", " : "");
        e = next(e);
    }
    return ret + "]";
}

template <int Index, typename... Ts>
struct print_tuple
{
    string operator()(const tuple<Ts...> &t)
    {
        string ret = print_tuple<Index - 1, Ts...>{}(t);
        ret += (Index ? ", " : "");
        return ret + to_string(get<Index>(t));
    }
};
template <typename... Ts>
struct print_tuple<0, Ts...>
{
    string operator()(const tuple<Ts...> &t) { return to_string(get<0>(t)); }
};
template <typename... Ts>
string to_string(const tuple<Ts...> &t)
{
    const auto Size = tuple_size<tuple<Ts...>>::value;
    return print_tuple<Size - 1, Ts...>{}(t);
}


void dbgr() { ; }
template <typename Heads, typename... Tails>
void dbgr(Heads H, Tails... T)
{
    cout << to_string(H) << " | ";
    dbgr(T...);
}

void dbgm() { ; }
template <typename Heads, typename... Tails>
void dbgm(Heads H, Tails... T)
{
    cout << H << " | ";
    dbgm(T...);
}

void dbgs() { ; }
template <typename Heads, typename... Tails>
void dbgs(Heads H, Tails... T)
{
    cout << H << " ";
    dbgs(T...);
}

#define dbgv(...) cout << to_string(__VA_ARGS__) << endl;
#define dbg(...)                          \
    cout << "[" << #__VA_ARGS__ << "]: "; \
    dbgv(__VA_ARGS__);
#define dbgp(...)                         \
    cout << "[" << #__VA_ARGS__ << "]: "; \
    dbgr(__VA_ARGS__);                    \
    cout << " ";
#define dbgr(...)      \
    dbgr(__VA_ARGS__); \
    cout << endl;
#define dbgm(...)                         \
    cout << "[" << #__VA_ARGS__ << "]: "; \
    dbgr(__VA_ARGS__);