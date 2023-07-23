#include <bits/stdc++.h>
using namespace std;

const int MAX_SZ = 14;


// ++Interface

void tutorial(bool isRobot);

bool checkIsRobot();

void read_(int t[][MAX_SZ], int n);
    // read table and count colored cells

int count_(int t[][MAX_SZ], int n, int CNT);

void show_for_humans(int t[][MAX_SZ], int n);

void show_for_robots(int t[][MAX_SZ], int n);

void show_ans(int t[][MAX_SZ], int n, bool isRobot);

void wantCont(bool isRobot);

// -- Interface



// ++ solver

void rotate_(int t[][MAX_SZ], int n);
    //rotate table

int o(int a);
    // get opposite number(blue <-> red = 2 <-> 1)

int add1(int t[][MAX_SZ], int n, int CNT);
    // add these 1 0 1 -> 1 -1 1
    // and these 0 1 1 0 -> -1 1 1 -1

int add2(int t[][MAX_SZ], int n, int CNT);
    // add these 1 1 0 0 -> 1 1 2 2

int add3(int t[][MAX_SZ], int n, int CNT);
    //add these 1 2 1 2 and 1 2 0 0 -> 1 2 1 2 and 1 2 2 1

void fill_using_adds(int t[][MAX_SZ], int n, int CNT);
    // generate answer

// -- solver


////////////////////////////////////////////////////////
int main()
{
    cout << "Oh hi! It's 0h h1 solver.\n";

    freopen("0h h1 parsed.txt", "r", stdin);
        int n; cin >> n;
        int t[MAX_SZ][MAX_SZ] = {0};
        read_(t, n);

        int CNT = count_(t, n, CNT);

        fill_using_adds(t, n, CNT);
        show_ans(t, n, 0);
}

///////////////////////////////////////////////////////


void show_for_robots(int t[][MAX_SZ], int n)
{
    for(int i=1; i<=n; ++i, cout << endl)
        for(int j=1; j<=n; ++j)
            cout << t[i][j] << ' ';
    return;
}

void show_for_humans(int t[][MAX_SZ], int n)
{
    for(int i=1; i<=n; ++i, cout << endl)
        for(int j=1; j<=n; ++j)
            cout << (t[i][j] == 1 ? "R" : (t[i][j] == 2 ? "B" : "E")) << ' ';
    return;
}

void show_ans(int t[][MAX_SZ], int n, bool isRobot)
{
    cout << "Here you are:\n";
    if(isRobot)
        show_for_robots(t, n);
    else
        show_for_humans(t, n);
}

void rotate_(int t[][MAX_SZ], int n)
{
    for(int i=1; i<=n; ++i)
        for(int j=i+1; j<=n; ++j)
            swap(t[i][j], t[j][i]);
}

int o(int a)
{
    // get opposite number(blue -> red = 1 -> 2)
    return (a == 1 ? 2 : 1);
}

int add1(int t[][MAX_SZ], int n, int CNT)
{
    // add these 1 0 1 -> 1 -1 1
    // and these 0 1 1 0 -> -1 1 1 -1
    int cnt = 0;
    while(cnt < CNT)
    {
        cnt = CNT;
        for(int i=1; i<=n; ++i)
            for(int j=1; j<=n; ++j)
            {
                if(t[i][j] == t[i][j+1] and t[i][j] > 0)
                {
                    if(t[i][j-1] == 0)
                        t[i][j-1] = o(t[i][j]), CNT++;
                    if(t[i][j+2] == 0)
                        t[i][j+2] = o(t[i][j]), CNT++;
                }
                if(t[i][j] == t[i][j+2] and t[i][j] > 0)
                    if(t[i][j+1] == 0)
                        t[i][j+1] = o(t[i][j]), CNT++;
            }
    }
    return CNT;
}

int add2(int t[][MAX_SZ], int n, int CNT)
{
    // add these 1 1 0 0 -> 1 1 2 2
    int cnt = 0;
    while(cnt < CNT)
    {
        cnt = CNT;
        for(int i=1; i<=n; ++i)
        {
            int cn_b=0, cn_r=0;
            for(int j=1; j<=n; ++j)
            {
                if(t[i][j] == 1) cn_b++;
                if(t[i][j] == 2) cn_r++;
            }
            if(cn_b == n/2 or cn_r == n/2)
            {
                for(int j=1; j<=n; ++j)
                    if(t[i][j] == 0)
                    {
                        t[i][j] = (cn_b == n/2 ? 2 : 1);
                        CNT++;
                    }
            }
        }
    }
    return CNT;
}

int add3(int t[][MAX_SZ], int n, int CNT)
{
    //add these 1 2 1 2 and 1 2 0 0 -> 1 2 1 2 and 1 2 2 1
    int cnt=0;
    while(cnt < CNT)
    {
        cnt = CNT;
        for(int i=1; i<=n; ++i)
            for(int j=i+1; j<=n; ++j)
            {
                int cnt_i=0, cnt_j=0, cnt_m=0; // counters of colored cells in i and j
                                         // and of matches in lines
                for(int k=1; k<=n; ++k)
                {
                    if(t[i][k] > 0) cnt_i++;
                    if(t[j][k] > 0) cnt_j++;
                    if(t[i][k] == t[j][k] and t[i][k] > 0)
                                    cnt_m++;
                }

                if(((cnt_i == n and cnt_j == n-2) or
                    (cnt_i == n-2 and cnt_j == n)) and
                    (cnt_m == n-2))
                {
                    for(int k=1; k<=n; ++k)
                    {
                        if(t[i][k] == 0)
                            t[i][k] = o(t[j][k]), CNT++;

                        if(t[j][k] == 0)
                            t[j][k] = o(t[i][k]), CNT++;
                    }
                }
            }
    }
    return CNT;
}

void read_(int t[][MAX_SZ], int n)
{
    for(int i=1; i<=n; ++i)
        for(int j=1; j<=n; ++j)
        {
            char c; cin >> c;
            t[i][j] = ((c == 'E' or c == '0') ? 0 :
                       (c == 'R' or c == '1') ? 1 : 2);
        }
}
int count_(int t[][MAX_SZ], int n, int CNT)
{
    for(int i=1; i<=n; ++i)
        for(int j=1; j<=n; ++j)
        {
            CNT += (t[i][j] > 0 ? 1 : 0);
        }
    return CNT;
}

void fill_using_adds(int t[][MAX_SZ], int n, int CNT)
{
    cout << "Wait please...\n";
    int cnt = 0;
    while(cnt < CNT)
    {
        cnt = CNT;

        CNT = add1(t, n, CNT);
        CNT = add2(t, n, CNT);
        CNT = add3(t, n, CNT);

        rotate_(t, n);

        CNT = add1(t, n, CNT);
        CNT = add2(t, n, CNT);
        CNT = add3(t, n, CNT);

        rotate_(t, n);
    }
}

bool checkIsRobot()
{
    cout << "Are you a robot? Enter YES or NO: ";
    string isRobot; cin >> isRobot;
    return isRobot == "YES";
}

void tutorial(bool isRobot)
{
    cout << "Enter the table size and the table.\n"
        << "The input should look like this: \n";
    if(isRobot)
        cout << "2\n0 2\n2 1\n";
    else
        cout << "2\nE B\nB R\n";
    cout << "Your turn:\n";
    return;
}

void wantCont(bool isRobot)
{
    cout << "\nWant to solve another one?"
         << " Then enter " << (isRobot ? "1" : "YES") << ": ";
    string a; cin >> a;
    if(a != "1" and a != "YES")
    {
        cout << "BYE";
        exit(0);
    }
    else cout << "\n";
    return;
}
