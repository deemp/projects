#include<bits/stdc++.h>
using namespace std;

int get_size(string &s){
    regex reg ("[0-9]{1,2} x [0-9]{1,2}");
    smatch matches;
    regex_search(s, matches, reg);
    if(!matches.empty()){
        string cur = matches.str();
        string ret = "";

        for(int i = 0; isdigit(cur[i]); ++i){
            ret += cur[i];
        }

        int num = 0;

        stringstream ss;
        ss << ret, ss >> num;
        return num;
    }

    return -1;
}

vector<int> get_tile_color(string s){
    regex reg ("tile-[0-9]{1,2}-[0-9]{1,2}[^] class=[^]tile tile-[0-9]?");
    smatch matches;

    vector<int> ans;

    while(regex_search(s, matches, reg)){
        for(auto x : matches){
//            cout << x;
            string cur(x);
            ans.emplace_back(isdigit(cur.back()) ? cur.back() : '0');
        }
        s = matches.suffix().str();
    }

    return ans;
}

int main(){
    freopen("0hh1.txt", "r", stdin);
    freopen("0h h1 parsed.txt", "w", stdout);

    int n = 0;
    string s;

    int arr[20][20] = {};
    vector<int> colors;
    while(getline(cin, s)){
        int sz = get_size(s);

        if(sz > 0){
            n = sz;
        }

        auto paint = get_tile_color(s);
        colors.insert(end(colors),begin(paint), end(paint));
    }

    cout << n << endl;
    char col[] = {'E', 'R', 'B'};
    for(int i = 0; i < n; ++i){
        for(int j = 0; j < n; ++j){
            cout << col[colors[i*n + j] - '0'] << ' ';
        }
        cout << "\n";
    }
}
