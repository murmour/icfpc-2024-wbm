#pragma comment(linker,"/STACK:64000000")
#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <ctime>
#include <algorithm>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <map>
#include <set>
#include <queue>
#include <unordered_set>
#include <unordered_map>

using namespace std;

#define WR printf
//#define RE scanf
#define PB push_back
#define SE second
#define FI first

#define FOR(i,k,n) for(int i=(k); i<=(n); i++)
#define DFOR(i,k,n) for(int i=(k); i>=(n); i--)
#define SZ(a) (int)((a).size())
#define FA(i,v) FOR(i,0,SZ(v)-1)
#define RFA(i,v) DFOR(i,SZ(v)-1,0)
#define CLR(a) memset(a, 0, sizeof(a))

#define LL long long
#define VI  vector<int>
#define PAR pair<int ,int>
#define o_O 1000000000

void __never(int a){printf("\nOPS %d", a);}
#define ass(s) {if (!(s)) {__never(__LINE__);cout.flush();cerr.flush();abort();}}


string A = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n";
char B[256];

string decode( string s )
{
	string res = "";
	for (int a=1; a<(int)s.size(); a++)
		res.push_back( A[s[a]-33] );
	return res;
}

string encode( string s )
{
	string res = "S";
	for (auto c : s)
		res.push_back( B[(int)c] );
	return res;
}


int n, m;
vector< string > T;

int dx[] = { 1, -1, 0, 0 };
int dy[] = { 0, 0, 1, -1 };
string cmd = "DURL";

void clear_field()
{
	for (int a=0; a<n; a++)
		for (int b=0; b<m; b++)
			if (T[a][b]=='x')
				T[a][b] = '.';
}

string find_path( int sx, int sy )
{
	queue< pair< int, int > > Q;
	vector< string > L = T;
	vector< vector< int > > D( n, vector< int >( m, 0 ) );
	Q.push( { sx, sy } );
	L[sx][sy] = 'S';
	T[sx][sy] = 'x';

	int best = 0, ex = sx, ey = sy;
	while ( Q.size() > 0 )
	{
		auto [x,y] = Q.front();
		Q.pop();
		if (D[x][y] > best)
		{
			best = D[x][y];
			ex = x;
			ey = y;
		}
		for (int i=0; i<4; i++)
		{
			int xx = x+dx[i], yy = y+dy[i];
			if (0<=xx && xx<n && 0<=yy && yy<m)
				if (T[xx][yy]=='.')
				{
					Q.push( { xx, yy } );
					L[xx][yy] = cmd[i];
					D[xx][yy] = D[x][y]+1;
					T[xx][yy] = 'x';
				}
		}
	}

	clear_field();

	string res;
	while (L[ex][ey] != 'S')
	{
		char c = L[ex][ey];
		res.push_back( c );
		for (int a=0; a<4; a++)
			if (cmd[a]==c)
			{
				ex -= dx[a];
				ey -= dy[a];
				break;
			}
	}
	reverse( res.begin(), res.end() );
	return res;
}

void dfs( int x, int y, string & res )
{
	T[x][y] = 'x';
	for (int i=0; i<4; i++)
	{
		int xx = x+dx[i], yy = y+dy[i];
		if (0<=xx && xx<n && 0<=yy && yy<m)
			if (T[xx][yy]=='.')
			{
				res.push_back( cmd[i] );
				dfs( xx, yy, res );
				res.push_back( cmd[i ^ 1] );
			}
	}
}

int init_x, init_y;

pair<int, int> get_deltas(char c) {
	for (int i=0; i<4; i++)
		if (cmd[i]==c)
		{
			return {dx[i], dy[i]};
		}
	ass(false);
}

void run_path(const string &path) {
	auto maze = T;
	int x = init_x;
	int y = init_y;
	maze[x][y] = 'x';
	for (char c : path) {
		auto [dx, dy] = get_deltas(c);
		if (dx != 0) {
			if (x + dx >= 0 && x + dx < n && maze[x+dx][y] != '#') x += dx;
		} else {
			if (y + dy >= 0 && y + dy < m && maze[x][y+dy] != '#') y += dy;
		}
		maze[x][y] = 'x';
	}
	int filled = 0;
	int total = 0;
	for (int i = 0; i < n; i++)
		for (int j = 0; j < m; j++) {
			if (maze[i][j] == '#') continue;
			total++;
			if (maze[i][j] == 'x') filled++;
		}
	maze[x][y] = '@';
	for (auto &s : maze) {
		printf("%s\n", s.c_str());
	}
	printf("%d/%d filled\n", filled, total);
}

string take(int n, const string &s) {
	return s.substr(0, n);
}

string mul(int n, const string &s) {
	string res = "";
	for (int i = 0; i < n; i++)
		res += s;
	return res;
}

void solve21() {
	auto bigu = mul(243, "U");
	auto bigl = mul(243, "L");
	auto bigr = mul(243, "R");
	auto bigd = mul(243, "D");

	string ru_walker = mul(243, bigr + bigl + 'D');
	string lu_walker = mul(243, bigl + bigr + 'D');

	string ud_walker = mul(243, bigu + bigd + 'L');

	string special = mul(243, "LLLLLLD" + bigl + bigr);

	auto path = bigu + take(61 * 487 + 40, ru_walker) + bigu + take((132 - 57) * 487 + 80, ru_walker) + special + bigu + ru_walker;
	path += take(89, bigr) + take(152, bigu) + lu_walker;
	path += bigu + lu_walker + take(104, bigu) + take(26, bigl) + ud_walker;

	string garb;
	ifstream raw("21.raw", ifstream::in);
	string path2;
	raw >> garb >> garb >> path2;
	printf("%d %d\n", (int)path.size(), (int)path2.size());

	run_path(path);
}

string inflate(const string &s) {
	string res = "";
	for (auto c : s)
		for (int i = 0; i < 15; i++)
			res.push_back(c);
	return res;
}

int t_left[] = {1, 0, 3, 2};

string rec(int level, int t) {
	if (level == 0) return "";
	char c = "DRUL"[t];
	return rec(level - 1, t_left[t]) + rec(level - 1, t) + c + c + c + c + rec(level - 1, t) + rec(level - 1, 3 - t);
}

void solve16() {
	string path = "";
	int mulk = 2;
	auto bigu = mul(mulk, "U");
	auto bigl = mul(mulk, "L");
	auto bigr = mul(mulk, "R");
	auto bigd = mul(mulk, "D");

	string m1 = "RDL";
	string m2 = "DRU";
	string m3 = "LUR";
	string m4 = "ULD";

	string o1 = m2 + m1 + bigd + m1 + m4;
	string o2 = m1 + m2 + bigr + m2 + m3;
	string o3 = m4 + m3 + bigu + m3 + m2;
	string o4 = m3 + m4 + bigl + m4 + m1;

	string p1 = o2 + o1 + bigd + o1 + o4;
	string p2 = o1 + o2 + bigr + o2 + o3;
	string p3 = o4 + o3 + bigu + o3 + o2;
	string p4 = o3 + o4 + bigl + o4 + o1;

	string pp1 = p2 + p1 + bigd + p1 + p4;
	string pp2 = p1 + p2 + bigr + p2 + p3;
	string pp3 = p4 + p3 + bigu + p3 + p2;
	string pp4 = p3 + p4 + bigl + p4 + p1;

	string ppp1 = pp2 + pp1 + pp1 + pp4;
	string ppp2 = pp1 + pp2 + pp2 + pp3;
	string ppp3 = pp4 + pp3 + pp3 + pp2;

	//path = ppp1 + ppp2 + bigr + ppp2 + ppp3;
	path = rec(7, 1);
	run_path(path);
}

namespace p19 {

	string rec(int len, int t) {
		if (len == 0) return "";
		char c = "DRUL"[t];
		char rev = "DRUL"[(t + 2) % 4];
		return string(len, c) + rec(len / 2, (t + 3) % 4) + rec(len / 2, t) + rec(len / 2, (t + 1) % 4) + string(len, rev);
	}

	void solve19() {
		string path = "";
		for (int i = 0; i < 4; i++)
			path = path + rec(64, i);
		run_path(path);
	}
}

namespace p20 {

	string rec(int len, int t, bool f) {
		if (len == 0) return "";
		char c = "DRUL"[t];
		char rev = "DRUL"[(t + 2) % 4];
		bool f2 = !f;
		return string(len, c) + rec(len / 2, (t + 3) % 4, f2) + rec(len / 2, t, f2) + rec(len / 2, (t + 1) % 4, f2) + string(len, rev);
	}

	void solve20() {
		string path = "";
		for (int i = 0; i < 4; i++) {
			path = path + rec(64, (i + 2) % 4, false);
			path = path + "DRUL"[(i + 3) % 4];
		}
		run_path(path);
	}
}

namespace p4 {

	void solve() {
		string path = "";

		run_path(path);
	}
}

namespace p7 {
	string L(int n) { return string(n, 'L'); }
	string R(int n) { return string(n, 'R'); }
	string U(int n) { return string(n, 'U'); }
	string D(int n) { return string(n, 'D'); }

	void solve7() {
		string path = "";//"RRRRRRRDDDRRRRRRDDD" + L(25) + ;

		run_path(path);
	}
}

void solve11() {
	string path = "";
	auto bigu = mul(243, "U");
	auto bigl = mul(243, "L");
	auto bigr = mul(243, "R");
	auto bigd = mul(243, "D");

	auto pur = mul(15, "UR");
	auto prd = mul(15, "RD");
	auto pdl = mul(15, "DL");
	auto plu = mul(15, "LU");
	auto dig_r = mul(20, pur + prd);
	auto dig_l = mul(20, pdl + plu);
	auto dig_u = mul(20, plu + pur);
	auto dig_d = mul(20, prd + pdl);

	path = mul(100, dig_r + dig_d + dig_l + dig_u);

	run_path(path);
}

void solve( int id )
{
	cerr << "solve " << id << "\n";
	char buf[200];
	sprintf( buf, "../../data/in/lambdaman/%02d.in", id );
	ifstream fle( buf, ifstream::in );
	string s;
	T.clear();
	while (fle >> s) T.push_back( s );

	//for (auto & x : T) cout << x << "\n";

	n = (int)T.size();
	m = (int)T[0].size();
	int x=-1, y=-1;
	for (int a=0; a<n; a++)
		for (int b=0; b<m; b++)
			if (T[a][b]=='L')
			{
				x = a;
				y = b;
			}
	init_x = x;
	init_y = y;
	T[x][y] = '.';

	string path = find_path( x, y );
	//cout << path << "\n";

	if (id == 21) {
		solve21();
		return;
	}
	if (id == 11) {
		solve11();
		return;
	}
	if (id == 16) {
		solve16();
		return;
	}
	if (id == 19) {
		p19::solve19();
		return;
	}
	if (id == 20) {
		p20::solve20();
		return;
	}
	if (id == 7) {
		p7::solve7();
		return;
	}
	// if (id == 4) {
	// 	p4::solve();
	// 	return;
	// }

	int px = x, py = y;
	T[px][py] = 'x';
	for (auto c : path)
		for (int i=0; i<4; i++)
			if (cmd[i]==c)
			{
				px += dx[i];
				py += dy[i];
				T[px][py] = 'x';
				break;
			}

	//for (auto & x : T) cout << x << "\n";

	px = x; py = y;
	string res = "";
	dfs( px, py, res );
	for (auto c : path)
		for (int i=0; i<4; i++)
			if (cmd[i]==c)
			{
				px += dx[i];
				py += dy[i];
				T[px][py] = 'x';
				res.push_back( c );
				dfs( px, py, res );
				break;
			}
	for (int i = 0; i < n; i++)
		for (int j = 0; j < m; j++)
			ass(T[i][j] == 'x' || T[i][j] == '#');

	sprintf( buf, "solve lambdaman%d ", id );
	cout << string(buf) + res << "\n";

	//cout << encode( string(buf) + res ) << "\n";

	cout << "\n";
}

int main()
{
	//freopen("input.txt","r",stdin);
	//freopen("output.txt","w",stdout);

	for (int a=0; a<(int)A.size(); a++)
		B[ (int)A[a] ] = a+33;

	//for (int a=21; a<=21; a++)
	//	solve( a );
	solve(20);


	return 0;
}