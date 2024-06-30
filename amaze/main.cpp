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
#define RE scanf
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
		res.push_back( B[c] );
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

void solve( int id )
{
	cerr << "solve " << id << "\n";
	char buf[200];
	sprintf( buf, "..\\..\\data\\in\\lambdaman\\%02d.in", id );
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
	T[x][y] = '.';

	string path = find_path( x, y );
	//cout << path << "\n";

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

	sprintf( buf, "solve lambdaman%d ", id );
	cout << string(buf) + res << "\n";

	cout << encode( string(buf) + res ) << "\n";

	cout << "\n";
}

int main()
{
	freopen("input.txt","r",stdin);
	freopen("output.txt","w",stdout);

	for (int a=0; a<(int)A.size(); a++)
		B[ A[a] ] = a+33;

	for (int a=1; a<=20; a++)
		solve( a );
	

	return 0;
}