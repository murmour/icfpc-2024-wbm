#pragma comment(linker,"/STACK:64000000")
#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
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
#include <random>

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

random_device rdev;
mt19937 gmt(rdev());

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

map< string, vector< vector< string > > > G;

// hack: negative numbers or big integers can be inserted as initialization of variables

void init_gadgets()
{
	G["op"] = { { // a=op b=input1 c=input2 d=output
		" c ",
		"ba.",
		" . ",
		"d@d",
		" 1 " } };
	G["ops"] = { { // a=op b=input1 c=input2   b op c -> S
		" c ",
		"baS",
		" . " } };
	G["copyop"] = { { // a=op b=input1 c=input2 d=output1 e=output2  b -> d   b op c -> e
		"    c ",
		" .<ba.",
		"d@d . ",
		" 1 e@e",
		"    1 " } };
	G["copy3"] = { { // a=input b=output1 c=output2 d=output3
		" .<a>. ",
		"b@bvd@d",
		" 1 . 1 ",
		"  c@c  ",
		"   1   " } };
	G["copy3t2"] = { { // a=old_input b=new_input c=output1 d=output2 e=output3  2ticks
		"   a   ",
		"   v   ",
		" .<b>. ",
		"c@cvd@d",
		" 1 . 1 ",
		"  e@e  ",
		"   1   " } };
	G["copy2"] = { { // a=input b=output1 c=output2
		" a>. ",
		" vc@c",
		" . 1 ",
		"b@b  ",
		" 1   " }, {
		" .<a ",
		"b@bv ",
		" 1 . ",
		"  c@c",
		"   1 " }, {
		" .<a>. ",
		"b@b c@c",
		" 1   1 " } };
	G["copy2t2"] = { { // a=old_input b=new_input c=output1 d=output2  2ticks
		" .<b>. ",
		"c@c^d@d",
		" 1 a 1 " } };
	G["copy1"] = { { // a=input b=output
		"a>. ",
		" b@b",
		"  1 " }, {
		" .<a",
		"b@b ",
		" 1  " }, {
		" a ",
		" v ",
		" . ",
		"b@b",
		" 1 " } };
	G["copy1t2"] = { { // a=old_input b=new_input c=output   2ticks
		"b>. ",
		"^c@c",
		"a 1 " }, {
		" .<b",
		"c@c^",
		" 1 a" } };
	G["eqs3"] = { { // a=input b=input c=input   if (a==b) then c->S
		" bS",
		"a#^",
		"c>." } };
	G["neqs3"] = { { // a=input b=input c=input   if (a!=b) then c->S
		" bS",
		"a=^",
		"c>." } };
	G["eqs3t2"] = { { // a=input b=input c=input   if (a==b) then c->S
		"   bS",
		"a>.#^",
		"c>.>." } };
	G["neqs3t2"] = { { // a=input b=input c=input   if (a!=b) then c->S
		"   bS",
		"a>.=^",
		"c>.>." } };
	G["eqs2"] = { { // a=input b=input   if (a==b) then a->S
		" a ",
		"b=S" }, {
		" a",
		"b=",
		" S" } };
	G["divs0"] = { { // a=input1 b=input2   if (a%b==0) then 0->S
		" b ",
		"a%.",
		" . ",
		"0=S" }, {
		" b ",
		"a%.",
		" . ",
		"0= ",
		" S " }, {
		" b 0 ",
		"a%.=S",
		" .   " }, {
		" b 0",
		"a%.=",
		" . S" } };
	G["gcds"] = { { // a,b,c=input   if a==0 then b/c->S
		"   0 ",
		" ca#.",
		"b/.>S",
		" .   " }, {
		"  c ",
		" b/.",
		" 0. ",
		"a#v ",
		"  S " } };
	G["neqs"] = { { // a=input  if a%2!=0 then a%2 -> S
		" 2 ",
		"a%.",
		" . ",
		"0#S" }, {
		" 2 0",
		"a%.#",
		" . S" } };
	G["neqs2"] = { { // a,b=input  if a != b then a -> S
		" b",
		"a#",
		" S" }, {
		" b ",
		"a#S" } };
	G["g001"] = { {
		" a 1 ",
		"b+.+S",
		" . . " } };
}

bool allowed( char c )
{
	return ('a'<=c && c<='z' && c!='v') || ('A'<=c && c<='Z' && c!='S') ||
		c=='[' || c==']' || c=='{' || c=='}' || c=='(' || c==')' || c=='$' || c=='&';
}

struct GADGET
{
	string name;
	string param;
	vector< string > shape;

	void patch()
	{
		for (auto & row : shape)
			for (char & c : row)
				if ('a'<=c && c<='j')
				{
					int ind = c-'a';
					ass( ind < (int)param.size() );
					c = param[ind];
				}
	}
};

string task_id;
vector< pair< string, string > > program;
map< char, string > init;
vector< GADGET > gadgets;

char F[200][200];
char F2[200][200];

int best_area = 280;
int best_x=-1, best_y=-1;

string patch_with_jumps()
{
	string res = "";
	map< char, pair< int, int > > pos;
	for (int x=1; x<=best_x; x++)
		for (int y=1; y<=best_y; y++)
		{
			char c = F2[x][y];
			if (allowed(c))
				if (F2[x][y-1]!='@' && F2[x][y+1]!='@')
				{
					//ass( pos.find( c ) == pos.end() );
					pos[c] = { x, y };
				}
		}
	for (int x=1; x<=best_x; x++)
	{
		for (int y=1; y<=best_y; y++)
		{
			char c = F2[x][y];
			if (allowed(c))
			{
				if (F2[x][y+1]=='@')
					res += to_string( y-pos[c].second+1 );
				else if (F2[x][y-1]=='@')
					res += to_string( x-pos[c].first );
				else if (init.find( c ) != init.end())
					res += init[c];
				else res.push_back( '.' );
			}
			else if (c==0) res.push_back( '.' );
			else res.push_back( c );
			if (y!=best_y) res.push_back( ' ' );
		}
		if (x!=best_x) res.push_back( '\n' );
	}
	return res;
}

bool can_place( int x, int y, GADGET & g )
{
	for (int i=0; i<(int)g.shape.size(); i++)
		for (int j=0; j<(int)g.shape[i].size(); j++)
			if (g.shape[i][j] != ' ')
				if (F[x+i][y+j])
					return false;
	return true;
}

void place( int x, int y, GADGET & g )
{
	for (int i=0; i<(int)g.shape.size(); i++)
		for (int j=0; j<(int)g.shape[i].size(); j++)
			if (g.shape[i][j] != ' ')
				F[x+i][y+j] = g.shape[i][j];
}

void unplace( int x, int y, GADGET & g )
{
	for (int i=0; i<(int)g.shape.size(); i++)
		for (int j=0; j<(int)g.shape[i].size(); j++)
			if (g.shape[i][j] != ' ')
				F[x+i][y+j] = 0;
}

pair< int, int > pack_stupid()
{
	CLR(F);
	int resx=0, resy=0;
	for (auto & g : gadgets)
		for (int y = 1; ; y++)
			if (can_place( 1, y, g ))
			{
				place( 1, y, g );
				resx = max( resx, (int)g.shape.size() );
				resy = max( resy, y+(int)g.shape[0].size()-1 );
				break;
			}
	return { resx, resy };
}

void mother_packer()
{
	cerr << "stupid ";
	auto [x,y] = pack_stupid();

	if (x*y < best_area)
	{
		best_area = x*y;
		best_x = x;
		best_y = y;
		//CLR( F2 );
		for (int i=1; i<=x; i++)
			for (int j=1; j<=y; j++)
				F2[i][j] = F[i][j];
	}
	cerr << best_area << "\n";
}

void dfsp( int i, int mx, int my, vector< int > & p )
{
	if (i==(int)p.size())
	{
		if (mx*my < best_area)
		{
			cerr << "area=" << mx*my << "\n";
			best_area = mx*my;
			best_x = mx;
			best_y = my;
			//CLR( F2 );
			for (int i=1; i<=mx; i++)
				for (int j=1; j<=my; j++)
					F2[i][j] = F[i][j];
			cout << "area=" << mx*my << "\n";
			string sol = "solve 3d" + task_id + "\n" + patch_with_jumps();
			cout << sol << "\n";
			cout << encode( sol ) << "\n";
		}
		return;
	}

	set< pair< int, int > > Set;
	for (int x=1; x<=mx+1; x++)
		for (int y=1; ; y++)
			if (can_place( x, y, gadgets[p[i]] ))
			{
				Set.insert( { x, y } );
				break;
			}
	for (int y=1; y<=my+1; y++)
		for (int x=1; ; x++)
			if (can_place( x, y, gadgets[p[i]] ))
			{
				Set.insert( { x, y } );
				break;
			}
	vector< pair< int, int > > pos( Set.begin(), Set.end() );
	shuffle( pos.begin(), pos.end(), gmt );

	for (auto [x,y] : pos)
	{
		int mx2 = max( mx, x+(int)gadgets[p[i]].shape.size()-1 );
		int my2 = max( my, y+(int)gadgets[p[i]].shape[0].size()-1 );
		if (mx2 * my2 < best_area)
			if (mx2 < 100 && my2 < 100)
			{
				place( x, y, gadgets[p[i]] );
				dfsp( i+1, mx2, my2, p );
				unplace( x, y, gadgets[p[i]] );
			}
	}
}

void random_packer()
{
	cerr << "random\n";
	CLR(F);

	vector< int > p;
	for (int i=0; i<(int)gadgets.size(); i++)
		p.push_back( i );

	for (int i=0; i<10; i++)
	{
		shuffle( p.begin(), p.end(), gmt );
		dfsp( 0, 0, 0, p );
	}
	cerr << best_area << "\n";
}

void permute_packer()
{
	cerr << "permute\n";
	CLR(F);

	vector< int > p;
	for (int i=0; i<(int)gadgets.size(); i++)
		p.push_back( i );

	do
	{
		dfsp( 0, 0, 0, p );
	}
	while (next_permutation( p.begin(), p.end() ));
	cerr << best_area << "\n";
}

void dfs( int i )
{
	if (i==(int)program.size())
	{
		mother_packer();
		random_packer();
		permute_packer();
		return;
	}

	auto & versions = G[program[i].first];
	for (auto & g : versions)
	{
		GADGET ga = { program[i].first, program[i].second, g };
		ga.patch();
		gadgets.push_back( ga );
		dfs( i+1 );
		gadgets.pop_back();
	}
}

int main()
{
	freopen("input.txt","r",stdin);
	freopen("output.txt","w",stdout);

	for (int a=0; a<(int)A.size(); a++)
		B[ A[a] ] = a+33;

	init_gadgets();

	string cmd;
	while (cin >> cmd)
	{
		if (cmd=="end") break;
		else if (cmd=="task") cin >> task_id;
		else if (cmd=="=")
		{
			string x, y;
			cin >> x >> y;
			init[x[0]] = y;
		}
		else
		{
			string param;
			cin >> param;
			program.push_back( { cmd, param } );
		}
	}

	dfs( 0 );

	cout << best_area << " " << best_x << " " << best_y << "\n";
	for (int i=1; i<=best_x; i++)
	{
		for (int j=1; j<=best_y; j++)
			if (F2[i][j]) cout << F2[i][j] << " ";
			else cout << "  ";
		cout << "\n";
	}
	cout << "\n";

	string sol = "solve 3d" + task_id + "\n" + patch_with_jumps();
	cout << sol << "\n";
	cout << encode( sol ) << "\n";

	return 0;
}