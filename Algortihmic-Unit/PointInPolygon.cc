#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <queue>
#include <set>
#include <map>
#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cmath>
#include <cassert>
using namespace std;

vector<string> split( const string& s, const string& delim =" " ) {
    vector<string> res;
    string t;
    for ( long long i = 0 ; i != s.size() ; i++ ) {
        if ( delim.find( s[i] ) != string::npos ) {
            if ( !t.empty() ) {
                res.push_back( t );
                t = "";
            }
        } else {
            t += s[i];
        }
    }
    if ( !t.empty() ) {
        res.push_back(t);
    }
    return res;
}

vector<long long> splitInt( const string& s, const string& delim =" " ) {
    vector<string> tok = split( s, delim );
    vector<long long> res;
    for ( long long i = 0 ; i != tok.size(); i++ )
        res.push_back( atoi( tok[i].c_str() ) );
    return res;
}

#define ARRSIZE(x) (sizeof(x)/sizeof(x[0]))

template<typename T> void print( T a ) {
    cerr << a;
}
static void print( long long a ) {
    cerr << a << "L";
}
static void print( string a ) {
    cerr << '"' << a << '"';
}
template<typename T> void print( vector<T> a ) {
    cerr << "{";
    for ( long long i = 0 ; i != a.size() ; i++ ) {
        if ( i != 0 ) cerr << ", ";
        print( a[i] );
    }
    cerr << "}" << endl;
}
template<typename T> void eq( long long n, T have, T need ) {
    if ( have == need ) {
        cerr << "Case " << n << " passed." << endl;
    } else {
        cerr << "Case " << n << " failed: expected ";
        print( need );
        cerr << " received ";
        print( have );
        cerr << "." << endl;
    }
}
template<typename T> void eq( long long n, vector<T> have, vector<T> need ) {
    if( have.size() != need.size() ) {
        cerr << "Case " << n << " failed: returned " << have.size() << " elements; expected " << need.size() << " elements.";
        print( have );
        print( need );
        return;
    }
    for( long long i= 0; i < have.size(); i++ ) {
        if( have[i] != need[i] ) {
            cerr << "Case " << n << " failed. Expected and returned array differ in position " << i << ".";
            print( have );
            print( need );
            return;
        }
    }
    cerr << "Case " << n << " passed." << endl;
}
static void eq( long long n, string have, string need ) {
    if ( have == need ) {
        cerr << "Case " << n << " passed." << endl;
    } else {
        cerr << "Case " << n << " failed: expected ";
        print( need );
        cerr << " received ";
        print( have );
        cerr << "." << endl;
    }
}

inline long long sqr (long long n)
{
  return n* n;

}

inline long long sign (long long n)
{
  if (n< 0)
    return -1;
  else if (n== 0)
    return 0;
  else
    return 1;

}

class PointInPolygonlygon {
private:
  vector<pair<long long , long long> > Points;
  long long n;

  bool IsOnLineSegment (pair<long long, long long> P, pair<long long, long long> S, 
		   pair<long long, long long> E)
  {


    if ((P.first- S.first)* (S.second- E.second)== 
	(S.first- E.first)* (P.second- S.second))
    {
      if (sign (S.first- P.first)* sign (E.first- P.first)< 0)
	return true;
      if (sign (S.second- P.second)* sign (E.second- P.second)< 0)
	return true;

    }

    return false;

  }


  double Distance2 (pair<long long, int> P, pair<int, int> Q)
  {
    double Result= sqr (P.first- Q.first)+ sqr (Q.second- P.second);
    return Result;
  }

  bool HasIntersection (pair<long long, int> S1, pair<int, int> E1, 
		   pair<long long, int> S2, pair<int, int> E2)
  {
    long long A1= S1.second- E1.second, B1= E1.first- S1.first, C1= -(A1* S1.first+ B1* S1.second);
    assert (A1* E1.first+ B1* E1.second+ C1== 0);
    assert (A1* S1.first+ B1* S1.second+ C1== 0);

    long long A2= S2.second- E2.second, B2= E2.first- S2.first, C2= -(A2* S2.first+ B2* S2.second);
    assert (A2* E2.first+ B2* E2.second+ C2== 0);
    assert (A2* S2.first+ B2* S2.second+ C2== 0);

    if (sign (A2* S1.first+ B2* S1.second+ C2)* sign (A2* E1.first+ B2* E1.second+ C2)<= 0)
      if (sign (A1* S2.first+ B1* S2.second+ C1)* sign (A1* E2.first+ B1* E2.second+ C1)<= 0)
      {
	return true;
      }

    return false;

  }

  long long MaxX, MaxY;
  bool IsOnInterior (pair<long long, int> P)
  {
    pair<long long, int> Start;
    Start.first= MaxX+ 1;
    Start.second= MaxY+ 1;

    bool HasColision= true;
    while (HasColision)
    {
      HasColision= false;
      Start.first++;

      for (long long i= 0; i< Points.size (); ++i)
	if (IsOnLineSegment (Points [i], Start, P))
	{
	  HasColision= true;
	  break;

	}

      if (HasColision)
	continue;

    }

    long long Count= 0;
    for (long long i= 0; i< Points.size (); ++i)
    {
      if (HasIntersection (Start, P, Points [i], Points [(i+ 1)% n]))
	Count++;

    }
    return (Count% 2== 1);

  }

  bool IsOnBoundry (pair<long long, int> P)
  {
    for (long long i= 0; i< Points.size (); ++i)
      if (IsOnLineSegment (P, Points [i], Points [(i+ 1)% n]))
      {
	return true;
      }

    return false;

  }

public:
    string testPoint (vector <string> Vertices, int X, int Y) {

	n= Vertices.size ();
	MaxX= MaxY= -1;
	pair<long long, int> P= make_pair (X, Y);

	for (long long i= 0; i< Vertices.size (); ++i)
	{
	  stringstream ss;
	  ss<< Vertices [i];
	  long long x, y;
	  ss>> x>> y;
	  if (MaxX< x)
	    MaxX= x;
	  if (MaxY< y)
	    MaxY= y;

	  Points.push_back (make_pair (x, y));

	}
 
	for (int i= 0; i< Points.size (); ++i)
	  if (Points [i].first== P.first && Points [i].second== P.second)
	    return "BOUNDARY";

	if (IsOnBoundry (P))
	  return "BOUNDARY";

	if (IsOnInterior (P))
	  return "INTERIOR";
	else
	  return "EXTERIOR";

    }

};


int main( int argc, char* argv[] ) {
    {
        string verticesARRAY[] = {"500 0", "500 100", "400 100", "400 200", "300 200", "300 300", "200 300", "200 400", "100 400", "100 500", "0 500", "0 400", "-100 400", "-100 300", "-200 300", "-200 200", "-300 200", "-300 100", "-400 100", "-400 0", "-500 0", "-500 -100", "-400 -100", "-400 -200", "-300 -200", "-300 -300", "-200 -300", "-200 -400", "-100 -400", "-100 -500", "0 -500", "0 -400", "100 -400", "100 -300", "200 -300", "200 -200", "300 -200", "300 -100", "400 -100", "400 0"};
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
        eq(0, theObject.testPoint(vertices, 400, 200),"BOUNDARY");

    }
    {
        string verticesARRAY[] = {"0 1000", "1000 1000", "1000 800", "200 800", "200 600", "600 600", "600 400", "200 400", "200 200", "1000 200", "1000 0", "0 0"};
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
        eq(0, theObject.testPoint(vertices, 0, 500),"BOUNDARY");

    }

    {
        string verticesARRAY[] = {"0 0",
            "0 10",
            "10 10",
            "10 0"};
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
        eq(0, theObject.testPoint(vertices, 5, 5),"INTERIOR");
    }
    {
        string verticesARRAY[] = {"0 0",
            "0 10",
            "10 10",
            "10 0"};
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
        eq(1, theObject.testPoint(vertices, 10, 15),"EXTERIOR");
    }
    {
        string verticesARRAY[] = {"0 0",
            "0 10",
            "10 10",
            "10 0"};
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
        eq(2, theObject.testPoint(vertices, 5, 10),"BOUNDARY");
    }
    {
        string verticesARRAY[] = {"-100 -90", "-100 100","100 100", "100 -100",
            "-120 -100","-120 100","-130 100","-130 -110",
            "110 -110", "110 110", "-110 110","-110 -90"};
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
      //  eq(3, theObject.testPoint(vertices, 0, 0),"EXTERIOR");
    }
    {
        string verticesARRAY[] = {"0 0","0 1000","1000 1000","1000 800",
            "200 800","200 600","600 600","600 400",
            "200 400","200 200","1000 200","1000 0"};
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
        eq(4, theObject.testPoint(vertices, 100, 500),"INTERIOR");
    }
    {
        string verticesARRAY[] = {"0 1000","1000 1000","1000 800",
            "200 800","200 600","600 600","600 400",
            "200 400","200 200","1000 200","1000 0","0 0"};
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
        eq(5, theObject.testPoint(vertices, 322, 333),"EXTERIOR");
    }
    {
        string verticesARRAY[] = {"500 0","500 100","400 100","400 200","300 200",
            "300 300","200 300","200 400","100 400","100 500",
            "0 500","0 400","-100 400","-100 300","-200 300",
             "-200 200","-300 200","-300 100","-400 100","-400 0",
             "-500 0","-500 -100","-400 -100","-400 -200","-300 -200",
             "-300 -300","-200 -300","-200 -400","-100 -400","-100 -500",
             "0 -500","0 -400","100 -400","100 -300","200 -300",
             "200 -200","300 -200","300 -100","400 -100","400 0"};
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
        eq(6, theObject.testPoint(vertices, 200, 200),"INTERIOR");
    }
    {
        string verticesARRAY[] = {"1 0","2 0","2 1","3 1","3 0","4 0","4 -1","5 -1","5 0",
            "6 0","6 2","0 2","0 3","-1 3","-1 4","0 4","0 6","1 6",
            "1 7","0 7","0 8","-2 8","-2 2","-8 2","-8 0","-7 0",
            "-7 -1","-6 -1","-6 0","-4 0","-4 1","-3 1","-3 0",
            "-2 0","-2 -6","0 -6","0 -5","1 -5","1 -4","0 -4",
            "0 -3","-1 -3","-1 -2","0 -2","0 -1","1 -1"} ;
        vector <string> vertices( verticesARRAY, verticesARRAY+ARRSIZE(verticesARRAY) );
        PointInPolygonlygon theObject;
        eq(7, theObject.testPoint(vertices, 0, 0),"INTERIOR");
    }
}
