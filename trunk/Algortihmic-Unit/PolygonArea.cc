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
using namespace std;

vector<string> split( const string& s, const string& delim =" " ) {
      vector<string> res;
          string t;
	      for ( int i = 0 ; i != s.size() ; i++ ) {
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

vector<int> splitInt( const string& s, const string& delim =" " ) {
      vector<string> tok = split( s, delim );
          vector<int> res;
	      for ( int i = 0 ; i != tok.size(); i++ )
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
          for ( int i = 0 ; i != a.size() ; i++ ) {
	            if ( i != 0 ) cerr << ", ";
		            print( a[i] );
			        }
	      cerr << "}" << endl;
}
template<typename T> void eq( int n, T have, T need ) {
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
template<typename T> void eq( int n, vector<T> have, vector<T> need ) {
      if( have.size() != need.size() ) {
	        cerr << "Case " << n << " failed: returned " << have.size() << " elements; expected " << need.size() << " elements.";
		        print( have );
			        print( need );
				        return;
					    }
          for( int i= 0; i < have.size(); i++ ) {
	            if( have[i] != need[i] ) {
		                  cerr << "Case " << n << " failed. Expected and returned array differ in position " << i << ".";
				              print( have );
					                  print( need );
							              return;
								              }
		        }
	      cerr << "Case " << n << " passed." << endl;
}
static void eq( int n, string have, string need ) {
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

struct TVector
{
  int x, y;

  TVector (int x, int y)
  {
    this->x= x;
    this->y= y;

  }

  TVector (TVector P1, TVector P2)
  {
    x= P2.x- P1.x;
    y= P2.y- P1.y;

  }

  TVector ()
  {
    x= y= 0;
  }

  TVector operator - (TVector &Another)
  {
    TVector Result (x- Another.x, y- Another.y);

    return Result;

  }

  int operator * (TVector Another)
  {
    return x* Another.y- y* Another.x;

  }

};


class ConvexPolygon {
  int n;
public:
  double findArea(vector <int> x, vector <int> y) {
    double Result= 0.0;
    vector<TVector> Points;
    n= x.size ();
    Points.resize (n);

    for (int i= 0; i< n; ++i)
    {
      Points [i].x= x [i];
      Points [i].y= y [i];

    }

    for (int i= 1; i< n- 1; ++i)
    {
      Result+= (Points [0]- Points [i]) * (Points [0]- Points [i+ 1]);
    }

    if (Result< 0)
      Result= -Result;
    Result/= 2.0;

    return Result;
  }

};


int main( int argc, char* argv[] ) {
	   {
	             int xARRAY[] = {0,0,1};
		             vector <int> x( xARRAY, xARRAY+ARRSIZE(xARRAY) );
			             int yARRAY[] = {0,1,0};
				             vector <int> y( yARRAY, yARRAY+ARRSIZE(yARRAY) );
					             ConvexPolygon theObject;
						             eq(0, theObject.findArea(x, y),0.5);
							         }
    {
              int xARRAY[] = {-10000,-10000,10000,10000};
	              vector <int> x( xARRAY, xARRAY+ARRSIZE(xARRAY) );
		              int yARRAY[] = {10000,-10000,-10000,10000};
			              vector <int> y( yARRAY, yARRAY+ARRSIZE(yARRAY) );
				              ConvexPolygon theObject;
					              eq(1, theObject.findArea(x, y),4.0E8);
						          }
    {
              int xARRAY[] = {100,80,30,-30,-80,-100,-80,-30,30,80};
	              vector <int> x( xARRAY, xARRAY+ARRSIZE(xARRAY) );
		              int yARRAY[] = {0,58,95,95,58,0,-58,-95,-95,-58};
			              vector <int> y( yARRAY, yARRAY+ARRSIZE(yARRAY) );
				              ConvexPolygon theObject;
					              eq(2, theObject.findArea(x, y),29020.0);
						          }
    {
              int xARRAY[] = {-1646,-9172,-9830,-9802,-9749,-9474,-8668,-6832,120,8380,9338,9307,8042};
	              vector <int> x( xARRAY, xARRAY+ARRSIZE(xARRAY) );
		              int yARRAY[] = {-9998,-8619,-7863,3976,4541,5975,8127,9500,9612,8734,5216,-9042,-9689};
			              vector <int> y( yARRAY, yARRAY+ARRSIZE(yARRAY) );
				              ConvexPolygon theObject;
					              eq(3, theObject.findArea(x, y),3.55115104E8);
						          }
    {
              int xARRAY[] = {-6010,-7937,-8782,-9506,-9654,-9852,-9854,-9998,-9999,-9996,-9901,-9811,
		           -9444,-8798,-8580,-2085,6842,8339,9827,9946,9993,9959,9940,9855,9657,
			              8504,8262,7552,6326,5537,4723};
	              vector <int> x( xARRAY, xARRAY+ARRSIZE(xARRAY) );
		              int yARRAY[] = {-9976,-9947,-9873,-9739,-9654,-8501,-8475,-5009,475,4926,7078,8673,9417,
				           9785,9820,9974,9986,9979,9862,9211,-5070,-6599,-7121,-8624,-8912,-9710,
					              -9766,-9863,-9914,-9941,-9962};
			              vector <int> y( yARRAY, yARRAY+ARRSIZE(yARRAY) );
				              ConvexPolygon theObject;
					              eq(4, theObject.findArea(x, y),3.939960635E8);
						          }
}

