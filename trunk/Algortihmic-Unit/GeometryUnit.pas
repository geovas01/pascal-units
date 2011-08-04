unit GeometryUnit;

{$mode objfpc}{$H+}

interface

type

  TPoint= record
    x, y: Extended;

  end;

  TLine= record
    a, b, c: Extended;

  end;

  TLineSegment= record
     L: TLine;
     S, E: TPoint;

  end;

  function CreateLine (P, Q: TPoint): TLine;
  function CreateLine (_a, _b, _c: Integer): TLine;
  function CreateLineSeqment (P, Q: TPoint): TLineSegment;

  {*Check L1.a* L2.b=L1.b*L2.a if necessary*}
  function FindIntersectionPoint (L1, L2: TLine; var IntersectionPoint: TPoint): Boolean;

  function PointOnLine (P: TPoint; L: TLine): Boolean;
  function PointOnLineSegment (P: TPoint; L: TLineSegment): Boolean;

  {
      Returns +1: if P is inside the polygon described by Points
      Returns 0: if P is on the boundry of the polygon described by Points
      Returns -1: if P is on outside of the polygon described by Points
  }
  function PointInsidePolyoing (P: TPoint; Points: array of TPoint; NoPoint: Integer= -1): Integer;

const
  Zero= 1e-10;

implementation

function Divide (a, b: Integer): Integer; inline;
begin
  Result:= a div b;

end;

function Divide (a, b: Extended): Extended; inline;
begin
  Result:= a/ b;

end;

function CreateLine(P, Q: TPoint): TLine;
begin
  Result.a:= P.y- Q.y;
  Result.b:= Q.x- P.x;
  Result.c:= P.x* Q.y- Q.x* P.y;

end;

function CreateLine (_a, _b, _c: Integer): TLine;
begin
  Result.a:= _a;
  Result.b:= _b;
  Result.c:= _c;

end;

function CreateLineSeqment(P, Q: TPoint): TLineSegment;
begin
  Result.L:= CreateLine (P, Q);
  Result.S:= P;
  Result.E:= Q;

end;

function FindIntersectionPoint (L1, L2: TLine; var IntersectionPoint: TPoint): Boolean;
begin
  if Abs (L1.a* L2.b- L1.b* L2.a)< Zero then
    Exit (False)
  else
  begin
    IntersectionPoint.x:= (L1.a* L2.c- L2.a* L1.c)/ (L2.a* L1.b- L1.a* L2.b);
    if Zero< Abs (L1.b) then
      IntersectionPoint.y:= -(L1.a* IntersectionPoint.x+ L1.c)/ L1.b
    else
      IntersectionPoint.y:= -(L2.a* IntersectionPoint.x+ L2.c)/ L2.b;

    Result:= True;

  end;

end;

function PointOnLine (P: TPoint; L: TLine): Boolean;
begin
  Result:= Abs (L.a* P.x+ L.b* P.y+ L.c)< Zero;

end;

function PointOnLineSegment (P: TPoint; L: TLineSegment): Boolean;
begin
  Result:= False;

  if PointOnLine (P, L.L) then
    if Abs (Abs (P.x- L.S.x)+ Abs (P.x- L.E.x)- Abs (L.S.x- L.E.x))<= Zero then
      Exit (True);

end;

function PointInsidePolyoing (P: TPoint; Points: array of TPoint; NoPoint: Integer): Integer;

  function IsOnBoundry (P: TPoint): Boolean;
  var
    i: Integer;

  begin
    for i:= 0 to NoPoint- 1 do
      if PointOnLineSegment (P, CreateLineSeqment (Points [i], Points [(i+ 1) mod NoPoint])) then
        Exit (True);

    Result:= False;

  end;

  function IsOnInterior (P: TPoint): Boolean;
  var
    Start: TPoint;
    HasColision: Boolean;
    i, Count: Integer;
    IntersectionPoint: TPoint;

  begin
    Start.x:= Points [0].x+ 1;
    Start.y:= Points [0].y+ 1;

    for i:= 1 to NoPoint do
    begin
      if Start.x< Points [i].x then
        Start.x:= Points [i].x+ 1;
      if Start.y< Points [i].y then
        Start.y:= Points [i].y+ 1;

    end;

    HasColision:= True;
    while (HasColision) do
    begin 
      HasColision:= false;
      Start.x:= Start.x+ 1;

      for i:= 0 to NoPoint do
	if PointOnLineSegment (P, CreateLineSeqment (Points [i], Start)) then
        begin	
	  HasColision:= true;
	  break;

	end;

      if HasColision then
	continue;

    end; 

    Count:= 0;
    for i:= 0 to NoPoint do
    begin
      if FindIntersectionPoint (CreateLine (Start, P), CreateLine (Points [i], Points [(i+ 1) mod NoPoint]), IntersectionPoint) then
        if PointOnLineSegment (IntersectionPoint, CreateLineSeqment (Start, P)) and PointOnLineSegment (IntersectionPoint, CreateLineSeqment (Points [i], Points [(i+ 1) mod NoPoint])) then
          Inc (Count);

    end;

    Result:= (Count mod 2= 1);

  end;

var
  i, j: Integer;
  RandomPoint: TPoint;

begin
  if NoPoint< 0 then
    NoPoint:= High (Points)- Low (Points)+ 1;
 
  for i:= 0 to NoPoint- 1 do
    if (Abs (Points [i].x- P.x)< 1e-5) and (Abs (Points [i].y- P.y)< 1e-5) then
      Exit (0);
  
  if IsOnBoundry (P) then
    Exit (0);
  
  if IsOnInterior (P) then
    Exit (1)
  else
    Exit (-1);
  
  


end;

end.

