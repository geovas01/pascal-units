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

  {*Change L1.a* L2.b=L1.b*L2.a if necessary*}
  function FindIntersectionPoint (L1, L2: TLine; IntersectionPoint: TPoint): Boolean;

  function PointOnLine (P: TPoint; L: TLine): Boolean;
  function PointOnLineSegment (P: TPoint; L: TLineSegment): Boolean;

  function PointInsidePolyoing (P: TPoint; Polygon: array of TPoint; NPoint: Integer= -1): Boolean;

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

function FindIntersectionPoint (L1, L2: TLine; IntersectionPoint: TPoint): Boolean;
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

function PointInsidePolyoing (P: TPoint; Polygon: array of TPoint; NPoint: Integer): Boolean;
var
  i, j: Integer;
  RandomPoint: TPoint;

begin
  if NPoint< 0 then
    NPoint:= High (Polygon)- Low (Polygon)+ 1;



end;

end.

