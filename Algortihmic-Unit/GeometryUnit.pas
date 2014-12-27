unit GeometryUnit;

{$mode objfpc}{$H+}

interface
uses
  Math;
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
  { It assumes L1 and L2 are different}
  function FindIntersectionPoint (L1, L2: TLine; var IntersectionPoint: TPoint): Boolean;
  function FindIntersectionPoint (L1, L2: TLineSegment; var IntersectionPoint: TPoint): Boolean;
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

  function SameLine(L1, L2: TLine): Boolean;
  begin
    if Abs (L1.a* L2.b- L1.b* L2.a)< Zero then
    begin
      if Zero < Abs(L1.a) then
        Exit(Abs(L1.c / L1.a - L2.c / L2.a) <= Zero)
      else
      Exit(Abs(L1.c / L1.b - L2.c / L2.b) <= Zero)
    end
    else
      Result := False;
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
    begin
      if not SameLine(L1, L2) then
        Exit(False)
      else
      begin
        if Zero < Abs(L1.a) then
        begin
          IntersectionPoint.x := 0;
          IntersectionPoint.y := -L1.c / L1.a;
        end
        else // Zero < Abs(L1.b)
        begin
          IntersectionPoint.x := -L1.c / L1.b;
          IntersectionPoint.y := 0;
        end
      end;
    end
    else
    begin
      if Zero< Abs (L2.b) then
        IntersectionPoint.x:= (L1.a* L2.c- L2.a* L1.c)/ (L2.a* L1.b- L1.a* L2.b)
      else // Zero< Abs (L2.a)
      IntersectionPoint.x:= -L2.c / L2.a;

      if Zero< Abs (L1.b) then
        IntersectionPoint.y:= -(L1.a * IntersectionPoint.x + L1.c)/ L1.b
      else
        IntersectionPoint.y:= -(L2.a * IntersectionPoint.x + L2.c)/ L2.b;

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
      Result := (P.x <= max(L.S.x, L.E.x)) and (min(L.S.x, L.E.x) <= P.x)
         and (P.y <= max(L.S.y, L.E.y)) and (min(L.S.y, L.E.y) <= P.y);
  end;

  function HaveIntersection(L1, L2: TLineSegment): Boolean; // Confrimed
  { To find orientation of ordered triplet (p, q, r).
     The function returns following values
     0 --> p, q and r are colinear
     1 --> Clockwise
     2 --> Counterclockwise
   }
    function Orientation(p, q, r: TPoint): Integer;
    var
      Val: Extended;
    begin
      // See 10th slides from following link for derivation of the formula
      // http://www.dcs.gla.ac.uk/~pat/52233/slides/Geometry1x1.pdf
      Val := (q.y - p.y) * (r.x - q.x) -
                (q.x - p.x) * (r.y - q.y);

      if Val = 0  then
        Exit(0);
      if Val > 0 then
        Result := 1
      else
        Result := 2;
    end;

   var
     o1, o2, o3, o4: Integer;
  begin
    o1 := orientation(L1.S, L1.E, L2.S);
    o2 := orientation(L1.S, L1.E, L2.E);
    o3 := orientation(L2.S, L2.E, L1.S);
    o4 := orientation(L2.S, L2.E, L1.E);

     // General case
     if (o1 <> o2) and (o3 <> o4) then
         Exit(True);

     // Special Cases
     // p1, q1 and p2 are colinear and p2 lies on segment p1q1
     if (o1 = 0) and PointOnLineSegment(L2.S, L1) then
       Exit(True);

     // p1, q1 and p2 are colinear and q2 lies on segment p1q1
     if (o2 = 0) and PointOnLineSegment(L2.E, L1) then
       Exit(True);

     // p2, q2 and p1 are colinear and p1 lies on segment p2q2
     if (o3 = 0) and PointOnLineSegment(L1.S, L2) then
       Exit(True);

      // p2, q2 and q1 are colinear and q1 lies on segment p2q2
     if (o4 = 0) and PointOnLineSegment(L1.E, L2) then
       Exit(True);

     Result := False; // Doesn't fall in any of the above cases
  end;

  function FindIntersectionPoint(L1, L2: TLineSegment;
    var IntersectionPoint: TPoint): Boolean;   // looks bogus
  var
    P: TPoint;
  begin
    if SameLine(L1.L, L2.L) then
    begin
      Result := False;
      if PointOnLineSegment(L1.S, L2) then
      begin
        IntersectionPoint := L1.S;
        Result := True;
      end;
      if (not Result) and PointOnLineSegment(L1.E, L2) then
      begin
        IntersectionPoint := L1.E;
        Result := True;
      end;
      if (not Result) and PointOnLineSegment(L2.S, L1) then
      begin
        IntersectionPoint := L2.S;
        Result := True;
      end;
      if (not Result) and PointOnLineSegment(L2.E, L1) then
      begin
        IntersectionPoint := L2.E;
        Result := True;
      end;
      Exit;
    end;

    if not FindIntersectionPoint(L1.L, L2.L, P) then
    begin
      Exit(False);
    end;
    if PointOnLineSegment(P, L1) and PointOnLineSegment(P, L2) then
    begin
      IntersectionPoint := P;
      Exit(True);
    end;
    Result := False;
  end;


function PointInsidePolyoing (P: TPoint; Points: array of TPoint; NoPoint: Integer): Integer;

  function IsOnBoundry (P: TPoint): Boolean;
  var
    i: Integer;

  begin
    for i := 0 to NoPoint- 1 do
      if PointOnLineSegment (P, CreateLineSeqment (Points [i], Points [(i+ 1) mod NoPoint])) then
        Exit (True);

    Result := False;

  end;

  function IsOnInterior (P: TPoint): Boolean;
  var
    Start: TPoint;
    HasColision: Boolean;
    i, Count: Integer;
    IntersectionPoint: TPoint;

  begin
    Start.x := Points [0].x+ 1;
    Start.y := Points [0].y+ 1;

    for i := 1 to NoPoint do
    begin
      if Start.x< Points [i].x then
        Start.x := Points [i].x+ 1;
      if Start.y< Points [i].y then
        Start.y := Points [i].y+ 1;

    end;

    HasColision := True;
    while (HasColision) do
    begin 
      HasColision := false;
      Start.x := Start.x+ 1;

      for i := 0 to NoPoint do
	if PointOnLineSegment (P, CreateLineSeqment (Points [i], Start)) then
        begin	
	  HasColision := true;
	  break;

	end;

      if HasColision then
	continue;

    end; 

    Count := 0;
    for i := 0 to NoPoint do
    begin
      if FindIntersectionPoint (CreateLine (Start, P), CreateLine (Points [i], Points [(i+ 1) mod NoPoint]), IntersectionPoint) then
        if PointOnLineSegment (IntersectionPoint, CreateLineSeqment (Start, P)) and PointOnLineSegment (IntersectionPoint, CreateLineSeqment (Points [i], Points [(i+ 1) mod NoPoint])) then
          Inc (Count);

    end;

    Result := (Count mod 2= 1);

  end;

var
  i, j: Integer;
  RandomPoint: TPoint;

begin
  if NoPoint< 0 then
    NoPoint := High (Points)- Low (Points)+ 1;
 
  for i := 0 to NoPoint- 1 do
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

