unit GeomertyUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;
type
  TPoint= record
    x, y: Extended;

  end;

function SubtractPoint (A, B: TPoint): TPoint;
function GetDistance (A, B: TPoint): Extended;
function GetAngle (A, B, C: TPoint): Extended;
function CirclePassing (P1, P2, P3: TPoint): TPoint;

implementation


{Tested: Problem Codeforces.1C}
function SubtractPoint (A, B: TPoint): TPoint;
begin
  Result:= A;
  Result.x-= B.x;
  Result.y-= B.y;

end;

{Tested: Problem Codeforces.1C}
function GetDistance (A, B: TPoint): Extended;
begin
  Result:= Sqrt (Sqr (A.x- B.x)+ Sqr (A.y- B.y));

end;

{Tested: Problem Codeforces.1C}
function GetAngle (A, B, C: TPoint): Extended;

  function _GetAngle (C: TPoint): Extended;{C, (0,0), (1, 0)}
  begin
    Result:= ArcCos (C.x/ Sqrt (Sqr (C.x)+ Sqr (C.y)));

    if C.y< 0 then
      Result:= 2* Pi- Result;

  end;
var
  aa, bb, cc: Extended;
  d: Extended;

begin
  Result:= _GetAngle (SubtractPoint (A, B))- _GetAngle (SubtractPoint (C, B));
  if Result< 0 then
    Result+= 2* Pi;

  cc:= GetDistance (A, B);
  bb:= GetDistance (A, C);
  aa:= GetDistance (B, C);

  d:= arccos ((aa*aa+cc*cc-bb*bb)/(2*aa*cc));
  if (1e-6< Abs (Result- d)) and
     (1e-6< Abs (Result- 2* Pi+ d)) then
       WriteLn ('Error');

//  Result:= d;
end;

{Tested: Problem Codeforces.1C}
function CirclePassing (P1, P2, P3: TPoint): TPoint;
var
  x, y: array [0..3] of Extended;
  x1, y1, dy1, dx1: Extended;
  x2, y2, dy2, dx2: Extended;

begin
  x [1]:= P1.x; x [2]:= P2.x; x [3]:= P3.x;
  y [1]:= P1.y; y [2]:= P2.y; y [3]:= P3.y;

  x1:= (x [2] + x [1]) / 2;
  y1:= (y [2] + y [1]) / 2;
  dy1:= x [2]- x [1];
  dx1:= -(y [2]- y [1]);

  x2:= (x [3] + x [2]) / 2;
  y2:= (y [3]+ y [2]) / 2;
  dy2:= x [3] - x [2];
  dx2:= -(y [3]- y [2]);

  Result.x:= (y1 * dx1 * dx2 + x2 * dx1 * dy2 - x1 * dy1 * dx2 - y2 * dx1 * dx2)/ (dx1 * dy2 - dy1 * dx2);
  Result.y:= (Result.x - x1) * dy1 / dx1 + y1;

{
  WriteLn ('A:', Distance (Result, P1), ' * ', Distance (Result, P2));
  WriteLn ('B:', Distance (Result, P1), ' * ', Distance (Result, P3));
}

end;

end.

