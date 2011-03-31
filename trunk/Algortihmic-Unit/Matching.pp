unit MatchingUnit;
{$mode Objfpc}
interface
type
  TAdjMartix= array of array of Boolean;

  function MaximumBipartiteMatching (M: TAdjMartix; n: Integer): Integer;

implementation

function MaximumBipartiteMatching (M: TAdjMartix; n: Integer): Integer;
var
  IsVisited: array [0..100] of Boolean;
  Prev: array [0..100] of Integer;

  function FindNextDFS (v: Integer): Boolean;
  var
    i: Integer;

  begin
    if IsVisited [v] then
      Exit (False);

    IsVisited [v]:= True;

    for i:= 0 to n- 1 do
      if M [v, i] then
        if FindNextDFS (Prev [i]) then
        begin
          Prev [i]:= v;
          Exit (True);

        end;

    Result:= False;

  end;

var
  i: Integer;

begin
  FillChar (Prev, SizeOf (Prev), 255);
  Result:= 0;

  for i:= 0 to n- 1 do
  begin
    FillChar (IsVisited, SizeOf (IsVisited), 0);
    if {not??!!} FindNextDFS (i) then
      Inc (Result);

  end;

end;

end.
