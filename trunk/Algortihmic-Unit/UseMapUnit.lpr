program UseMapUnit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this }, MapUnit;

{$IFDEF WINDOWS}{$R UseMapUnit.rc}{$ENDIF}


function IsGreaterThan (const a, b: Integer): Boolean;
begin
  Result:= a< b;

end;

type
  TIntMap= specialize TMap<Integer>;

var
  IntMap: TIntMap;

begin
  IntMap:= TIntMap.Create (@IsGreaterThan);

  IntMap.IsExists (4);

end.

