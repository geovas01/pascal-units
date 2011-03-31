program UseHeap;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, HeapUnit
  { you can add units after this };

{$IFDEF WINDOWS}{$R UseHeap.rc}{$ENDIF}

type
  TIntHeap= specialize THeap<Integer>;

function IntGreaterThan (const a, b: Integer): Boolean;
begin
  Result:= b< a;

end;

var
  Heap: TIntHeap;
  n: Integer;

begin

  Heap:= TIntHeap.Create (10, @IntGreaterThan);

  ReadLn (n);
  while n<> 0 do
  begin
    if n< 0 then
    begin
      WriteLn (Heap.Min);
      Heap.DeleteMin;

    end;
    if 0< n then
      Heap.Insert (n);

    Heap.Print;
    ReadLn (n);
  end;

end.

