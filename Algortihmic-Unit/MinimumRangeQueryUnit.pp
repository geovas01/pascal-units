unit MinimumRangeQueryUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
{
Tested on http://www.codechef.com/MARCH14/problems/ANUGCD
}
  { TMinRangeQuery }

  generic TMinRangeQuery<T> = class(TObject)
  private
  type
  TArr = array of T;
    TIntArr = array of Integer;
    TMinFunction = function (a, b: T): T;
  var
    MinFunction: TMinFunction;
    FMin: array of TArr;

    FCount: Integer;
    FLogCount: Integer;
  private
    property LogCount: Integer read FLogCount;

  public
    property Count: Integer read FCount;

    constructor Create (var InputList: TArr; MinFunctionCompare: TMinFunction);
    function GetMin(Start: Integer; Fin: Integer): T;
  end;

implementation
uses
  Math;

constructor TMinRangeQuery.Create (var InputList: TArr;
  MinFunctionCompare: TMinFunction);
var
  i: Integer;
  l: Integer;
  P2: Integer;

begin
  inherited Create;

  MinFunction:= MinFunctionCompare;
  FCount:= Length(InputList);
  i:= FCount;
  FLogCount:= 1+ Round(log2(FCount));
  l:= FLogCount;

  SetLength(FMin, FLogCount);
  for i:= 0 to LogCount- 1 do
    SetLength(FMin[i], Count);

  for i:= 0 to FCount- 1 do
    FMin[0][i]:= InputList[i];

  P2:= 1;
  for l:= 1 to LogCount- 1 do
  begin
    for i:= 0 to Count- 2* P2 do
      FMin[l, i]:= MinFunction(FMin[l- 1, i],
                               FMin[l- 1, i+ P2]);
    P2*= 2;

  end;

end;


function TMinRangeQuery.GetMin(Start: Integer; Fin: Integer): T;
var
  Len: Integer;
  P2Index, P2: Integer;
  c: Integer;

begin
  Len:= Fin- Start+ 1;
  Assert(Len< Count);
  P2Index:= Trunc(log2(Len));
  P2:= 1 shl P2Index;

  Result:= MinFunction(FMin[P2Index][Start],
                       FMin[P2Index][Fin+ 1- P2]);
end;


end.

