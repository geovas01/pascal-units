unit BitVectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, TSeitinVariableUnit, ClauseUnit;

type

  { TBitVector }

  TBitVector= class (specialize TFPGList<TLiteral>)
  private
    function GetBit (Index: Integer): TLiteral;

  public
    // if Index is out of bound, we return the False literal
    property Bit [Index: Integer]: TLiteral read GetBit;

    constructor Create (Size: Integer);
    constructor Create (Size: Integer; Literal: TLiteral);
    constructor Create;

    function ToString: AnsiString;
    function Copy: TBitVector;

  end;

implementation

{ TBitVector }

function TBitVector.GetBit(Index: Integer): TLiteral;
begin
  if Index< Count then
    Result:= Self [Index]
  else
    Result:= TSeitinVariableUnit.GetVariableManager.FalseLiteral;

end;

constructor TBitVector.Create (Size: Integer);
var
  i: Integer;

begin
  inherited Create;

  Count:= Size;
  for i:= 0 to Count- 1 do
    Items[i]:= CreateLiteral (
             TSeitinVariableUnit.GetVariableManager.CreateNewVariable,
             False);

end;

constructor TBitVector.Create(Size: Integer; Literal: TLiteral);
var
  i: Integer;

begin
  inherited Create;

  Count:= Size;
  for i:= 0 to Count- 1 do
    Items[i]:= Literal;

end;

constructor TBitVector.Create;
begin
  inherited;

end;

function TBitVector.ToString: AnsiString;
var
  i: Integer;
  S, Space: AnsiString;
  MaxLen: Integer;

begin
  Result:= '(';

  MaxLen:= -1;
  for i:= 0 to Self.Count- 2 do
    if MaxLen< Length (LiteralToString(Self [i])) then
      MaxLen:= Length (LiteralToString(Self [i]));
  if Count<> 0 then
    if MaxLen< Length (LiteralToString (Self [Count- 1])) then
      MaxLen:= Length (LiteralToString(Self [Count- 1]));

  Space:= '';
  for i:= 1 to MaxLen do
    Space+= ' ';

  for i:= 0 to Self.Count- 2 do
  begin
    S:= LiteralToString (Self [i])+ ',';
    Result+= System.Copy (Space, 1, MaxLen- Length (S))+ S;

  end;

  if Count<> 0 then
  begin
    S:= LiteralToString (Self [Count- 1])+ ',';
    Result+= System.Copy (Space, 1, MaxLen- Length (S))+ S;

  end;

  Result+= ')';

end;

function TBitVector.Copy: TBitVector;
var
  i: Integer;

begin
  Result:= TBitVector.Create (Count, GetVariableManager.FalseLiteral);

  for i:= 0 to Count- 1 do
    Result [i]:= Self [i];

end;

end.

