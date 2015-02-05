unit BitVectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, TSeitinVariableUnit, ClauseUnit, gvector;

type

  { TBitVector }

  TBitVector= class(specialize TVector<TLiteral>)
  private
    function GetBit(Index: Integer): TLiteral; inline;
    function GetCount: Integer; inline;
    procedure SetCount(AValue: Integer); inline;

  public
    // if Index is out of bound, we return the False literal
    property Bit[Index: Integer]: TLiteral read GetBit;
    property Count: Integer read GetCount write SetCount;

    constructor Create(s: Integer);
    constructor Create(s: Integer; Literal: TLiteral);
    constructor Create;

    procedure Add(l: TLiteral);

    function ToString: AnsiString;
    function Copy: TBitVector;

  end;

implementation

{ TBitVector }

function TBitVector.GetBit(Index: Integer): TLiteral;
begin
  if Index < Size then
    Result:= Self[Index]
  else
    Result:= TSeitinVariableUnit.GetVariableManager.FalseLiteral;

end;

function TBitVector.GetCount: Integer;
begin
  Result := Size;
end;

procedure TBitVector.SetCount(AValue: Integer);
begin
  Resize(AValue);
end;

constructor TBitVector.Create(s: Integer);
var
  i: Integer;

begin
  inherited Create;

  Resize(s);
  for i:= 0 to s - 1 do
    Items[i]:= CreateLiteral(
             TSeitinVariableUnit.GetVariableManager.CreateNewVariable,
             False);

end;

constructor TBitVector.Create(s: Integer; Literal: TLiteral);
var
  i: Integer;

begin
  inherited Create;

  ReSize(s);
  for i:= 0 to s - 1 do
    Items[i]:= Literal;

end;

constructor TBitVector.Create;
begin
  inherited;

end;

procedure TBitVector.Add(l: TLiteral);
begin
  Self.PushBack(l);
end;

function TBitVector.ToString: AnsiString;
var
  i: Integer;
  S, Space: AnsiString;
  MaxLen: Integer;

begin
  Result:= '(';

  MaxLen:= -1;
  for i:= 0 to Self.Size - 2 do
    if MaxLen< Length(LiteralToString(Self[i])) then
      MaxLen:= Length(LiteralToString(Self[i]));
  if Size <> 0 then
    if MaxLen< Length(LiteralToString(Self[Size- 1])) then
      MaxLen:= Length(LiteralToString(Self[Size- 1]));

  Space:= '';
  for i:= 1 to MaxLen do
    Space+= ' ';

  for i:= 0 to Self.Size- 2 do
  begin
    S:= LiteralToString(Self[i])+ ',';
    Result+= System.Copy(Space, 1, MaxLen- Length(S))+ S;

  end;

  if Size<> 0 then
  begin
    S:= LiteralToString(Self[Size- 1])+ ',';
    Result+= System.Copy(Space, 1, MaxLen- Length(S))+ S;

  end;

  Result+= ')';

end;

function TBitVector.Copy: TBitVector;
var
  i: Integer;

begin
  Result:= TBitVector.Create(Size, GetVariableManager.FalseLiteral);

  for i:= 0 to Size- 1 do
    Result[i]:= Self[i];

end;

end.

