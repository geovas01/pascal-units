unit BinaryAdderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractAdderUnit, ClauseUnit;

type

  { TBinaryAdder }

  TBinaryAdder= class (TAbstractAdder)
  private
    Carries: TLiteralCollection;

  public
    procedure AddExtraClauses_Medium; override;

    function Encode: TLiteralCollection; override;

    constructor Create (n1, n2: TLiteralCollection);
    destructor Destroy; override;

  end;

implementation
uses
  TSeitinVariableUnit, Math;

{ TBinaryAdder }
{
Formula c = _0_;
out.clear();

for (int i = 0; i < max(xs.size(),ys.size()); i++){
    Formula x = i < xs.size() ? xs[i] : _0_;
    Formula y = i < ys.size() ? ys[i] : _0_;
    out.push(FAs(x,y,c));
    c         = FAc(x,y,c);
}
out.push(c);

while (out.last() == _0_)
    out.pop();
}

function TBinaryAdder.Encode: TLiteralCollection;
  //Returns a xor b xor c
  function FAs (a, b, c: TLiteral): TLiteral;
  var
    Literals: TLiteralCollection;

  begin
    Literals:= TLiteralCollection.Create;

    Literals.AddItem (a);
    Literals.AddItem (b);
    Literals.AddItem (c);

    Result:= GetVariableManager.CreateVariableDescribingXOR (Literals);

    Literals.Free;

  end;

  //Returns {a, b, c}>= 2
  function FAc (a, b, c: TLiteral): TLiteral;
  begin
    Result:= GetVariableManager.CreateVariableDescribingFACarry (a, b, c);

  end;

var
  Carry: TLiteral;
  xi, yi: TLiteral;
  i: Integer;

begin
  Carry:= GetVariableManager.FalseLiteral;
  Result:= TLiteralCollection.Create;
  Carries:= TLiteralCollection.Create;

  for i:= 0 to Max (x.Count, y.Count)- 1 do
  begin
    if i< x.Count then
      xi:= x.Item [i]
    else
      xi:= GetVariableManager.FalseLiteral;

    if i< y.Count then
      yi:= y.Item [i]
    else
      yi:= GetVariableManager.FalseLiteral;

    Result.AddItem (FAs (xi, yi, Carry));
    Carry:= FAc (xi, yi, Carry);
    Carries.AddItem (Carry);

  end;

end;

constructor TBinaryAdder.Create (n1, n2: TLiteralCollection);
begin
  inherited;

  Carries:= nil;

end;

destructor TBinaryAdder.Destroy;
begin
  Carries.Free;

  inherited Destroy;

end;

procedure TBinaryAdder.AddExtraClauses_Medium;
begin

end;


end.

