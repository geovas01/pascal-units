unit PBModEncoderDCUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractPBModEncoderUnit, PBConstraintUnit,
  ClauseUnit, TSeitinVariableUnit, CollectionUnit;

type
  { TPBModEncoderDC }

  TPBModEncoderDC= class (TAbstractPBModEncoder)
  private
  protected
    Memory: array of TListOfLiteralCollection;

    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function EncodePBMod: TLiteral; override;

    destructor Destroy; override;
    constructor Create (_VariableManager: TVariableManager;
                          _Coefs: TInt64Collection; _b: Int64;
                          _OrigSum: TPBSum; _Permutation: TIntegerCollection;
                           _Modulo: Integer);

  end;


implementation
uses
  ParameterManagerUnit;

{ TPBModEncoderDC }

procedure TPBModEncoderDC.AddExtraClauses_Medium;
var
  l, i: Integer;
  ActiveAnswer: TLiteralCollection;
  b1, b2: Integer;

begin
  for l:= 0 to High (Memory) do
    for i:= 0 to Memory [l].Count- 1 do
      begin
        ActiveAnswer:= Memory [l].Item [i];

        for b1:= 0 to Modulo- 1 do
          if GetVar (ActiveAnswer.Item [b1])<> 0 then
            for b2:= b1+ 1 to Modulo- 1 do
              if GetVar (ActiveAnswer.Item [b2])<> 0 then
              begin
                VariableGenerator.SatSolver.BeginConstraint;

                VariableGenerator.SatSolver.AddLiteral (NegateLiteral (ActiveAnswer.Item [b1]));
                VariableGenerator.SatSolver.AddLiteral (NegateLiteral (ActiveAnswer.Item [b2]));

                VariableGenerator.SatSolver.SubmitClause;// Result[i]=> \lnot Result [j]

              end;

      end;

  for l:= 0 to High (Memory) do
    for i:= 0 to Memory [l].Count- 1 do
    begin
      ActiveAnswer:= Memory [l].Item [i];

      VariableGenerator.SatSolver.BeginConstraint;
      for b1:= 0 to Modulo- 1 do
        VariableGenerator.SatSolver.AddLiteral (ActiveAnswer.Item [b1]);
      VariableGenerator.SatSolver.SubmitClause;// Result [0] or  Result [1] or ... or Result [m]

    end;

end;

procedure TPBModEncoderDC.AddExtraClauses_High;
begin
// Do Nothing ..
end;

function TPBModEncoderDC.EncodePBMod: TLiteral;

  function Encode (Index: Integer; Len: Integer): TLiteralCollection;
  {
    Create an answer for InputLiterals [Index, ..., Index+ Len- 1]
  }
  var
    Temp: TLiteralCollection;
    Left, Right: TLiteralCollection;
    i, j: Integer;

  begin

    if Len= 1 then
    begin
      Result:= TLiteralCollection.Create (Modulo, VariableGenerator.FalseLiteral);

      if Coefs.Item [Index]<> 0 then
      begin
        Result.Item [Coefs.Item [Index]]:= CopyLiteral (OrigSum.Item [Permutation.Item [Index]].Literal);
        Result.Item [0]:= CopyLiteral (NegateLiteral (OrigSum.Item [Permutation.Item [Index]].Literal));

      end
      else
        Result.Item [0]:= VariableGenerator.TrueLiteral;

      Memory [Len].AddItem (Result);

    end
    else
    begin
      Left:= Encode (Index, Len div 2);
      Right:= Encode (Index+ Len div 2, Len- Len div 2);

      Result:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);

      Temp:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);

      for i:= 0 to Modulo- 1 do
      begin

        for j:= 0 to Modulo- 1 do
          Temp.Item [j]:= VariableGenerator.CreateVariableDescribingAND
                               (
                               Left.Item [j],
                               Right.Item [(i- j+ Modulo) mod Modulo]
                               );
        Result.Item [i]:= VariableGenerator.CreateVariableDescribingOR (Temp);

      end;

      Temp.Free;

{
      Left.Free;
      Right.Free;
}
      Memory [Len].AddItem (Result);

    end;

  end;

var
  Temp: TLiteralCollection;

begin
  if OrigSum.Count= 0 then
  begin
    if b= 0 then
      Result:=  VariableGenerator.TrueLiteral
    else
      Result:=  VariableGenerator.FalseLiteral;

  end
  else
  begin
    Temp:= Encode (0, OrigSum.Count);
    Result:= Temp.Item [b];

  end;

end;

destructor TPBModEncoderDC.Destroy;
var
  i: Integer;

begin

  for i:= 0 to High (Memory) do
    Memory [i].Free;
  SetLength (Memory, 0);

  inherited;

end;

constructor TPBModEncoderDC.Create(_VariableManager: TVariableManager;
  _Coefs: TInt64Collection; _b: Int64; _OrigSum: TPBSum;
  _Permutation: TIntegerCollection; _Modulo: Integer);
var
  i: Integer;

begin
  inherited;

  SetLength (Memory, OrigSum.Count+ 1);
  for i:= 0 to High (Memory) do
    Memory [i]:= TListOfLiteralCollection.Create;

end;

end.

