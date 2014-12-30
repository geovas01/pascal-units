unit PBModEncoderUsingAdderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractSorterEncoderUnit, AbstractPBModEncoderUnit,
  ClauseUnit, TSeitinVariableUnit, CollectionUnit, PBConstraintUnit;

type

  { TPBModEncoderUsingAdders }

  TPBModEncoderUsingAdders= class(TAbstractPBModEncoder)
  private
  protected
    DP: TListOfLiteralCollection;
    Adders: array of TAbstractSorterEncoder;
    OutputOfAdders: TListOfLiteralCollection;
    OutputOfIF: TListOfLiteralCollection;
    OutputOfSubtracter: TListOfLiteralCollection;


    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function EncodePBMod: TLiteral; override;

    constructor Create(_OrigConsrraint: TPBConstraint; _VariableManager: TVariableManager;
      _Coefs: TInt64Collection; _b: int64;
      _OrigSum: TPBSum; _Modulo: integer);
    destructor Destroy; override;

    procedure AddExtraClauses; override;

  end;

implementation
uses
  BigInt;

{ TPBModEncoderUsingAdders }

procedure TPBModEncoderUsingAdders.AddExtraClauses_Medium;
begin
  inherited AddExtraClauses_Medium;
end;

procedure TPBModEncoderUsingAdders.AddExtraClauses_High;
begin
  inherited AddExtraClauses_High;
end;

function TPBModEncoderUsingAdders.EncodePBMod: TLiteral;
var
  InputLiterals: TLiteralCollection;
  i, j: Integer;
  m, Res: TBigInt;

begin
  InputLiterals:= TLiteralCollection.Create;
  m:= BigIntFactory.GetNewMemeber.SetValue(Modulo);

  for i:= 0 to OrigSum.Count- 1 do
  begin
    Res:= OrigSum.Coef [i].Modulo(m);

    for j:= 1 to Res.GetValue do
      InputLiterals.PushBack(OrigSum.Literal [i]);

  end;
  raise Exception.Create('Not Implemented Yet!');
  Result:= 0;

end;

constructor TPBModEncoderUsingAdders.Create(_OrigConsrraint: TPBConstraint; _VariableManager: TVariableManager;
  _Coefs: TInt64Collection; _b: int64; _OrigSum: TPBSum;
  _Modulo: integer);
begin
  inherited;

end;

destructor TPBModEncoderUsingAdders.Destroy;
begin
  inherited Destroy;
end;

procedure TPBModEncoderUsingAdders.AddExtraClauses;
begin
  inherited AddExtraClauses;
end;

end.

