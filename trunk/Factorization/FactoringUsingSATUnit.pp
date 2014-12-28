unit FactoringUsingSATUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BigInt, gvector, BitVectorUnit, ClauseUnit;

type
  TBigIntCollection = specialize TVector<TBigInt>;

  { TBaseFactorizerUsingSAT }

  TBaseFactorizerUsingSAT= class (TObject)
  public
    constructor Create;
    destructor Destroy; override;

    function GenerateCNF (const a, b: TBitVector; n: TBigInt): TLiteral; virtual; abstract;

  end;

  function GetActiveFactorizer: TBaseFactorizerUsingSAT;

  procedure Initialize;// (FactorizerMode: AnsiString);
  procedure Finalize;

implementation
uses
  BinaryEncodingBasedFactoringUnit, ParameterManagerUnit,
  FactoringUsingModulosUnit;

{ TBaseFactorizerUsingSAT }

constructor TBaseFactorizerUsingSAT.Create;
begin
  inherited Create;


end;

destructor TBaseFactorizerUsingSAT.Destroy;
begin
  inherited;
end;

var
  ActiveFactorizer: TBaseFactorizerUsingSAT;

function GetActiveFactorizer: TBaseFactorizerUsingSAT;
begin
  Result:= ActiveFactorizer;

end;

procedure Initialize;// (FactorizerMode: AnsiString);
begin
  if UpperCase (GetRunTimeParameterManager.GetValueByName ('--FactorizerMode'))=
           UpperCase ('BinaryRep') then
    ActiveFactorizer:= TBinaryRepBasedFactorizer.Create
  else if UpperCase (GetRunTimeParameterManager.GetValueByName ('--FactorizerMode'))=
           UpperCase ('ModuloRep') then
    ActiveFactorizer:= TBinaryModuloFactorizer.Create
  else
    raise Exception.Create ('Invalid Factorizer Mode!');

end;

procedure Finalize;
begin
  ActiveFactorizer.Free;
  ActiveFactorizer:= nil;

end;

initialization
  ActiveFactorizer:= nil;

finalization
  Finalize;

end.

