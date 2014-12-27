unit RSAFactoringUsingSATUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gvector, BigInt;

type
  TBigIntCollection = specialize TVector<TBigInt>;

  { TBaseRSAFactorizerUsingSAT }

  TBaseRSAFactorizerUsingSAT = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;

    procedure GenerateCNF(n: TBigInt); virtual; abstract;

  end;

  function GetActiveRSAFactorizer: TBaseRSAFactorizerUsingSAT;

  procedure Initialize;
  procedure Finalize;

implementation

uses
  BinaryEncodingBasedRSAFactoringUnit, ParameterManagerUnit,
  BinaryModuloRSAFactorizerUnit;

{ TBaseFactorizerUsingSAT }

constructor TBaseRSAFactorizerUsingSAT.Create;
begin
  inherited Create;


end;

destructor TBaseRSAFactorizerUsingSAT.Destroy;
begin
  inherited;
end;

var
  ActiveRSAFactorizer: TBaseRSAFactorizerUsingSAT;

function GetActiveRSAFactorizer: TBaseRSAFactorizerUsingSAT;
begin
  Result:= ActiveRSAFactorizer;

end;

procedure Initialize;// (FactorizerMode: AnsiString);
begin
  if UpperCase (GetRunTimeParameterManager.GetValueByName ('--RSAFactorizerMode'))=
           UpperCase ('BinaryRep') then
    ActiveRSAFactorizer := TBinaryRepBasedRSAFactorizer.Create
  else if UpperCase (GetRunTimeParameterManager.GetValueByName ('--RSAFactorizerMode'))=
           UpperCase ('ModuloRep') then
    ActiveRSAFactorizer := TBinaryModuloRSAFactorizer.Create
  else
    raise Exception.Create ('Invalid RSAFactorizerMode!');
end;

procedure Finalize;
begin
  ActiveRSAFactorizer.Free;
  ActiveRSAFactorizer:= nil;

end;

initialization
  ActiveRSAFactorizer:= nil;

finalization
  Finalize;

end.
