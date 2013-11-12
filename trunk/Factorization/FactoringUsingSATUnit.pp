unit FactoringUsingSATUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BigInt, GenericCollectionUnit,
    SatSolverInterfaceUnit;

type
  TBigIntCollection= specialize TGenericCollection<TBigInt>;

  { TBaseFactorizerUsingSAT }

  TBaseFactorizerUsingSAT= class (TObject)
  private
    function GetPrime (Index: Integer): TBigInt; virtual;
    function GetPrimeCount: Integer;  virtual;
  protected
    property Prime [Index: Integer]: TBigInt read GetPrime;
    property PrimeCount: Integer read GetPrimeCount;

  protected
    FPrimes: TBigIntCollection;
    PrimeProduct: TBigInt;

    function GenerateNextPrime: TBigInt;

  public
    constructor Create;
    destructor Destroy; override;

    procedure GenerateCNF (n: TBigInt; SatSolver: TSATSolverInterface); virtual; abstract;

  end;

  function GetActiveFactorizer: TBaseFactorizerUsingSAT;

  procedure Initialize (FactorizerMode: AnsiString);
implementation
uses
  BinaryEncodingForFactoringUnit;

{ TBaseFactorizerUsingSAT }

function TBaseFactorizerUsingSAT.GetPrime(Index: Integer): TBigInt;
begin
  Result:= FPrimes.Item [Index];

end;

function TBaseFactorizerUsingSAT.GetPrimeCount: Integer;
begin
  Result:= FPrimes.Count;

end;

function TBaseFactorizerUsingSAT.GenerateNextPrime: TBigInt;

  function IsPrime (n: TBigInt): Boolean;
  var
    i: Integer;
    Tmp: TBigInt;

  begin
    Result:= False;

    for i:= 0 to PrimeCount- 1 do
    begin
      if n.Modulo (Prime [i]).IsZero then
        Exit;

      Tmp:= Prime [i].Mul (Prime [i]);
      if Tmp.CompareWith (n)> 0 then
        break;

    end;

    Result:= True;;


  end;

var
  Tmp: TBigInt;

begin
  Result:= FPrimes.LastItem.Copy;
  Result.Incr;
  while not IsPrime (Result) do
    Result.Incr;

  FPrimes.AddItem (Result);
  Tmp:= PrimeProduct.Mul (Result);
  PrimeProduct.Free;
  PrimeProduct:= Tmp;

end;

constructor TBaseFactorizerUsingSAT.Create;
begin
  inherited Create;

  FPrimes:= TBigIntCollection.Create;
  FPrimes.AddItem (BigIntFactory.GetNewMemeber.SetValue (2));

  PrimeProduct:= BigIntFactory.GetNewMemeber.SetValue (2);

end;

destructor TBaseFactorizerUsingSAT.Destroy;
var
  i: Integer;

begin
  for i:= 0 to FPrimes.Count- 1 do
    BigIntFactory.ReleaseMemeber (FPrimes.Item [i]);

  FPrimes.Clear;
  FPrimes.Free;

  BigIntFactory.ReleaseMemeber (PrimeProduct);

  inherited Destroy;

end;

var
  ActiveFactorizer: TBaseFactorizerUsingSAT;

function GetActiveFactorizer: TBaseFactorizerUsingSAT;
begin
  Result:= ActiveFactorizer;

end;

procedure Initialize(FactorizerMode: AnsiString);
begin
  if FactorizerMode= 'BinaryRep' then
    ActiveFactorizer:= TBinaryRepBasedFactorizer.Create
  else
    raise Exception.Create ('Invalid Factorizer Mode!');

end;

initialization
  ActiveFactorizer:= nil;

finalization
  ActiveFactorizer.Free;

end.

