unit FactoringUsingModulosUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FactoringUsingSATUnit, BigInt,
  BitVectorUnit, BinaryArithmeticCircuitUnit, gvector;

type

  { TBaseFactoringUsingModulo }

  TBaseFactoringUsingModulo= class(TBaseFactorizerUsingSAT)
  private
    BinaryArithmeticCircuit: TBinaryArithmeticCircuit;

    function GetPrime(Index: Integer): TBigInt; virtual;
    function GetPrimeCount: Integer;  virtual;
  protected
    property Prime [Index: Integer]: TBigInt read GetPrime;
    property PrimeCount: Integer read GetPrimeCount;

  protected
    FPrimes: TBigIntCollection;
    PrimeProduct: TBigInt;

    function GenerateNextPrime: TBigInt;

    function GetModulos(n: TBigInt): TBigIntCollection;

    function Modulo(a: TBitVector; m: TBigInt; ResultBitCount: Integer):
                     TBitVector; virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;

    procedure GenerateCNF(n: TBigInt); override;

  end;

  { TBinaryModuloFactorizer }

  TBinaryModuloFactorizer= class(TBaseFactoringUsingModulo)
  protected
    function Modulo(a: TBitVector; m: TBigInt; ResultBitCount: Integer):
                     TBitVector; override;

  public
    constructor Create;
    destructor Destroy; override;

  end;

 const
    VerbBaseFactoringUsingModulo: Integer = 3;

implementation
uses
  ClauseUnit, TSeitinVariableUnit, ParameterManagerUnit,
  GenericCollectionUnit, SatSolverInterfaceUnit, PBConstraintUnit;

function TBinaryModuloFactorizer.Modulo(a: TBitVector; m: TBigInt;
  ResultBitCount: Integer): TBitVector;
var
  i: Integer;
  P2, P2ModM: TBigInt;
  ciCoef: TBigInt;
  ciLit : TBitVector;

begin// Sum a_i2^i = Sum a_i (2^i mod M)
  Result:= TBitVector.Create(ResultBitCount, GetVariableManager.FalseLiteral);
  P2:= BigIntFactory.GetNewMemeber.SetValue(1);
  for i:= 1 to ResultBitCount do
    P2.Add(P2);

  Assert(0 < P2.CompareWith(m));
  BigIntFactory.ReleaseMemeber(P2);

  P2:= BigIntFactory.GetNewMemeber.SetValue(1);

  for i:= 0 to a.Count- 1 do
  begin
    ciCoef := P2.Modulo(m);
  //  ciLit := a[i];


  end;

  BigIntFactory.ReleaseMemeber(P2);

end;

constructor TBinaryModuloFactorizer.Create;
begin
  inherited;

end;

destructor TBinaryModuloFactorizer.Destroy;
begin
  inherited Destroy;
end;


{ TBaseFactoringUsingModulo }

function TBaseFactoringUsingModulo.GetPrime(Index: Integer): TBigInt;
begin
  Result:= FPrimes.Items[Index];

end;

function TBaseFactoringUsingModulo.GetPrimeCount: Integer;
begin
  Result:= FPrimes.Size;

end;

function TBaseFactoringUsingModulo.GenerateNextPrime: TBigInt;

  function IsPrime(n: TBigInt): Boolean;
  var
    i: Integer;
    Tmp: TBigInt;

  begin
    Result:= False;

    for i:= 0 to PrimeCount- 1 do
    begin
      if n.Modulo(Prime [i]).IsZero then
        Exit;

      Tmp:= Prime [i].Mul(Prime [i]);
      if Tmp.CompareWith(n)> 0 then
        break;

    end;

    Result:= True;;


  end;

var
  Tmp: TBigInt;

begin
  Result:= FPrimes.Back.Copy;
  Result.Incr;
  while not IsPrime(Result) do
    Result.Incr;

  FPrimes.PushBack(Result);
  Tmp:= PrimeProduct.Mul(Result);
  BigIntFactory.ReleaseMemeber(PrimeProduct);
  PrimeProduct:= Tmp;

end;

function TBaseFactoringUsingModulo.GetModulos(n: TBigInt): TBigIntCollection;
var
  nCopy: TBigInt;
  m, Temp, SqN2: TBigInt;
  i: Integer;

begin
  SqN2:= n.Pow(2);
  Result:= TBigIntCollection.Create;

  while PrimeProduct.CompareWith(SqN2)<= 0 do
    GenerateNextPrime;

  Result.PushBack(Prime[0]);
  Result.PushBack(Prime [1]);
  m:= Prime [1].Copy;
  for i:= 2 to PrimeCount- 1 do
  begin
    Result.PushBack(Prime [i]);
    Temp:= m.Mul(Prime [i]);
    BigIntFactory.ReleaseMemeber(m);
    m:= Temp;

    if SqN2.CompareWith(Temp)< 0 then
      break;

  end;

  BigIntFactory.ReleaseMemeber(m);
  BigIntFactory.ReleaseMemeber(SqN2);

end;

constructor TBaseFactoringUsingModulo.Create;
var
  P2: TBigInt;

begin
  inherited Create;

  BinaryArithmeticCircuit:= TBinaryArithmeticCircuit.Create;
  FPrimes:= TBigIntCollection.Create;
  P2:= BigIntFactory.GetNewMemeber;
  P2.SetValue(2);
  FPrimes.PushBack(P2);

  PrimeProduct:= BigIntFactory.GetNewMemeber;
  PrimeProduct.SetValue(2);

end;

destructor TBaseFactoringUsingModulo.Destroy;
var
  i: Integer;

begin
  for i:= 0 to FPrimes.Size - 1 do
    BigIntFactory.ReleaseMemeber(FPrimes.Items[i]);

  BigIntFactory.ReleaseMemeber(PrimeProduct);
  FPrimes.Clear;
  FPrimes.Free;

  BigIntFactory.ReleaseMemeber(PrimeProduct);

  BinaryArithmeticCircuit.Free;

  inherited Destroy;
end;

procedure TBaseFactoringUsingModulo.GenerateCNF(n: TBigInt);
var
  i: Integer;
  MCount: Integer;
  Temp, m, cmodMInt: TBigInt;
  Modulos: TBigIntCollection;
  BitCount, Logm: Integer;

  One, a, b, c: TBitVector;
  aModm, bModm, cModm, mBin,
  cPrime, cPrimeModm: TBitVector;
  P2, AndResult: TBigInt;
  IsEqualLit: TLiteral;

begin
  Modulos:= GetModulos(n);

  c:= BinaryArithmeticCircuit.BinaryRep(n);
  BitCount:= c.Count;

  if (GetRunTimeParameterManager.Verbosity and
     (1 shl VerbBaseFactoringUsingModulo)) <> 0 then
  begin
    WriteLn('[BaseFactoringUsingModulo.GenerateCNF] Encoding ', n.ToString, ' needs ', BitCount, ' bits');
    WriteLn('[BaseFactoringUsingModulo.GenerateCNF] n = a * b, where a is a ', BitCount, '-bit integer and b has a ', BitCount,'-bit integer.');

  end;

  a:= TBitVector.Create(BitCount{- 1});
  b:= TBitVector.Create(BitCount);

  if (GetRunTimeParameterManager.Verbosity and
     (1 shl VerbBaseFactoringUsingModulo)) <> 0 then
  begin
    WriteLn('[BaseFactoringUsingModulo.GenerateCNF] a = ', a.ToString);
    WriteLn('[BaseFactoringUsingModulo.GenerateCNF] b = ', b.ToString);
    WriteLn('[BaseFactoringUsingModulo.GenerateCNF] c = ', c.ToString);

  end;

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(
      BinaryArithmeticCircuit.IsLessThanOrEq(a, b));
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral
     (BinaryArithmeticCircuit.IsLessThan(b, c));
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  Temp:= BigIntFactory.GetNewMemeber.SetValue(1);
  One:= BinaryArithmeticCircuit.BinaryRep(Temp, a.Count);
  BigIntFactory.ReleaseMemeber(Temp);

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(
    BinaryArithmeticCircuit.IsLessThan(One, a));// AG1
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(
    BinaryArithmeticCircuit.IsLessThan(One, b)); // bG1
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  One.Free;

  cmodMInt:= nil;
  for i:= 0 to Modulos.Size- 1 do
  begin
    m:= Modulos.Items[i];
    mBin:= BinaryArithmeticCircuit.BinaryRep(m);
    cmodMInt:= n.Modulo(m);

    if GetRunTimeParameterManager.Verbosity and
       (1 shl VerbBaseFactoringUsingModulo) <> 0 then
    begin
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: M [', i, '] = ', m.ToString, ' mBin= ', mBin.ToString);
    end;

    aModm:= Modulo(a, m, mBin.Count);
    bModm:= Modulo(b, m, mBin.Count);

    cModm:= BinaryArithmeticCircuit.BinaryRep(cmodMInt);

    if GetRunTimeParameterManager.Verbosity and
       (1 shl VerbBaseFactoringUsingModulo) <> 0 then
    begin
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: aModm =', aModm.ToString);
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: bModm =', bModm.ToString);
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: cModm =', cModm.ToString);
    end;

    cPrime:= BinaryArithmeticCircuit.Mul(aModm, bModm);
    cPrimeModm:= BinaryArithmeticCircuit.Remainder(cPrime, mBin);
    if GetRunTimeParameterManager.Verbosity and
       (1 shl VerbBaseFactoringUsingModulo) <> 0 then
    begin
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: cPrime =', cPrime.ToString);
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: cPrimeMode =',
                                    cPrimeModm.ToString);
    end;

    IsEqualLit:= BinaryArithmeticCircuit.IsEqual(cModm, cPrimeModm);
    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    SatSolverInterfaceUnit.GetSatSolver.AddLiteral(IsEqualLit);
    SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

    mBin.Free;
    aModm.Free;
    bModm.Free;
    cModm.Free;
    cPrime.Free;
    cPrimeModm.Free;
    BigIntFactory.ReleaseMemeber(cmodMInt);

  end;

  Modulos.Clear;
  Modulos.Free;

end;

end.

