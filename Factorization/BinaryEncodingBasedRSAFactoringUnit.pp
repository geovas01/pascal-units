unit BinaryEncodingBasedRSAFactoringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RSAFactoringUsingSATUnit, BigInt,
  BinaryArithmeticCircuitUnit;

type

  { TBinaryEncodingForRSAFactoring }

  { TBinaryRepBasedRSAFactorizer }

  TBinaryRepBasedRSAFactorizer = class(TBaseRSAFactorizerUsingSAT)
  private
    BinaryArithmeticCircuit: TBinaryArithmeticCircuit;

  public
    constructor Create;
    destructor Destroy; override;

    procedure GenerateCNF(n: TBigInt); override;
  end;

implementation
uses
  BitVectorUnit, ClauseUnit, ParameterManagerUnit,
  SatSolverInterfaceUnit, TSeitinVariableUnit;

{ TBinaryEncodingForRSAFactoring }

constructor TBinaryRepBasedRSAFactorizer.Create;
begin
  inherited Create;

  BinaryArithmeticCircuit:= TBinaryArithmeticCircuit.Create;

end;

destructor TBinaryRepBasedRSAFactorizer.Destroy;
begin
  BinaryArithmeticCircuit.Free;

  inherited Destroy;
end;

procedure TBinaryRepBasedRSAFactorizer.GenerateCNF(n: TBigInt);
var
  aBitCount, bBitCount: Integer;
  a, b, One: TBitVector;
  c, cPrime: TBitVector;
  isEqualLit,
    aLEb: TLiteral;
  aG1, bG1: TLiteral;

begin
  c := BinaryArithmeticCircuit.BinaryRep(n);
  bBitCount := C.Count;
  aBitCount := bBitCount;

  if GetRunTimeParameterManager.Verbosity<> 0 then
  begin
    WriteLn('[GenerateCNF] Encoding ', n.ToString, ' needs ', c.Count, ' bits');
    WriteLn('[GenerateCNF] n = a * b, where a is a ', aBitCount, '-bit integer and b is a ', bBitCount,'-bit integer.');

  end;

  a:= TBitVector.Create(aBitCount{- 1});
  b:= TBitVector.Create(bBitCount);

  if GetRunTimeParameterManager.Verbosity<> 0 then
  begin
    WriteLn('[GenerateCNF] a = ', a.ToString);
    WriteLn('[GenerateCNF] b = ', b.ToString);
    WriteLn('[GenerateCNF] c = ', c.ToString);

  end;


  cPrime:= BinaryArithmeticCircuit.Mul(a, b);
  if GetRunTimeParameterManager.Verbosity<> 0 then
  begin
    WriteLn('[GenerateCNF] Prime = ', cPrime.ToString);
    WriteLn('[GenerateCNF] c = ', c.ToString);

  end;

  IsEqualLit:= BinaryArithmeticCircuit.IsEqual(c, cPrime);

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(isEqualLit);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  cPrime.Free;

  aLEb:= BinaryArithmeticCircuit.IsLessThanOrEq(a, b);

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(aLEb);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  One:= TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);
  One[0]:= GetVariableManager.TrueLiteral;

  aG1:= BinaryArithmeticCircuit.IsLessThan(One, a);
  WriteLn('aG1 =', LiteralToString(aG1));
  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(aG1);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  bG1:= BinaryArithmeticCircuit.IsLessThan(One, b);
  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(bG1);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  One.Free;
  a.Free;
  b.Free;

end;

end.

