unit BinaryEncodingForFactoringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FactoringUsingSATUnit, BigInt,
    BitVectorUnit, BinaryArithmeticCircuitUnit;

type
  { TBinaryRepBasedFactorizer }

  TBinaryRepBasedFactorizer= class (TBaseFactorizerUsingSAT)
  private
    BinaryArithmeticCircuit: TBinaryArithmeticCircuit;

  public
    constructor Create;
    destructor Destroy; override;

    procedure GenerateCNF (n: TBigInt); override;

  end;


implementation
uses
  TSeitinVariableUnit, ClauseUnit, SatSolverInterfaceUnit, ParameterManagerUnit;

{ TBinaryRepBasedFactorizer }

constructor TBinaryRepBasedFactorizer.Create;
begin
  inherited Create;

  BinaryArithmeticCircuit:= TBinaryArithmeticCircuit.Create;
end;

destructor TBinaryRepBasedFactorizer.Destroy;
begin
  BinaryArithmeticCircuit.Free;

  inherited Destroy;

end;

procedure TBinaryRepBasedFactorizer.GenerateCNF (n: TBigInt);
var
  BitCount: Integer;
  P2: TBigInt;
  a, b, One: TBitVector;
  c, cPrime: TBitVector;
  isEqualLit,
    aLEb: TLiteral;
  i: Integer;
  AndResult: TBigInt;
  aG1, bG1: TLiteral;

begin
  P2:= BigIntFactory.GetNewMemeber.SetValue (2);
  BitCount:= 1;

  while P2.CompareWith (n)<= 0 do
  begin
    Inc (BitCount);
    P2.Add (P2);
    WriteLn (BitCount, ' ', n.ToString, ' ', P2.ToString);

  end;

  c:= TBitVector.Create (BitCount, GetVariableManager.FalseLiteral);
  P2.SetValue (1);
  for i:= 0 to BitCount- 1 do
  begin
    AndResult:= n.ArithmaticAnd (P2);
//    WriteLn (P2.ToString, ' ', AndResult.ToString);

    if AndResult.IsZero then
      c [i]:= GetVariableManager.FalseLiteral
    else
      c [i]:= GetVariableManager.TrueLiteral;

    BigIntFactory.ReleaseMemeber (AndResult);

    P2.Add (P2);

  end;

  BigIntFactory.ReleaseMemeber (P2);

  if GetRunTimeParameterManager.Verbosity<> 0 then
  begin
    WriteLn ('[GenerateCNF] Encoding ', n.ToString, ' needs ', BitCount, ' bits');
    WriteLn ('[GenerateCNF] n = a * b, where a is a ', (BitCount + 1) div 2, '-bit integer and b has a ', BitCount,'-bit integer.');

  end;

  a:= TBitVector.Create (BitCount{- 1});
  b:= TBitVector.Create (BitCount);

  if GetRunTimeParameterManager.Verbosity<> 0 then
  begin
    WriteLn ('[GenerateCNF] a = ', a.ToString);
    WriteLn ('[GenerateCNF] b = ', b.ToString);
    WriteLn ('[GenerateCNF] c = ', c.ToString);

  end;


  cPrime:= BinaryArithmeticCircuit.Mul (a, b);
  if GetRunTimeParameterManager.Verbosity<> 0 then
  begin
    WriteLn ('[GenerateCNF] Prime = ', cPrime.ToString);
    WriteLn ('[GenerateCNF] c = ', c.ToString);

  end;

  IsEqualLit:= BinaryArithmeticCircuit.IsEqual (c, cPrime);

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral (isEqualLit);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  cPrime.Free;

  aLEb:= BinaryArithmeticCircuit.IsLessThanOrEq (a, b);

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral (aLEb);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  One:= TBitVector.Create (a.Count, GetVariableManager.FalseLiteral);
  One [0]:= GetVariableManager.TrueLiteral;

  aG1:= BinaryArithmeticCircuit.IsLessThan (One, a);
  WriteLn ('aG1 =', LiteralToString (aG1));
  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral (aG1);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  bG1:= BinaryArithmeticCircuit.IsLessThan (One, b);
  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral (bG1);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  One.Free;
  a.Free;
  b.Free;

end;

end.

