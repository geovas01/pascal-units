unit BinaryEncodingBasedFactoringUnit;

{$mode objfpc}{$H+}
{$ASSERTIONS on}
interface

uses
  Classes, SysUtils, FactoringUsingSATUnit, BigInt,
    BinaryArithmeticCircuitUnit, BitVectorUnit, ClauseUnit;

type
  { TBinaryRepBasedFactorizer }

  TBinaryRepBasedFactorizer = class(TBaseFactorizerUsingSAT)
  private
    BinaryArithmeticCircuit: TBinaryArithmeticCircuit;

  public
    constructor Create;
    destructor Destroy; override;

    function GenerateCNF(const a, b: TBitVector; n: TBigInt): TLiteral; override;

  end;


implementation
uses
  TSeitinVariableUnit, SatSolverInterfaceUnit, ParameterManagerUnit;
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

function TBinaryRepBasedFactorizer.GenerateCNF(const a, b: TBitVector;
  n: TBigInt): TLiteral;
var
  aBitCount, bBitCount: Integer;
  One: TBitVector;
  c: TBitVector;
  cBinRep,
  MulEncOutputLit,
  aLEb,
  aG1, bG1: TLiteral;

begin
  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;

  c := TBitVector.Create();
  cBinRep := BinaryArithmeticCircuit.EncodeBinaryRep(n, c);

  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(cBinRep);
  bBitCount := a.Count;
  aBitCount := b.Count;
  assert(aBitCount + bBitCount >= c.Count, 'a.Count + b.Count < c.Count');

  if GetRunTimeParameterManager.Verbosity<> 0 then
  begin
    WriteLn('[GenerateCNF] Encoding ', n.ToString, ' needs ', c.Count, ' bits');
    WriteLn('[GenerateCNF] n = a * b, where a is a ', aBitCount, '-bit integer and b is a ', bBitCount,'-bit integer.');

  end;

  if GetRunTimeParameterManager.Verbosity<> 0 then
  begin
    WriteLn('[GenerateCNF] a = ', a.ToString);
    WriteLn('[GenerateCNF] b = ', b.ToString);
    WriteLn('[GenerateCNF] c = ', c.ToString);

  end;

  MulEncOutputLit := BinaryArithmeticCircuit.EncodeMul(a, b, c);

  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(MulEncOutputLit);

  c.Free;

  if UpperCase(ParameterManagerUnit.GetRunTimeParameterManager.GetValueByName('--AddaLeb'))
    = UpperCase('True') then
    aLEb:= BinaryArithmeticCircuit.EncodeIsLessThanOrEq(a, b)
  else
    aLEb := GetVariableManager.TrueLiteral;

  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(aLEb);

  One := nil;
  if (a.Count = c.Count) or (b.Count = c.Count) then
  begin
    One:= TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);
    One[0]:= GetVariableManager.TrueLiteral;

    aG1:= BinaryArithmeticCircuit.EncodeIsLessThan(One, a);

    SatSolverInterfaceUnit.GetSatSolver.AddLiteral(aG1);

    bG1:=   BinaryArithmeticCircuit.EncodeIsLessThan(One, b);
    SatSolverInterfaceUnit.GetSatSolver.AddLiteral(bG1);

  end
  else
  begin
    aG1:= GetVariableManager.TrueLiteral;
    bG1:= GetVariableManager.TrueLiteral;

  end;

  One.Free;
  a.Free;
  b.Free;

  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);

  if GetRunTimeParameterManager.Verbosity<> 0 then
  begin
    WriteLn('cBinRep =', LiteralToString(cBinRep));
    WriteLn('aLEb =', LiteralToString(aLEb));
    WriteLn('aG1 =', LiteralToString(aG1));
    WriteLn('bG1 =', LiteralToString(bG1));
    WriteLn('Result =', LiteralToString(Result));
  end;

  SatSolverInterfaceUnit.GetSatSolver.SubmitAndGate(Result);
end;

end.

