unit FactoringUsingSATUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BigInt, BitVectorUnit, ClauseUnit,
  BaseArithmeticCircuitUnit;

type

  { TBaseFactorizerUsingSAT }

  TBaseFactorizerUsingSAT= class (TObject)
  protected
    ArithmeticCircuit: TBaseArithmeticCircuit;

  public
    constructor Create;
    destructor Destroy; override;

    function GenerateCNF (const a, b: TBitVector; n: TBigInt): TLiteral; virtual;

  end;

  function GetActiveFactorizer: TBaseFactorizerUsingSAT;

  procedure Initialize;// (FactorizerMode: AnsiString);
  procedure Finalize;

implementation
uses
  BinaryEncodingBasedFactoringUnit, ParameterManagerUnit,
  ModuloBasedFactoringUnit, SatSolverInterfaceUnit, TSeitinVariableUnit;

{ TBaseFactorizerUsingSAT }

constructor TBaseFactorizerUsingSAT.Create;
begin
  inherited Create;

  ArithmeticCircuit := nil;
end;

destructor TBaseFactorizerUsingSAT.Destroy;
begin
  ArithmeticCircuit.Free;

  inherited;
end;

function TBaseFactorizerUsingSAT.GenerateCNF(const a, b: TBitVector; n: TBigInt
  ): TLiteral;
var
  aBitCount, bBitCount: Integer;
  One: TBitVector;
  c: TBitVector;
  cBinRep,
  MulEncOutputLit,
  aLEb,
  aG1, bG1: TLiteral;
  i: Integer;

begin
  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;

  c := TBitVector.Create();
  cBinRep := ArithmeticCircuit.EncodeBinaryRep(n, c);

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

  MulEncOutputLit := ArithmeticCircuit.EncodeMul(a, b, c);

  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(MulEncOutputLit);


  if UpperCase(ParameterManagerUnit.GetRunTimeParameterManager.GetValueByName('--AddaLeb'))
    = UpperCase('True') then
    aLEb:= ArithmeticCircuit.EncodeIsLessThanOrEq(a, b)
  else
    aLEb := GetVariableManager.TrueLiteral;

  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(aLEb);

  One := nil;
  if (a.Count >= c.Count) or (b.Count >= c.Count) then
  begin
    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    for i := 1 to a.Count - 1 do
      SatSolverInterfaceUnit.GetSatSolver.AddLiteral(a[i]);
    aG1:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SatSolverInterfaceUnit.GetSatSolver.SubmitAndGate(aG1);

    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    for i := 1 to b.Count - 1 do
      SatSolverInterfaceUnit.GetSatSolver.AddLiteral(b[i]);
    bG1:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SatSolverInterfaceUnit.GetSatSolver.SubmitAndGate(bG1);

  end
  else
  begin
    aG1:= GetVariableManager.TrueLiteral;
    bG1:= GetVariableManager.TrueLiteral;

  end;

  c.Free;
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

