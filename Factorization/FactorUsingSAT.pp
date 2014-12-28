program FactorUsingSAT;

{$mode objfpc}{$H+}
{$ASSERTIONS ON}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SatSolverInterfaceUnit, ParameterManagerUnit, MiniSatSolverUnit,
  MiniSatSolverInterfaceUnit, SysUtils, contnrs, gvector, BigInt,
  FactoringUsingSATUnit, AbstractSolverUnit, CNFCollectionUnit,
  TSeitinVariableUnit, BinaryEncodingBasedFactoringUnit, BitVectorUnit,
  BinaryArithmeticCircuitUnit, BaseArithmeticCircuitUnit, BaseCircuitUnit,
FactoringUsingModulosUnit, GenericCollectionUnit, StreamUnit, PBConstraintUnit,
  AbstractPBModEncoderUnit;

procedure Initialize;
begin
  ParameterManagerUnit.Initialize;

  SatSolverInterfaceUnit.Initialize;
  TSeitinVariableUnit.Initialize;
  FactoringUsingSATUnit.Initialize;//('BinaryRep');

end;

procedure Finalize;
begin
  FactoringUsingSATUnit.Finalize;

  SatSolverInterfaceUnit.Finalize;
  TSeitinVariableUnit.Finalize;
  ParameterManagerUnit.Finalize;

end;

var
  n: TBigInt;
  InputNumber: AnsiString;
  a, b: TBitVector;

begin
  if Paramcount= 0 then
  begin
    WriteLn('Invalid Usage!');
    WriteLn(ParamStr(0) + ' n ');
    Halt(1);

  end;

  Initialize;

  InputNumber:= GetRunTimeParameterManager.ValueByName['--InputNumber'];
  n:= BigIntFactory.GetNewMemeber.LoadFromString(@InputNumber[1]);

  a:= TBitVector.Create(n.Log.ToInteger);
  b:= TBitVector.Create(n.Log.ToInteger);
  WriteLn('c a = ', a.ToString);
  WriteLn('c b = ', b.ToString);

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(FactoringUsingSATUnit.GetActiveFactorizer.GenerateCNF(a, b, n));//, CNFCollection);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  BigIntFactory.ReleaseMemeber(n);

  Finalize;
end.

