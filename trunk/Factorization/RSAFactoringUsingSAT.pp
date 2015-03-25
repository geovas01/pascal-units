program RSAFactoringUsingSAT;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SatSolverInterfaceUnit, ParameterManagerUnit, MiniSatSolverUnit,
  MiniSatSolverInterfaceUnit, SysUtils, contnrs, gvector, BigInt,
  AbstractSolverUnit, CNFCollectionUnit,
  TSeitinVariableUnit, BitVectorUnit,
  BinaryArithmeticCircuitUnit, BaseArithmeticCircuitUnit, BaseCircuitUnit,
GenericCollectionUnit, StreamUnit, PBConstraintUnit,
  AbstractPBModEncoderUnit, RSAFactoringUsingSATUnit,
FactoringUsingRSAModulosUnit,
BinaryModuloRSAFactorizerUnit;

procedure Initialize;
begin
  ParameterManagerUnit.Initialize;

  SatSolverInterfaceUnit.Initialize;
  TSeitinVariableUnit.Initialize;
  RSAFactoringUsingSATUnit.Initialize;//('BinaryRep');

end;

procedure Finalize;
begin
  RSAFactoringUsingSATUnit.Finalize;

  SatSolverInterfaceUnit.Finalize;
  TSeitinVariableUnit.Finalize;
  ParameterManagerUnit.Finalize;

end;

var
  n: TBigInt;
  InputNumber: AnsiString;

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

  RSAFactoringUsingSATUnit.GetActiveRSAFactorizer.GenerateCNF(n);//, CNFCollection);

  BigIntFactory.ReleaseMemeber(n);

  Finalize;
end.

