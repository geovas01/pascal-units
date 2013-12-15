program FactorUsingSAT;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SatSolverInterfaceUnit, ParameterManagerUnit, MiniSatSolverUnit,
  SysUtils, BigInt, FactoringUsingSATUnit, AbstractSolverUnit,
  CNFCollectionUnit, TSeitinVariableUnit, BinaryEncodingForFactoringUnit,
  BitVectorUnit, BaseCircuitUnit, BinaryArithmeticCircuitUnit,
  BaseArithmeticCircuitUnit, FactoringUsingModulos, GenericCollectionUnit;

procedure Initialize;
begin
  ParameterManagerUnit.Initialize;

  SatSolverInterfaceUnit.Initialize;
  TSeitinVariableUnit.Initialize;
  FactoringUsingSATUnit.Initialize;// ('BinaryRep');

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

begin
  if Paramcount= 0 then
  begin
    WriteLn ('Invalid Usage!');
    WriteLn (ParamStr (0)+ ' n ');
    Halt (1);

  end;
  Initialize;


  InputNumber:= GetRunTimeParameterManager.ValueByName ['--InputNumber'];
  n:= BigIntFactory.GetNewMemeber.LoadFromString (@InputNumber[1]);

  FactoringUsingSATUnit.GetActiveFactorizer.GenerateCNF (n);//, CNFCollection);

  Finalize;
end.

