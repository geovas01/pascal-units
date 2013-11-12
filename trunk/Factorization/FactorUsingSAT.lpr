program FactorUsingSAT;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SatSolverInterfaceUnit, ParameterManagerUnit, MiniSatSolverUnit,
  SysUtils, BigInt, FactoringUsingSATUnit, AbstractSolverUnit,
  CNFCollectionUnit, TSeitinVariableUnit, BinaryEncodingForFactoringUnit,
  BitVector, BaseCircuitUnit;

var
  n: Integer;
  CNFCollection: TCNFCollection;

begin
  if Paramcount= 0 then
  begin
    WriteLn ('Invalid Usage!');
    WriteLn (ParamStr (0)+ ' n ');
    Halt (1);

  end;

  WriteLn (Paramcount);
  ParameterManagerUnit.Initialize;
  FactoringUsingSATUnit.Initialize ('BinaryRep');
  n:= StrToInt (GetRunTimeParameterManager.ValueByName ['--InputNumber']);

  CNFCollection:= TCNFCollection.Create;
  FactoringUsingSATUnit.GetActiveFactorizer.GenerateCNF (
                BigIntFactory.GetNewMemeber.SetValue (n), CNFCollection);

end.

