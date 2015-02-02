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
ModuloBasedFactoringUnit, GenericCollectionUnit, StreamUnit, PBConstraintUnit,
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
  n:= BigIntFactory.GetNewMember.LoadFromString(@InputNumber[1]);

  a:= TBitVector.Create(n.Log);
  b:= TBitVector.Create(n.Log);
  WriteLn('c a = ', a.ToString);
  WriteLn('c b = ', b.ToString);

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(FactoringUsingSATUnit.GetActiveFactorizer.GenerateCNF(a, b, n));//, CNFCollection);
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  BigIntFactory.ReleaseMemeber(n);

  Finalize;
end.

Hi Reza,

  In theory, you do not need to talk to any professors before applying but IMO, it is a good idea to do so (Specifically in the case of SFU, where you need to name two/three professors in your application). Due to the large number of applicants, usually the profs you named  on your application will review your file.
  Having said that, some of the profs do not like to reply to the applicants email before seeing their applications.

  If you have already sent 10 emails, just wait and do not send anymore emails. Wait and pray:P

Good luck,
Amir
