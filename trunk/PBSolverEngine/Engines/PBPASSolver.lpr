{%RunWorkingDir ./}
program PBPASSolver;

{$mode objfpc}{$H+}
{$DEFINE SYSTEMINLINE}
{$ASSERTIONS ON}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, 
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, TSeitinVariableUnit, SatSolverInterfaceUnit, PBParserUnit,
  ProblemDescriptionUnit, AbstractPBSolverUnit,
  //heaptrc,
  BaseUnix, ParameterManagerUnit, WideStringUnit;

var
  ProblemType: TSpecMode;

procedure TermSignHandler (aSig : cInt);cdecl;
begin
  if aSig<> SIGTerm then
    Exit;

  case ProblemType of
    smDecision:
      ;
    smOptimization:
      GetSolverEngine.ProcessSigTerm;

  end;

  halt;
end;

procedure Initialize;
begin
  FpSignal (SIGTerm, @TermSignHandler);
  ParameterManagerUnit.Initialize;
  SatSolverInterfaceUnit.Initialize;
  TSeitinVariableUnit.Initialize;
  AbstractPBSolverUnit.Initialize;

end;

procedure Finalize;
begin
  SatSolverInterfaceUnit.Finalize;
  TSeitinVariableUnit.Finalize;
  AbstractPBSolverUnit.Finalize;
  ParameterManagerUnit.Finalize;

end;

function CreateLazyPBParser (Stream: TStream): TPBParser;
var
  S: array [0..100] of Char;

begin
  S:= '';
  Stream.Read (S, 100);
  Stream.Position:= 0;

  if UpperCase (Copy (S, 1, Length ('* Version 2')))= UpperCase ('* Version 2') then
    Result:= TLazyVersion2PBParser.Create (Stream)
  else
    Result:= TLazyPBParser.Create (Stream);

end;

function CreateEagerParser (Stream: TStream): TPBParser;
var
  S: array [0..100] of Char;

begin
  S:= '';
  Stream.Read (S, 100);
  Stream.Position:= 0;

  if UpperCase (Copy (S, 1, Length ('Vestion 2')))= UpperCase ('Version 2') then
    Result:= TPBParser.Create (Stream)
  else
    Result:= TPBVersion2Parser.Create (Stream);

end;

{$IFDEF WINDOWS}{$R PBPASSolver.rc}{$ENDIF}

procedure Verify;
var
  Assignment: TAssignments;
  PBParser: TPBParser;
  Spec: TPBSpecification;
  Stream: TFileStream;

begin
  WriteLn ('<Main>');

  Stream:= TFileStream.Create (GetRunTimeParameterManager.GetValueByName ('--InputFilename'), fmOpenRead);
  Assignment:= TAssignments.Load (
        TFileStream.Create (GetRunTimeParameterManager.ValueByName ['--Assignment'], fmOpenRead));

  PBParser:= CreateLazyPBParser (Stream);
  Spec:= PBParser.ParsePB;

  if GetSolverEngine.Verify (Spec, Assignment) then
    WriteLn ('<Verified\>')
  else
    WriteLn ('<Incorrect\>');

  Spec.Free;
  Finalize;
  WriteLn ('</Main>');

end;

var
  PBParser: TPBParser;
  Stream: TFileStream;
  Spec: TPBSpecification;

begin
  Initialize;

  if UpperCase (GetRunTimeParameterManager.ValueByName ['--Assignment'])<> '' then
  begin
    Verify;
    Exit;

  end;

  WriteLn ('<Main>');
  Stream:= TFileStream.Create (GetRunTimeParameterManager.GetValueByName ('--InputFilename'), fmOpenRead);

  if UpperCase (GetRunTimeParameterManager.ValueByName ['--Parser'])= UpperCase ('LazyParser') then
  begin
    PBParser:= CreateLazyPBParser (Stream);
    Spec:= PBParser.ParsePB;

  end
  else
  begin
    PBParser:= CreateEagerParser (Stream);

    Spec:= PBParser.ParsePB;
    PBParser.Free;

  end;

  Stream.Free;

  ProblemType:= Spec.SpecMode;
  if GetRunTimeParameterManager.Verbosity and Ord (vbFull)<> 0 then
    WriteLn (Spec.ToString);

{  if UpperCase (GetRunTimeParameterManager. GetValueByName ['Verifier'])= '1' then
    GetSolverEngine.Verify (Spec)
  else
}
  GetSolverEngine.Solve (Spec);

  Spec.Free;
  Finalize;
  WriteLn ('</Main>');

end.
{
  TODO: If there are two sets of variable which are independent ....

}
