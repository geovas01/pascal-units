unit Wrapper4GLPK;
{$Mode objfpc}

interface
uses
  Classes, AbstractLPSolver, CTypes, SysUtils;

type

  TGLPK= class (TAbstractLPSolver)
  private
    StringToPCChar: TStringList;
    IsMaximizationProblem: Boolean;
    FProblemName: String;
    Variables: TVariablesInfo;
    Constraints: TConstraints;
    FObjectiveFunction: TLinearFunction;

    function ConvertStringToPCChar (const S: String): PCChar;

  public

    constructor CreateMaximizationProblem (const ProbName: String);
    constructor CreateMinimizationProblem (const ProbName: String);

    procedure AddNewIntegerVariable (const VariableName: String);
    procedure SetABoundForAVariable (const VarName: String; UpperBound: Boolean; Value: Integer);
    procedure ReleaseABoundFromAVariable (const VarName: String; UpperBound: Boolean);

    function InitializeConstraint: TConstraint;
    function ToXML: AnsiString;

    procedure SolveByGLPK;

  end;

implementation
uses
  Interfaces;

{ TGPLK }

procedure TGLPK.AddNewIntegerVariable (const VariableName: String);
var
  NewVar: TBaseVariableInfo;

begin
  if 0<= Variables.IndexOf (UpperCase (VariableName)) then
    raise EVarAlreadyExists.Create (VariableName);

  NewVar:= TIntVariableInfo.Create (VariableName);
  Variables.AddObject (UpperCase (VariableName), NewVar);

end;

constructor TGLPK.CreateMaximizationProblem (const ProbName: String);
begin
  inherited;

end;

constructor TGLPK.CreateMinimizationProblem (const ProbName: String);
begin
  inherited;

end;

function TGLPK.ConvertStringToPCChar (const S: String): PCChar;
var
  Index: Integer;
  i: Integer;

begin
  Index:= StringToPCChar.IndexOf (S);

  if 0<= Index then
    Result:= PCChar (StringToPCChar.Objects [Index])
  else
  begin
    GetMem (Result, Length (S)+ 1);
    for i:= 1 to Length (S) do
      (Result+ i- 1)^:= Ord (S [i]);
    (Result+ Length (S))^:= 0;

    StringToPCChar.AddObject (S, TObject (Result));
    StringToPCChar.Sort;

  end;

end;

function TGLPK.InitializeConstraint: TConstraint;
begin
  Result:= TConstraint.Create (Variables);
  
  Constraints.Add (Result);
 
end;

procedure TGLPK.SetABoundForAVariable (const VarName: String; UpperBound: Boolean; Value: Integer);
var
  VarInfo: TBaseVariableInfo;

begin
  VarInfo:= Variables.VariableByName [VarName];

  if UpperBound then
    VarInfo.SetUpperBound (Value)
  else
    VarInfo.SetLowerBound (Value);

end;

procedure TGLPK.SolveByGLPK;

  procedure ExportVariables (ProblemObj: TGLPKProblemPointer; Variables: TVariablesInfo);
  var
    i: Integer;
    ActiveVariable: TBaseVariableInfo;

  begin
    for i:= 0 to Variables.Count- 1 do
    begin
      ActiveVariable:= Variables.VariableByIndex [i];

      case ActiveVariable.VariableType of
        vtInteger:
          GLPKAddNewIntegerVariable (
                           ConvertStringToPCChar (Variables.VariableByIndex [i].Name),
                           Ord (ActiveVariable.BoundStatus),
                           ActiveVariable.LowerBound,
                           ActiveVariable.UpperBound,
                           ObjectiveFunction.FindCoef (ActiveVariable), ProblemObj);

        vtContinues:
          GLPKAddNewContinuesVariable (ConvertStringToPCChar (Variables.VariableByIndex [i].Name), Ord (ActiveVariable.BoundStatus), ActiveVariable.LowerBound, ActiveVariable.UpperBound, ObjectiveFunction.FindCoef (ActiveVariable), ProblemObj)

        else
          raise Exception.Create ('Invalid VariableType!');

      end;

    end;

   end;

  procedure ExportConstraints (ProblemObj: TGLPKProblemPointer; Constraints: TConstraints; Variables: TVariablesInfo);
  var
    c, v: Integer;
    ActiveConstraint: TConstraint;
    ActiveVariable: TBaseVariableInfo;
    IA, IB, IC: PCInt32;
    IAPtr, IBPtr, ICPtr: PCInt32;
    Size, MaxIndex: Integer;
    Coef: CInt16;

  begin
    Size:= Variables.Count* Constraints.Count+ 1;
    GetMem (IA, 2* Size);
    GetMem (IB, 2* Size);
    GetMem (IC, 2* Size);

    for c:= 0 to Constraints.Count- 1 do
    begin
      ActiveConstraint:= Constraints.Constraint [c];

      GLPKAddNewConstraint (ConvertStringToPCChar ('Row'+ IntToStr (c+ 1)),
                            Ord (ActiveConstraint.BoundStatus),
                            ActiveConstraint.LowerBound,
                            ActiveConstraint.UpperBound,
                            ProblemObj);
    
    end;

    IAPtr:= IA; IBPtr:= IB; ICPtr:= IC;
    IAPtr^:= 0; IBPtr^:= 0; ICPtr^:= 0;
    Inc (IAPtr); Inc (IBPtr); Inc (ICPtr);
    MaxIndex:= 1;
    for c:= 0 to Constraints.Count- 1 do
    begin
      ActiveConstraint:= Constraints.Constraint [c];
      for v:= 0 to Variables.Count- 1 do
      begin
        ActiveVariable:= Variables.VariableByIndex [v];

        Coef:= ActiveConstraint.FindCoef (ActiveVariable);
        if Coef<> 0 then
        begin
          IAPtr^:= c+ 1;
          IBPtr^:= v+ 1;
          ICPtr^:= Coef;
       
          Inc (IAPtr, 1); Inc (IBPtr, 1); Inc (ICPtr, 1);
          Inc (MaxIndex);

        end;
          
      end;

    end;

    GLPKLoadMatrix (IA, IB, IC, MaxIndex- 1, ProblemObj);


  end;

  procedure RunSimplex (ProblemObj: TGLPKProblemPointer);
  begin
    GLPKSimplex (ProblemObj);

  end;

  procedure RunIntOptimization (ProblemObj: TGLPKProblemPointer);
  begin
    WriteLn ('In RunIntOptimization');
    GLPKIntOptimization (ProblemObj);

  end;

  procedure ImportVariables (ProblemObj: TGLPKProblemPointer; Variables: TVariablesInfo);
  var
    i: Integer;
    ActiveVariable: TBaseVariableInfo;
    PValue: double;

  begin
    if HasIntegerVariable then
    begin
      for i:= 0 to Variables.Count- 1 do
      begin
        ActiveVariable:= Variables.VariableByIndex [i];

        PValue:= GLPKGetMIPVariablePrimeValue (ProblemObj, i+ 1);
        ActiveVariable.Value:= PValue;

      end;
 

    end
    else
    begin
      for i:= 0 to Variables.Count- 1 do
      begin
        ActiveVariable:= Variables.VariableByIndex [i];

        PValue:= GLPKGetLPVariablePrimeValue (ProblemObj, i+ 1);
        ActiveVariable.Value:= PValue;

      end;

    end;

   end;

var
  ProblemObj: TGLPKProblemPointer;

begin
  StringToPCChar:= TStringList.Create;
  if IsMaximizationProblem then
    ProblemObj:= GLPKCreateMaximizationProblem (ConvertStringToPCChar (ProblemName))
  else 
    ProblemObj:= GLPKCreateMinimizationProblem (ConvertStringToPCChar (ProblemName));

  ExportVariables (ProblemObj, Variables);
  ExportConstraints (ProblemObj, Constraints, Variables);

  if HasIntegerVariable then
    RunIntOptimization (ProblemObj)
  else
    RunSimplex (ProblemObj);

  ImportVariables (ProblemObj, Variables);

end;

procedure TGLPK.ReleaseABoundFromAVariable (const VarName: String; UpperBound: Boolean);
var
  VarInfo: TBaseVariableInfo;

begin
  VarInfo:= Variables.VariableByName [VarName];

  if UpperBound then
    VarInfo.ReleaseUpperBound
  else
    VarInfo.ReleaseLowerBound;

end;

function TGLPK.ToXML: AnsiString;
begin
  if IsMaximizationProblem then
    Result:= '<MaxProblem '
  else
    Result:= '<MinProblem ';

  Result+= 'Name= "'+ FProblemName+ '" >';
  Result+= Variables.ToXML;
  Result+= ObjectiveFunction.ToXML;
  Result+= Constraints.ToXML;

  if IsMaximizationProblem then
    Result+= '</MaxProblem> '
  else
    Result+= '</MinProblem>';

end;

initialization

end.
