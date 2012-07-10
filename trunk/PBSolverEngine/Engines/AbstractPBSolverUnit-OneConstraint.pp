unit AbstractPBSolverUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SatSolverInterfaceUnit, TSeitinVariableUnit,
             ProblemDescriptionUnit, ClauseUnit, PBConstraintUnit, MyTypes;

type
  TPBSolverMode= (pbsMine);

  { TAbstractPBSolverEngine }

  TAbstractPBSolverEngine= class (TObject)
  private
    FLastModel: TIntegerCollection;
    LastAnswer: AnsiString;

    function GetCNFGenerator: TSATSolverInterface; inline;
    function GetVariableGenerator: TVariableManager; inline;

  protected
    property CNFGenerator: TSATSolverInterface read GetCNFGenerator;
    property VariableGenerator: TVariableManager read GetVariableGenerator;
    property LastModel: TIntegerCollection read FLastModel;

    function EncodeHardConstraint (AConstraint: TPBConstraint): TLiteral; virtual; abstract;
    function VerifyHardConstraint (AConstraint: TPBConstraint;
                                Assignment: TAssignments): Boolean; virtual;
    function EncodeEqualityConstraint (AConstraint: TPBConstraint): TLiteral; virtual; abstract;
    function EncodeGreaterThanOrEqualConstraint (AConstraint: TPBConstraint): TLiteral; virtual; abstract;

    function EncodeLessThanOrEqualConstraint (AConstraint: TPBConstraint): TLiteral; virtual; abstract;

    function BreakSymmetries (Problem: TPBSpecification): Boolean; virtual;

    procedure Report (S: AnsiString);
    procedure ReportLn (S: AnsiString= '');

    function SolveOptimizationProblem (Problem: TPBSpecification; PrintResult: Boolean): Int64;
    function SolveDecisionProblem (Problem: TPBSpecification; PrintResult: Boolean; BreakSymmetry: Boolean): Boolean;
    function VerifyDecisionProblem (Problem: TPBSpecification; Assignment: TAssignments): Boolean;

    function SimplifyEqualityConstraint (AConstraint: TPBConstraint): TPBConstraint; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    function Solve (Problem: TPBSpecification): Boolean; virtual;
    function Verify (Problem: TPBSpecification;
                     Assignments: TAssignments): Boolean; virtual;
    procedure ProcessSigTerm;

  end;

procedure Initialize;
procedure Finalize;
function GetSolverEngine: TAbstractPBSolverEngine; inline;

implementation
uses
  Math, ParameterManagerUnit,
  BigInt, MyPBSolverEngineUsingPrimesUnit, MyPBSolverEngineUsingLargeModuloUnit;

var
  PBSolver: TAbstractPBSolverEngine;

function GetSolverEngine: TAbstractPBSolverEngine; inline;
begin
  Exit (PBSolver);

end;

{ TAbstractPBSolverEngine }

function TAbstractPBSolverEngine.SimplifyEqualityConstraint (AConstraint: TPBConstraint): TPBConstraint;

  function DoSimplification (AConstraint: TPBConstraint): TPBConstraint;
  var
    NewLHS: TPBSum;
    NewRHS: TBigInt;
    Changed: Boolean;
    TrueIntegers, UnknownIntegers: TBigInt;
    i: Integer;
    TermExistsInSimplifiedCons: array of Boolean;
    Done: Boolean;
  
  begin
    if AConstraint.LHS.Count= 0 then
      Exit (AConstraint);

    SetLength (TermExistsInSimplifiedCons, AConstraint.LHS.Count);
    for i:= 0 to AConstraint.LHS.Count- 1 do
      TermExistsInSimplifiedCons [i]:= True;
  
    TrueIntegers:= BigIntFactory.GetNewMemeber.SetValue (0);
    UnknownIntegers:= BigIntFactory.GetNewMemeber.SetValue (0);
    Changed:= False;
    Done:= False;
  
    for i:= 0 to AConstraint.LHS.Count- 1 do
      if AConstraint.RHS.CompareWith (AConstraint.LHS.Item [i].Coef)< 0 then
      begin
        case CNFGenerator.GetLiteralValue (AConstraint.LHS.Item [i].Literal) of
          gbUnknown:
          begin
            CNFGenerator.BeginConstraint;
            CNFGenerator.AddLiteral (NegateLiteral (AConstraint.LHS.Item [i].Literal));
            CNFGenerator.SubmitClause;
            Changed:= True;
            TermExistsInSimplifiedCons [i]:= False;
  
          end;
          gbFalse:
            TermExistsInSimplifiedCons [i]:= False;
  
          gbTrue:
          begin
            Done:= True;
            Break;
  
          end;
  
        end;
  
      end
      else
      begin
        case CNFGenerator.GetLiteralValue (AConstraint.LHS.Item [i].Literal) of
          gbUnknown:
          begin
            TermExistsInSimplifiedCons [i]:= True;
            UnknownIntegers.Add (AConstraint.LHS.Item [i].Coef);
  
          end;
          gbTrue:
          begin
            TrueIntegers.Add (AConstraint.LHS.Item [i].Coef);
            TermExistsInSimplifiedCons [i]:= False;
            Changed:= True;
  
          end;
          gbFalse:
          begin
            TermExistsInSimplifiedCons [i]:= False;
            Changed:= True;
  
          end;
  
        end;
  
      end;
  
    if (GetRunTimeParameterManager.Verbosity and Ord (vbFull))<> 0 then
    begin
      WriteLn ('TrueIntegers=', TrueIntegers.ToString);
      WriteLn ('UnknownInteger=', UnknownIntegers.ToString);
  
      for i:= 0 to AConstraint.LHS.Count- 1 do
        if TermExistsInSimplifiedCons [i] then
          Write ('(', LiteralToString (AConstraint.LHS.Item [i].Literal), ':', 1, ')')
        else
          Write ('(', LiteralToString (AConstraint.LHS.Item [i].Literal), ':', 0, ')');
      WriteLn;
    end;
  
    if TrueIntegers.CompareWith (AConstraint.RHS)> 0 then
      Done:= True;
    if UnknownIntegers.Add (TrueIntegers).CompareWith (AConstraint.RHS)< 0 then
      Done:= True;
//UnknownIntegers has been modified !!;

    if Done then
    begin
      SetLength (TermExistsInSimplifiedCons, 0);
      UnknownIntegers.Free;
      TrueIntegers.Free;

      Exit (nil);
  
    end;
  
    if Changed then
    begin
      NewRHS:= AConstraint.RHS.Copy.Sub (TrueIntegers);
      NewLHS:= TPBSum.Create;
      for i:= 0 to AConstraint.LHS.Count- 1 do
        if TermExistsInSimplifiedCons [i] then
          NewLHS.AddNewTerm (AConstraint.LHS.Item [i].Copy);
      Result:= TPBConstraint.Create (NewLHS, '=', True, NewRHS);
      if (GetRunTimeParameterManager.Verbosity and Ord (vbFull))<> 0 then
        WriteLn ('New Constraint is: ', Result.ToString);

    end
    else
    begin
      SetLength (TermExistsInSimplifiedCons, 0);
      UnknownIntegers.Free;
      TrueIntegers.Free;

      Exit (AConstraint);

    end;
  
    SetLength (TermExistsInSimplifiedCons, 0);
    UnknownIntegers.Free;
    TrueIntegers.Free;
  
  end;

var
  TempConstraint: TPBConstraint;

begin
  TempConstraint:= AConstraint.Copy;

  Result:= DoSimplification (TempConstraint);

  while (Result<> TempConstraint) and (Result<> nil) do
  begin

    if GetRunTimeParameterManager.Verbosity and Ord (vbFull)<> 0 then
      WriteLn (TempConstraint.ToString, '<=>', Result.ToString);
    TempConstraint.Free;

    TempConstraint:= Result;
    Result:= DoSimplification (TempConstraint);

  end;

end;

procedure Initialize;
begin
  if UpperCase (GetRunTimeParameterManager.ValueByName ['--ModuloMode'])= UpperCase ('Prime')  then
    PBSolver:= TMyPBSolverEngineUsingPrimeModulos.Create
  else if UpperCase (GetRunTimeParameterManager.ValueByName ['--ModuloMode'])= UpperCase ('MinimalPrime')  then
    PBSolver:= TMyPBSolverEngineUsingMinimalPrimeModulos.Create
  else if UpperCase (GetRunTimeParameterManager.ValueByName ['--ModuloMode'])= UpperCase ('PrimePower')  then
    PBSolver:= TMyPBSolverEngineUsingPrimePowerModulos.Create
  else if UpperCase (GetRunTimeParameterManager.ValueByName ['--ModuloMode'])= UpperCase ('LargeModulo')  then
    PBSolver:= TMyPBSolverEngineUsingLargeModulos.Create
  else
  begin
    WriteLn ('Invalid ModuloMode: "', GetRunTimeParameterManager.ValueByName ['--ModuloMode'], '"');
    WriteLn ('ModuloMode can be "Prime", "MinimalPrime" or "PrimePower"');
    Halt (1);

  end;

end;

{ TAbstractPBSolverEngine }

function TAbstractPBSolverEngine.GetCNFGenerator: TSATSolverInterface;
begin
  Result:= SatSolverInterfaceUnit.GetSatSolver;

end;

function TAbstractPBSolverEngine.GetVariableGenerator: TVariableManager;
begin
  Result:= TSeitinVariableUnit.GetVariableManager;

end;

function TAbstractPBSolverEngine.VerifyHardConstraint (AConstraint: TPBConstraint;
                   Assignment: TAssignments): Boolean;
var
  LHSValue: TBigInt;
  i: Integer;
  CompareResult: Integer;

begin
  LHSValue:= BigIntFactory.GetNewMemeber.SetValue (0);

  for i:= 0 to AConstraint.LHS.Count- 1 do
  begin
    if Assignment.GetValue (AConstraint.LHS.Item [i].Literal)= gbTrue then
      LHSValue.Add (AConstraint.LHS.Item [i].Coef);

  end;

  CompareResult:= LHSValue.CompareWith (AConstraint.RHS);

  case AConstraint.CompareOperator of
    coEquality:
      Result:= (CompareResult= 0);
    coLessThanOrEqual:
      Result:= (CompareResult<= 0);
    coGreaterThanOrEqual:
      Result:= (0<= CompareResult);
  end;
end;

function TAbstractPBSolverEngine.BreakSymmetries (Problem: TPBSpecification): Boolean;
begin
  Result:= False;

end;

(*
function TAbstractPBSolverEngine.BreakTheSymmetries (Problem: TPBSpecification): Boolean;
var
  i, j: Integer;
  VarsInClauses: TList;
  {VarsInClauses [i]==> All the clauses in which the i-th variable has been occured}
  VarsInConstraints: TList;
  {VarsInConstraint [i]==> All the constraints in which the i-th variable has been occured}
  VarSumOfCoefs, NegVArSumOfCoefs: TList;
  {SumOfPos{Neg}Coefs [i]==> Sum of all positive {Negative} coefficient of the i-th variable in all clauses and constraints}

  function AreSame (v1, v2: Integer): Boolean;

    function ConstraintsAreTheSame (ActiveConstraint1, ActiveConstraint2: TPBConstraint; v1, v2: Integer): Boolean;
    var
      i, j: Integer;
      Term1, Term2: TTerm;
      Matched: Boolean;

    begin
      if ActiveConstraint1.CompareOperator<> ActiveConstraint2.CompareOperator then
        Exit (False);

      if ActiveConstraint1.RHS.CompareWith (ActiveConstraint2.RHS)<> 0 then
        Exit (False);

      Result:= False;
      for i:= 0 to ActiveConstraint1.LHS.Count- 1 do
      begin
        Term1:= ActiveConstraint1.LHS.Item [i];
        Matched:= False;

        for j:= 0 to ActiveConstraint2.LHS.Count- 1 do
        begin
          Term2:= ActiveConstraint2.LHS.Item [j];

          if (Term1.Literal.FRawValue= Term2.Literal.FRawValue) and (GetVar (Term1.Literal)<> v1) and (GetVar (Term1.Literal)<> v2)
             {(GetVar (Term2.Literal)<> v1) and (GetVar (Term2.Literal)<> v2) } then
            if Term1.Coef.CompareWith (Term2.Coef)= 0 then
            begin
              Matched:= True;
              Break;

            end;

          if (GetVar (Term1.Literal)= v1) and (GetVar (Term2.Literal)= v2) and not (IsNegated (Term1.Literal) xor IsNegated (Term2.Literal)) then
            if (Term1.Coef.CompareWith (Term2.Coef)= 0) then
            begin
              Matched:= True;
              Break;

            end;

          if (GetVar (Term1.Literal)= v2) and (GetVar (Term2.Literal)= v1) and not (IsNegated (Term1.Literal) xor IsNegated (Term2.Literal)) then
            if (Term1.Coef.CompareWith (Term2.Coef)= 0) then
            begin
              Matched:= True;
              Break;

            end;

        end;

        if not Matched then
          Exit;

      end;

      Result:= True;

    end;

    function ClausessAreTheSame (ActiveClause1, ActiveClause2: TClause; v1, v2: Integer): Boolean;
    var
      i, j: Integer;
      Matched: Boolean;

    begin
      if ActiveClause1.Count<> ActiveClause2.Count then
        Exit (False);

      Result:= False;
      for i:= 0 to ActiveClause1.Count- 1 do
      begin
        Matched:= False;

        for j:= 0 to ActiveClause2.Count- 1 do
        begin

          if (ActiveClause1.Item [i].FRawValue= ActiveClause2.Item [j].FRawValue) and (GetVar (ActiveClause1.Item [i])<> v1) and (GetVar (ActiveClause1.Item [i])<> v2) then
            begin
              Matched:= True;
              Break;

            end;

          if (GetVar (ActiveClause1.Item [i])= v1) and (GetVar (ActiveClause2.Item [j])= v2) and not (IsNegated (ActiveClause1.Item [i]) xor IsNegated (ActiveClause2.Item [j])) then
          begin
            Matched:= True;
            Break;

          end;

          if (GetVar (ActiveClause1.Item [i])= v2) and (GetVar (ActiveClause2.Item [j])= v1) and not (IsNegated (ActiveClause1.Item [i]) xor IsNegated (ActiveClause2.Item [j])) then
          begin
            Matched:= True;
            Break;

          end;

        end;

        if not Matched then
          Exit;

      end;

      Result:= True;

    end;


  var
    i, j: Integer;
    ActiveConstraint1, ActiveConstraint2: TPBConstraint;
    ActiveCluase1, ActiveCluase2: TClause;
    Matched: Boolean;

  begin
    Result:= False;

    if not (
       (TClauseCollection (VarsInClauses [v1]).Count= TClauseCollection (VarsInClauses [v2]).Count) and
       (TConstraintCollection (VarsInConstraints [v1]).Count= TConstraintCollection (VarsInConstraints [v2]).Count) and
       (TBigInt (VarSumOfCoefs [v1]).CompareWith (TBigInt (VarSumOfCoefs [v2]))= 0) and
       (TBigInt (NegVArSumOfCoefs [v1]).CompareWith (TBigInt (NegVArSumOfCoefs [v2]))= 0)
        ) then
      Exit;

    for i:= 0 to TConstraintCollection (VarsInConstraints [v1]).Count- 1 do
    begin
      Matched:= False;
      ActiveConstraint1:= TConstraintCollection (VarsInConstraints [v1]).Item [i];

      for j:= 0 to TConstraintCollection (VarsInConstraints [v2]).Count- 1 do
      begin
        ActiveConstraint2:= TConstraintCollection (VarsInConstraints [v2]).Item [j];

        if ConstraintsAreTheSame (ActiveConstraint1, ActiveConstraint2, Problem.InputVariable [v1], Problem.InputVariable [v2]) then
        begin
          Matched:= True;
          Break;

        end;

      end;

      if not Matched then
        Exit;

    end;

    for i:= 0 to TConstraintCollection (VarsInConstraints [v2]).Count- 1 do
    begin
      Matched:= False;
      ActiveConstraint1:= TConstraintCollection (VarsInConstraints [v2]).Item [i];

      for j:= 0 to TConstraintCollection (VarsInConstraints [v1]).Count- 1 do
      begin
        ActiveConstraint2:= TConstraintCollection (VarsInConstraints [v1]).Item [j];

        if ConstraintsAreTheSame (ActiveConstraint1, ActiveConstraint2, Problem.InputVariable [v1], Problem.InputVariable [v2]) then
        begin
          Matched:= True;
          Break;

        end;

      end;

      if not Matched then
        Exit;

    end;

    for i:= 0 to TClauseCollection (VarsInClauses [v1]).Count- 1 do
    begin
      Matched:= False;
      ActiveCluase1:= TClauseCollection (VarsInClauses [v1]).Item [i];

      for j:= 0 to TClauseCollection (VarsInClauses [v2]).Count- 1 do
      begin
        ActiveCluase2:= TClauseCollection (VarsInClauses [v2]).Item [j];

        if ClausessAreTheSame (ActiveCluase1, ActiveCluase2, Problem.InputVariable [v1], Problem.InputVariable [v2]) then
        begin
          Matched:= True;
          Break;

        end;

      end;

      if not Matched then
        Exit;

    end;

    for i:= 0 to TClauseCollection (VarsInClauses [v2]).Count- 1 do
    begin
      Matched:= False;
      ActiveCluase1:= TClauseCollection (VarsInClauses [v2]).Item [i];

      for j:= 0 to TClauseCollection (VarsInClauses [v1]).Count- 1 do
      begin
        ActiveCluase2:= TClauseCollection (VarsInClauses [v1]).Item [j];

        if ClausessAreTheSame (ActiveCluase1, ActiveCluase2, Problem.InputVariable [v1], Problem.InputVariable [v2]) then
        begin
          Matched:= True;
          Break;

        end;

      end;

      if not Matched then
        Exit;

    end;

   Result:= True;

  end;

var
  v1, v2: Integer;
  AClause: TClause;

begin
  VarsInClauses:= TList.Create;
  VarsInConstraints:= TList.Create;
  VarSumOfCoefs:= TList.Create;
  NegVArSumOfCoefs:= TList.Create;

  for i:= 0 to Problem.InputVariableCount do
  begin
    VarsInClauses.Add (TClauseCollection.Create);
    VarsInConstraints.Add (TConstraintCollection.Create);
    VarSumOfCoefs.Add (TBigInt.Create.SetValue (0));
    NegVArSumOfCoefs.Add (TBigInt.Create.SetValue (0));

  end;

  for i:= 0 to Problem.HardConstraintCount- 1 do
    Problem.HardConstraint [i].Finalize;


  for i:= 1 to Problem.InputVariableCount do
    for j:= 0 to Problem.HardConstraintCount- 1 do
      if Problem.HardConstraint [j].LHS.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], False))<> nil then
      begin
        TConstraintCollection (VarsInConstraints [i]).AddItem (Problem.HardConstraint [j]);
        TBigInt (VarSumOfCoefs [i]).Add (Problem.HardConstraint [j].LHS.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], False)));

      end;

  for i:= 1 to Problem.InputVariableCount do
    for j:= 0 to Problem.HardConstraintCount- 1 do
      if Problem.HardConstraint [j].LHS.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], True))<> nil then
      begin
        TConstraintCollection (VarsInConstraints [i]).AddItem (Problem.HardConstraint [j]);
        TBigInt (NegVArSumOfCoefs [i]).Add (Problem.HardConstraint [j].LHS.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], True)));

      end;

  for i:= 1 to Problem.InputVariableCount do
    for j:= 0 to Problem.CNFClauses.Count- 1 do
      if Problem.CNFClauses.Item [j].IsExist (CreateLiteral (Problem.InputVariable [i], False))
      or Problem.CNFClauses.Item [j].IsExist (CreateLiteral (Problem.InputVariable [i], True)) then
        TClauseCollection (VarsInClauses [i]).AddItem (Problem.CNFClauses.Item [j]);

      AreSame (1, 5);

  for i:= 1 to Problem.InputVariableCount do
    for j:= i+ 1 to Problem.InputVariableCount do
      if AreSame (i, j) then
      begin
        if (Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], False))<> nil) xor
           (Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [j], False))<> nil)
        then
          Continue;
        if (Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], True))<> nil) xor
           (Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [j], True))<> nil)
        then
          Continue;

        if Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], False))<> nil then
          if Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], False)).CompareWith
                  (Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [j], False)))<> 0 then
          Continue;

        if Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], True))<> nil then
          if Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [i], True)).CompareWith
                  (Problem.ObjectiveFunction.GetCoefByLiteral (CreateLiteral (Problem.InputVariable [j], True)))<> 0 then
          Continue;

        AClause:= TClause.Create;
        AClause.AddItem (CreateLiteral (Problem.InputVariable [i], True));
        AClause.AddItem (CreateLiteral (Problem.InputVariable [j], False));
        Problem.CNFClauses.Add (AClause);

      end;

  for i:= 0 to Problem.InputVariableCount do
  begin
    TClauseCollection (VarsInClauses [i]).Clear;
    TConstraintCollection (VarsInConstraints [i]).Clear;

    TClauseCollection (VarsInClauses [i]).Free;
    TConstraintCollection (VarsInConstraints [i]).Free;

    TBigInt (VarSumOfCoefs [i]).Free;
    TBigInt (NegVArSumOfCoefs [i]).Free;

  end;

end;
*)

procedure TAbstractPBSolverEngine.Report (S: AnsiString);
begin
  System.Write (S);

end;

procedure TAbstractPBSolverEngine.ReportLn (S: AnsiString);
begin
  System.WriteLn (S);

end;

constructor TAbstractPBSolverEngine.Create;
begin
  inherited Create;

  FLastModel:= nil;
  LastAnswer:= '';

end;

destructor TAbstractPBSolverEngine.Destroy;
begin
  FLastModel.Free;

  inherited Destroy;

end;

function PrintSubtract (a, b: Int64): AnsiString;
begin
  Result:= IntToStr (a- b);

end;

function TAbstractPBSolverEngine.SolveOptimizationProblem (Problem: TPBSpecification; PrintResult: Boolean): Int64;
{
var
  NewProblemDesc: TPBSpecification;
  i, j: Integer;
  ActiveConstraint: TPBConstraint;
  MainObjectiveFunction: TPBSum;
  High, Low, Mid, Temp: Int64;
  LastAnswerSign: Boolean;
  DecisionProblemResult: Boolean;
  NewObjectiveFunction: TPBSum;
  ObjectiveFunctionBias: Int64;
  ConstantTerm: Int64;

//  CNFCollection: TCNFCollection;
  DecisionProblemDescription: TClauseCollection;
  VarsInProblem: Integer;

  ActiveClause: TClause;
}
begin
  if PrintResult or (Problem<> nil) then
   ;
  Result:= 0;
  (*
  MainObjectiveFunction:= Problem.ObjectiveFunction;
  Problem.ObjectiveFunction:= nil;

  MainObjectiveFunction.Finalize;

  ObjectiveFunctionBias:= MainObjectiveFunction.ConstantTerm;

  NewObjectiveFunction:= MainObjectiveFunction.Copy;
  NewObjectiveFunction.Finalize;

  MainObjectiveFunction.Free;

//  ReNewSatSolver (ssCNFCollection);
  DecisionProblemResult:= SolveDecisionProblem (Problem, False, False);

  if not DecisionProblemResult then
  begin
    if PrintResult then
      ReportLn ('s UNSATISFIABLE');
    Exit (0);

  end;

  raise Exception.Create ('TAbstractPBSolverEngine.SolveOptimizationProblem');
  assert (false);
//??  CNFCollection:= CNFGenerator as TCNFCollection;
//??  DecisionProblemDescription:= CNFGenerator.AllClauses.Copy;

//    CNFCollection.ToString.SaveToFile ('CNF.txt');

  VarsInProblem:= Math.Max (DecisionProblemDescription.MaxVar, Problem.InputVariableCount+ 1);
  VarsInProblem:= Math.Max (VarsInProblem, VariableGenerator.LastUsedCNFIndex);

  FLastModel:= TIntegerCollection.Create;
  CNFGenerator.ImportModel (Problem.InputVariable [Problem.InputVariableCount], LastModel);

  LastAnswerSign:= True;
  Result:= NewObjectiveFunction.Evaluate (CNFGenerator, LastAnswerSign);
  Assert (LastAnswerSign);

  if PrintResult then
  begin
    LastAnswer:= PrintSubtract (Result, ObjectiveFunctionBias);
    ReportLn ('o '+ LastAnswer);

  end;

  NewProblemDesc:= TPBSpecification.Create (Problem.InputVariables);
  NewProblemDesc.AddNewConstraint (nil);

  High:= Result;
  Dec (High);
  Low:= 0;
  Mid:= 0;
  ActiveConstraint:= nil;

  while (Low<= High) and (High<> 0) do
  begin
//      WriteLn ('L:', Low.ToString, ' H:', High.ToString);

    Temp:= High+ Low;
    Mid:= Temp div 2;
//    Temp.Free;
//      WriteLn ('Mid=', Mid.ToString);

    Temp:= Mid;
    ActiveConstraint.Free;
    ActiveConstraint:= TPBConstraint.Create (NewObjectiveFunction.Copy, '<=', True, Mid);
    NewProblemDesc.SetConstraint (0, ActiveConstraint);

    Mid:= Temp;

    ReNewSatSolver;
    GetVariableGenerator.SetLastVariableIndex (VarsInProblem);
{
    for i:= 0 to DecisionProblemDescription.Count- 1 do
      CNFCollection.AddClause (DecisionProblemDescription.Item [i]);
}
    DecisionProblemResult:= SolveDecisionProblem (NewProblemDesc, False, False);

//      WriteLn (DecisionProblemResult);
//      ReadLn;

    if DecisionProblemResult then
    begin
      CNFGenerator.ImportModel (Problem.InputVariable [Problem.InputVariableCount], LastModel);
//      High.Free;
//      Result.Free;
      Result:= NewObjectiveFunction.Evaluate (CNFGenerator, LastAnswerSign);
      if PrintResult then
      begin
        LastAnswer:= PrintSubtract (Result, ObjectiveFunctionBias);
        ReportLn ('o '+ LastAnswer);

      end;

      Assert (LastAnswerSign);

      High:= Result;
      if High= 0 then
        Break;

      Dec (High);

    end
    else
    begin
      Low:= Mid- 1;

    end;

  end;

  if PrintResult then
  begin
    ReportLn ('o '+ PrintSubtract (Result, ObjectiveFunctionBias));
    ReportLn ('s OPTIMUM FOUND');
    Report ('v ');
    for i:= 1 to Problem.InputVariableCount do
    begin
      if LastModel.Item [Problem.InputVariable [i]]= 1 then
         Report ('x'+ IntToStr (Problem.InputVariable [i]- 1)+ ' ')
      else
         Report ('-x'+ IntToStr (Problem.InputVariable [i]- 1)+ ' ');
    end;
    ReportLn ();

  end;

  FLastModel.Free;
  FLastModel:= nil;
  NewObjectiveFunction.Free;
  NewProblemDesc.Free;

  DecisionProblemDescription.Free;
*)
end;

function TAbstractPBSolverEngine.SolveDecisionProblem (Problem: TPBSpecification; PrintResult: Boolean; BreakSymmetry: Boolean): Boolean;

  function GenerateNewProbelm (Problem: TPBSpecification): TPBConstraint;
  {generates a new problem equisatisfiable with Problem which has just a singme constraint.}
  var
    i, j: Integer;
    MultiplicationFactor, Temp: TBigInt;
    ActiveConstraintSum: TBigInt;
    ActiveConstraint: TPBConstraint;
    LiteralsCoef: array of TBigInt;

  begin
    SetLength (LiteralsCoef, 2* (Problem.InputVariableCount+ 1));// Each variable can be either positive or negative
    for i:= 0 to 2* (Problem.InputVariableCount+ 1) do
      LiteralsCoef [i]:= TBigInt.Create.SetValue (0);

    MultiplicationFactor:= TBigInt.Create.SetValue (1);

    for i:= 0 to Problem.ConstraintCount- 1 do
    begin
      ActiveConstraint:= Problem.Constraint [i];

      for j:= 0 to ActiveConstraint.LHS.Count- 1 do
      begin
        Temp:= ActiveConstraint.LHS.Item [j].Coef.Mul (MultiplicationFactor);

        LiteralsCoef [ActiveConstraint.LHS.Item [j].Literal].Add (Temp);

        Temp.Free;

      end;

      ActiveConstraintSum:= ActiveConstraint.LHS.SumOfCoefs;
      ActiveConstraintSum.Incr;

      Temp:= MultiplicationFactor.Mul (ActiveConstraintSum);
      MultiplicationFactor.Free;
      MultiplicationFactor:= Temp;

    end;

    //To be continued!:P
  end;

var
  i: Integer;
  ActiveConstraint: TPBConstraint;
  Lit: TLiteral;

begin
  if GetRunTimeParameterManager.Verbosity and Ord (vbFull)<> 0 then
    GetCNFGenerator.ReportForcedVariables;

  if BreakSymmetry then
    BreakSymmetries (Problem);

  Problem.DescribeNonLinearVariables;
  Result:= True;

  if UpperCase (GetRunTimeParameterManager.ValueByName ['--EncodeAsOneConstraint'])= UpperCase ('Enabled') then
  begin
    ActiveConstraint:= GenerateNewProbelm (Problem);
    Lit:= EncodeHardConstraint (ActiveConstraint);
    ActiveConstraint.Free;

    if Lit= VariableGenerator.FalseLiteral then
      Exit (False)
    else if Lit= VariableGenerator.TrueLiteral then
      Exit (True)
    else
    begin
      CNFGenerator.BeginConstraint;
      CNFGenerator.AddLiteral (Lit);
      CNFGenerator.SubmitClause;

    end;

  end
  else if UpperCase (GetRunTimeParameterManager.ValueByName ['--EncodeAsOneConstraint'])= UpperCase ('Disabled') then
  begin
    for i:= 0 to Problem.ConstraintCount- 1 do
    begin
      ActiveConstraint:= Problem.Constraint [i];

      Lit:= EncodeHardConstraint (ActiveConstraint);

      if GetRunTimeParameterManager.Verbosity and Ord (vbEveryThing)<> 0 then
        if i mod 1000= 0 then
          WriteLn ('c ', i, ' ', LiteralToString (Lit));

      if Lit= VariableGenerator.FalseLiteral then
      begin
        Result:= False;
        Break;

      end
      else if Lit= VariableGenerator.TrueLiteral then
       //Do nothing
      else
      begin
        if not VariableGenerator.SimulationMode then
        begin
          CNFGenerator.BeginConstraint;
          CNFGenerator.AddLiteral (Lit);
          CNFGenerator.SubmitClause;

        end;

        if GetRunTimeParameterManager.Verbosity and Ord (vbFull)<> 0 then
          CNFGenerator.ReportForcedVariables;

      end;

    end;

  end;

  if Result then
  begin
  //To handle empty problems
    CNFGenerator.BeginConstraint;
    CNFGenerator.AddLiteral (VariableGenerator.TrueLiteral);
    CNFGenerator.AbortConstraint;

    ReportLn ('c Calling SatSolver');
    Result:= CNFGenerator.Solve;

  end;

  if PrintResult then
  begin

    ReportLn ('c Var= '+ IntToStr (CNFGenerator.VarCount)+ ' Clauses='+ IntToStr (CNFGenerator.ClauseCount));
    if Result then
    begin
      ReportLn ('s SATISFIABLE');
      Report ('v ');

      for i:= 1 to Min (GetSatSolver.VarCount- 1, Problem.InputVariableCount) do
      begin
        case GetSatSolver.GetValueInModel (Problem.InputVariable [i]) of
          gbTrue:
             Report ('x'+ IntToStr (Problem.InputVariable [i]- 1)+ ' ');
          gbFalse:
             Report ('-x'+ IntToStr (Problem.InputVariable [i]- 1)+ ' ');
          gbUnknown:
             Report ('?x'+ IntToStr (Problem.InputVariable [i]- 1)+ ' ');
        end;

      end;
      ReportLn ('');

    end
    else
      ReportLn ('s UNSATISFIABLE');

  end;

end;

function TAbstractPBSolverEngine.VerifyDecisionProblem(
  Problem: TPBSpecification; Assignment: TAssignments): Boolean;
var
  i: Integer;
  ActiveConstraint: TPBConstraint;
  Lit: TLiteral;

begin
  if GetRunTimeParameterManager.Verbosity and Ord (vbFull)<> 0 then
    GetCNFGenerator.ReportForcedVariables;

  Result:= False;

  if Problem.NonLinearVariableDescriptions.Count<> 0 then
    Halt; //Problem.VerifyNonLinearVariables;

  for i:= 0 to Problem.ConstraintCount- 1 do
  begin
    ActiveConstraint:= Problem.Constraint [i];
    if not VerifyHardConstraint (ActiveConstraint, Assignment) then
      Exit;

  end;

  Result:= True;

end;

function TAbstractPBSolverEngine.Solve (Problem: TPBSpecification): Boolean;
var
  i, j: Integer;
  ActiveClause: TClause;
  Lit: TLiteral;

begin
//  Problem.MergeAndSimplify;

  for i:= 0 to Problem.ClauseCount- 1 do
  begin
    ActiveClause:= Problem.Clause [i];

    CNFGenerator.BeginConstraint;
    for j:= 0 to ActiveClause.Count- 1 do
    begin
      Lit:= ActiveClause.Item [j];
      CNFGenerator.AddLiteral (Lit);

    end;

    CNFGenerator.SubmitClause;
 
  end;

  case Problem.SpecMode of
    smDecision:
    begin
      Result:= SolveDecisionProblem (Problem, True, True);

    end;
    smOptimization:
    begin
      Halt (1);
//      OptResult:= SolveOptimizationProblem (Problem, True);
//      Result:= OptResult<> 0;


    end;
    smWeightedOptimization:;
  end;

end;

function TAbstractPBSolverEngine.Verify (Problem: TPBSpecification;
  Assignments: TAssignments): Boolean;
var
  i, j: Integer;
  ActiveClause: TClause;
  Lit: TLiteral;
  Satisfied: Boolean;

begin

  Result:= False;

  for i:= 0 to Problem.ClauseCount- 1 do
  begin
    ActiveClause:= Problem.Clause [i];

    Satisfied:= False;
    for j:= 0 to ActiveClause.Count- 1 do
    begin
      Lit:= ActiveClause.Item [j];
      if Assignments.GetValue (Lit)= gbTrue then
      begin
        Satisfied:= True;
        Break;

      end;

    end;

    if not Satisfied then
      Exit;

  end;

  case Problem.SpecMode of
    smDecision:
      Result:= VerifyDecisionProblem (Problem, Assignments);
    smOptimization:
      Halt (1);
    smWeightedOptimization:
      Halt (1);
  end;

end;

procedure TAbstractPBSolverEngine.ProcessSigTerm;
begin
  if LastAnswer<> '' then
    ReportLn ('o '+ LastAnswer);

end;

procedure Finalize;
begin
  PBSolver.Free;

end;

initialization
finalization

end.

