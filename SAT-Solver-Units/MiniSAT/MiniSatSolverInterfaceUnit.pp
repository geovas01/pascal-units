unit MiniSatSolverInterfaceUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SatSolverInterfaceUnit, MiniSatSolverUnit, ClauseUnit;

type
  TSolverID= Integer;

  { TMiniSatSolverInterface }

  TMiniSatSolverInterface= class(TSATSolverInterface)
  private
    MiniSatSolver: TMiniSatSolver;

  protected
    procedure SyncInteractiveUPInfo; override;

    function GetVarCount: Int64; override;
{
    function GetClauseCount: Int64; override;
}


  public
    function Solve: Boolean; override;
    function Solve(Literal: TLiteral): Boolean; override;
    procedure GetSolution(out Answer: AnsiString); override;

    procedure SubmitClause; override;

    constructor Create;
    destructor Destroy; override;

    procedure SetDecisionVar(Variable: TVariable; SetFlag: Boolean); override;
    function GenerateNewVariable(VariablePolarity: TVariablePolarity; Decide: Boolean): Integer; override;

    function GetValue(v: Integer): TGroundBool; override;
    function GetValueInModel(v: Integer): TGroundBool; override;
  end;

implementation
uses
  TSeitinVariableUnit;
{ TMiniSatSolver }

procedure TMiniSatSolverInterface.SubmitClause;
begin
  if NoOfLiteralInTopConstraint[gbTrue]= 0 then
    MiniSatSolver.AddClause(TopConstraint);

  inherited;

end;

procedure TMiniSatSolverInterface.SyncInteractiveUPInfo;
begin
  raise Exception.Create('');

end;

function TMiniSatSolverInterface.GetValue(v: Integer): TGroundBool;
begin
  Result := MiniSatSolver.GetValue(v);

end;

function TMiniSatSolverInterface.GetValueInModel(v: Integer): TGroundBool;
begin
  Result := MiniSatSolver.GetValueInModel(v);

end;

function TMiniSatSolverInterface.Solve: Boolean;
begin
  Exit(MiniSatSolver.Solve);

end;

function TMiniSatSolverInterface.Solve(Literal: TLiteral): Boolean;
begin
  Exit(MiniSatSolver.Solve(Literal));

end;

procedure TMiniSatSolverInterface.GetSolution(out Answer: AnsiString);
begin
  raise Exception.Create('');

end;

function TMiniSatSolverInterface.GenerateNewVariable(VariablePolarity: TVariablePolarity; Decide: Boolean): Integer;
begin
  FVarCount := MiniSatSolver.GetNewVar(VariablePolarity, Decide);
  Exit(FVarCount);

end;

procedure TMiniSatSolverInterface.SetDecisionVar(Variable: Integer; SetFlag: Boolean);
begin
  MiniSatSolver.SetDecisionVar(Variable, SetFlag);

end;


function TMiniSatSolverInterface.GetVarCount: Int64;
begin
  Exit(MiniSatSolver.NoVars);

end;

{
function TMiniSatSolverInterface.GetClauseCount: Int64;
begin
  Exit(MiniSatSolver.NoClauses);

end;
}

constructor TMiniSatSolverInterface.Create;
begin
  inherited Create;

  MiniSatSolver := TMiniSatSolver.Create;

end;

destructor TMiniSatSolverInterface.Destroy;
begin
  MiniSatSolver.Free;

  inherited Destroy;

end;

end.

