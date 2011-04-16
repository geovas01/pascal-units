unit RWalkSolverUnit;
{$mode objfpc}

interface
uses
  AbstractSolverUnit, ClauseUnit, SatSolverInterfaceUnit;

type
  TRWalkSolver= class (TAbstractSatSolver)
  private
  public
    constructor Create;
    destructor Destroy; override;

    function GetNewVar (VariablePolarity: TVariablePolarity; Decide: Boolean): TVariable; override;
    function AddClause (AClause: TClause): Boolean; override;
    function Solve: Boolean; override;
    procedure SetDecisionVar (Variable: Integer; SetFlag: Boolean); override;
    function GetValue (x: Integer): TGroundBool; override;
    function GetValueInModel (x: Integer): TGroundBool; override; 
    function NoAssigns: Integer; override;
    function NoClauses: Integer; override;
    function NoVars: Integer; override;

  end;

implementation

constructor TRWalkSolver.Create;
begin
end;

destructor TRWalkSolver.Destroy; 
begin

end;

function TRWalkSolver.GetNewVar (VariablePolarity: TVariablePolarity; Decide: Boolean): TVariable;
begin

end;

function TRWalkSolver.AddClause (AClause: TClause): Boolean; 
begin

end;

function TRWalkSolver.Solve: Boolean; 
begin
end;

procedure TRWalkSolver.SetDecisionVar (Variable: Integer; SetFlag: Boolean); 
begin
end;

function TRWalkSolver.GetValue (x: Integer): TGroundBool; 
begin
end;

function TRWalkSolver.GetValueInModel (x: Integer): TGroundBool; 
begin
end;

function TRWalkSolver.NoAssigns: Integer; 
begin
end;

function TRWalkSolver.NoClauses: Integer;
begin
end;

function TRWalkSolver.NoVars: Integer; 
begin
end;


end.
