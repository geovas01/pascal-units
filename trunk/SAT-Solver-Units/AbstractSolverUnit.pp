unit AbstractSolverUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClauseUnit, SatSolverInterfaceUnit;

type
  TSolverState= (ssConflict, //A conflict in the input CNF has been detected.
                 ssSAT,// The instance is satisfiable.                        
                 ssUnknown// No conflict detected but no solution has been found yet.
                );
  { TAbstractSatSolver }

  TAbstractSatSolver= class (TObject)
  private
  protected
    FSolverState: TSolverState;

  public
    property SolverState: TSolverState read FSolverState;

    constructor Create;
    destructor Destroy; override;

    function GetNewVar (VariablePolarity: TVariablePolarity= vpNone; Decide: Boolean= True): TVariable; virtual; abstract;
    {
    Adds a new Clause
    }
    function AddClause (AClause: TClause): Boolean; virtual; abstract;
    {
    Tries to find a solution for existing clauses.
      Returns True if a solution found, False otherwise.
    }
    function Solve: Boolean; virtual; abstract;
    {
      Tries to find a solution for existing clauses which maps Literal to True.
      Returns True if a solution found, False otherwise.
    }
    function Solve (Literal: TLiteral): Boolean; virtual; abstract;
    {
      Sets the decision mode for Variable.
    }
    procedure SetDecisionVar (Variable: TVariable; SetFlag: Boolean); virtual; abstract;
    {
      Returns the value of variable x
    }
    function GetValue (x: Integer): TGroundBool; virtual; abstract;
    {
      Returns the value of variable x in the last solution (this method should be
      called after Solve)
    }
    function GetValueInModel (x: Integer): TGroundBool; virtual; abstract;
    {
    }
    function NoAssigns: Integer; virtual; abstract;
    {
    Retursn the number of clauses
    }
    function NoClauses: Integer; virtual; abstract;
    {
    Retursn the number of variables.
    }
    function NoVars: Integer; virtual; abstract;

  end;

implementation

{ TAbstractSatSolver }

constructor TAbstractSatSolver.Create;
begin
  inherited Create;

  FSolverState:= ssUnknown

end;

destructor TAbstractSatSolver.Destroy;
begin

  inherited Destroy;
end;

end.

