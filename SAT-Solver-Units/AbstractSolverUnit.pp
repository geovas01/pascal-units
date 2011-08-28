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
    function AddClause (AClause: TClause): Boolean; virtual; abstract;
    function Solve: Boolean; virtual; abstract;
    function Solve (Literal: TLiteral): Boolean; virtual; abstract;
    procedure SetDecisionVar (Variable: TVariable; SetFlag: Boolean); virtual; abstract;
    function GetValue (x: Integer): TGroundBool; virtual; abstract;
    function GetValueInModel (x: Integer): TGroundBool; virtual; abstract;
    function NoAssigns: Integer; virtual; abstract;
    function NoClauses: Integer; virtual; abstract;
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

