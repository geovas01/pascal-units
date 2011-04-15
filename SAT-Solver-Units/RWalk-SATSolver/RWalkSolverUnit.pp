unit RWalkSolverUnit;
{$mode objfpc}

interface
uses
  AbstractSolverUnit;

type
  TRWalkSolver= class (TAbstractSatSolver)
  private
  public
    constructor Create;
    destructor Destroy; override;

    function GetNewVar (VariablePolrity: TVariablePolarity= vpNone; Decide: Boolean= True): TVariable; virtual; abstract;
    function AddClause (AClause: TClause): Boolean; virtual; abstract;
    function Solve: Boolean; virtual; abstract;
    procedure SetDecisionVar (Variable: Integer; SetFlag: Boolean); virtual; abstract;
    function GetValue (x: Integer): TGroundBool; virtual; abstract;
    function GetValueInModel (x: Integer): TGroundBool; virtual; abstract;
    function NoAssigns: Integer; virtual; abstract;
    function NoClauses: Integer; virtual; abstract;
    function NoVars: Integer; virtual; abstract;


  end;

implementation

end.
