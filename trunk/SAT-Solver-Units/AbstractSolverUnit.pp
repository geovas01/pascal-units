unit AbstractSolverUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClauseUnit, SatSolverInterfaceUnit;

type

  { TAbstractSatSolver }

  TAbstractSatSolver= class (TObject)
  private
  public
    constructor Create;
    destructor Destroy; override;

    function GetNewVar (VariablePolrity: TVariablePolarity= vpNone; Decide: Boolean= True): Integer; virtual; abstract;
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

{ TAbstractSatSolver }

constructor TAbstractSatSolver.Create;
begin
  inherited Create;

end;

destructor TAbstractSatSolver.Destroy;
begin

  inherited Destroy;
end;

end.

