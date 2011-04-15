unit CNFCollection;
{$mode objfpc}

interface

uses
  Classes, SysUtils, ClauseUnit, AbstractSolverUnit, MiniSatSolverInterfaceUnit;

type

  { TCNFCollection}

  TCNFCollection= class (TMiniSatSolverInterface)
  private
    AllClauses: TClauseCollection;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SubmitClause; override;
    function Solve: Boolean; override;

  end;

implementation

{ TCNFCollection }

constructor TCNFCollection.Create;
begin
  inherited Create;

  AllClauses:= TClauseCollection.Create;

end;

destructor TCNFCollection.Destroy;
begin
  AllClauses.Free;

  inherited Destroy;

end;

procedure TCNFCollection.SubmitClause; 
begin
   AllClauses.Add (TopConstraint.Copy);
  WriteLn ('*', TopConstraint.ToString);

  inherited;

end;

function TCNFCollection.Solve: Boolean;
var
  i: Integer;

begin
  for i:= 0 to AllClauses.Count- 1 do
    WriteLn (AllClauses.Item [i].ToString);

  Result:= inherited;

end;

end.
