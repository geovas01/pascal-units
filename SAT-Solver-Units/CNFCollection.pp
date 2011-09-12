unit CNFCollection;
{$mode objfpc}

interface

uses
  Classes, SysUtils, ClauseUnit, AbstractSolverUnit, MiniSatSolverInterfaceUnit,
    StreamUnit;

type

  { TCNFCollection}

  TCNFCollection= class (TMiniSatSolverInterface)
  private
    AllClauses: TClauseCollection;

  protected
    function GetCNF: TClauseCollection; override;
   
  public

    constructor Create;
    destructor Destroy; override;

    procedure SubmitClause; override;
    function Solve: Boolean; override;

    procedure SaveToFile (AnStream: TMyTextStream);
    procedure LoadFromFile (AnStream: TMyTextStream);

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

function TCNFCollection.GetCNF: TClauseCollection;
begin
  Result:= AllClauses.Copy;

end;

procedure TCNFCollection.SubmitClause; 
begin
  AllClauses.Add (TopConstraint.Copy);

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

procedure TCNFCollection.SaveToFile (AnStream: TMyTextStream);
var
  i: Integer;
  ActiveClause: TClause;

begin
  for i:= 0 to ClauseCount- 1 do
  begin
    ActiveClause:= AllClauses.Item [i];
    AnStream.WriteLine (ActiveClause.ToString);

  end;

end;

procedure TCNFCollection.LoadFromFile(AnStream: TMyTextStream);
begin

end;

end.
