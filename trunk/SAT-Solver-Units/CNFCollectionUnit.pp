unit CNFCollectionUnit;
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
uses
  ParameterManagerUnit;

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
  Stream: TMyTextStream;

begin

  if GetRunTimeParameterManager.GetValueByName ('--OutputFilename') <> '' then
  begin
    Stream:= TMyTextStream.Create (TFileStream.Create (GetRunTimeParameterManager.GetValueByName ('--OutputFilename'), fmCreate), True);
    SaveToFile (Stream);
    Stream.Free;

  end
  else
    for i:= 0 to AllClauses.Count- 1 do
      WriteLn (AllClauses.Item [i].ToString);

  Result:= False;

end;

procedure TCNFCollection.SaveToFile (AnStream: TMyTextStream);
var
  i, j: Integer;
  Lit: TLiteral;
  ActiveClause: TClause;
  MaxVarIndex: Integer;
  

begin
  MaxVarIndex:= -1;
  for i:= 0 to AllClauses.Count- 1 do
  begin
    ActiveClause:= AllClauses.Item [i];

    for j:= 0 to ActiveClause.Count- 1 do
      if MaxVarIndex< GetVar (ActiveClause.Item [j]) then
        MaxVarIndex:= GetVar (ActiveClause.Item [j]);

  end;


  AnStream.WriteLine ('p cnf '+ IntToStr (MaxVarIndex+ 1)+ ' '+ IntToStr (AllClauses.Count));
  for i:= 0 to AllClauses.Count- 1 do
  begin
    ActiveClause:= AllClauses.Item [i];
 
    if 0< ActiveClause.Count then
    begin
      Lit:= ActiveClause.Item [0];
   
      if IsNegated (Lit) then
        AnStream.WriteStr ('-'+ IntToStr (GetVar (Lit)))
      else
        AnStream.WriteStr (IntToStr (GetVar (Lit)));
  
  
      for j:= 1 to ActiveClause.Count- 1 do
      begin
        Lit:= ActiveClause.Item [j];
   
        if IsNegated (Lit) then
          AnStream.WriteStr (' -'+ IntToStr (GetVar (Lit)))
        else
          AnStream.WriteStr (' '+ IntToStr (GetVar (Lit)));
  
      end;

    end;

    AnStream.WriteLine (' 0');

  end;

end;

procedure TCNFCollection.LoadFromFile(AnStream: TMyTextStream);
begin

end;

end.
