unit CNFStreamUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MiniSatSolverInterfaceUnit, ClauseUnit,
   StreamUnit;

type

  { TCNFStream }

  TCNFStream= class (TMiniSatSolverInterface)
  protected
    MaxVarIndex: Integer;
    SubmittedClauseCount: Integer;
    OutputStream: TMyTextStream;
    function GetCNF: TClauseCollection; override;

  public

    constructor Create (OutputFilename: AnsiString);
    destructor Destroy; override;

    procedure SubmitClause; override;
    function Solve: Boolean; override;

    procedure SaveToFile (AnStream: TMyTextStream);
    procedure LoadFromFile (AnStream: TMyTextStream);

  end;

implementation

{ TCNFStream }

function TCNFStream.GetCNF: TClauseCollection;
begin
  WriteLn ('TCNFStream does not provide GetCNF function');
  Halt (1);

end;

constructor TCNFStream.Create (OutputFilename: AnsiString);
begin
  inherited Create;

  OutputStream:= TMyTextStream.Create (
     TFileStream.Create (OutputFilename, fmCreate), True);
  OutputStream.WriteLine ('                                                   ');
  SubmittedClauseCount:= 0;

end;

destructor TCNFStream.Destroy;
begin
  OutputStream.Free;

  inherited Destroy;

end;

procedure TCNFStream.SubmitClause;
var
  j: Integer;
  Lit: TLiteral;

begin

  if 0< TopConstraint.Count then
  begin
    Inc (SubmittedClauseCount);
    Lit:= TopConstraint.Item [0];

    if IsNegated (Lit) then
      OutputStream.WriteStr ('-'+ IntToStr (GetVar (Lit)))
    else
      OutputStream.WriteStr (IntToStr (GetVar (Lit)));


    for j:= 1 to TopConstraint.Count- 1 do
    begin
      Lit:= TopConstraint.Item [j];
      if MaxVarIndex< GetVar (Lit) then
        MaxVarIndex:= GetVar (Lit);

      if IsNegated (Lit) then
        OutputStream.WriteStr (' -'+ IntToStr (GetVar (Lit)))
      else
        OutputStream.WriteStr (' '+ IntToStr (GetVar (Lit)));

    end;

  end;

  OutputStream.WriteLine (' 0');

  inherited;

end;

function TCNFStream.Solve: Boolean;
begin
  Result:= False;

end;

procedure TCNFStream.SaveToFile(AnStream: TMyTextStream);
var
  i: Integer;
  NewClause: TClause;

begin
  for i:= 0 to MaxVarIndex do
    if GetValue (i)= gbTrue then
    begin
       NewClause:= TClause.Create (1);
       NewClause.Item [0]:= CreateLiteral (i, False);
       Inc (SubmittedClauseCount);
       OutputStream.WriteStr (IntToStr (i));
       OutputStream.WriteStr (' 0 ');


    end
    else if GetValue (i)= gbFalse then
    begin
       NewClause:= TClause.Create (1);
       NewClause.Item [0]:= CreateLiteral (i, True);
       Inc (SubmittedClauseCount);
       OutputStream.WriteStr (IntToStr (-i));
       OutputStream.WriteStr (' 0 ');

    end;

  OutputStream.Position:= 0;
  AnStream.WriteLine ('p cnf '+ IntToStr (MaxVarIndex)+ ' '+ IntToStr (SubmittedClauseCount));

end;

procedure TCNFStream.LoadFromFile(AnStream: TMyTextStream);
begin

end;

end.

