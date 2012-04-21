unit CNFStreamUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SatSolverInterfaceUnit, ClauseUnit,
   StreamUnit;

type

  { TCNFStream }

  TCNFStream= class (TSATSolverInterface)
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
    function GenerateNewVariable (VariablePolrity: TVariablePolarity; Decide: Boolean): Integer; override;

    procedure SaveToFile;
//    procedure LoadFromFile (AnStream: TMyTextStream);

  end;

implementation
uses
  TSeitinVariableUnit;
{
function IntToStr (n: Int64): AnsiString; inline;
var
  CurDigit: PChar;

begin
  if n< 0 then
    Exit ('-'+ IntToStr (-n));
  if n= 0 then
    Exit ('0');

  SetLength (Result, 20);
  CurDigit:= @(Result [1]);

  while n<> 0 do
  begin
    CurDigit^:= Char (48+ n mod 10);
    Inc (CurDigit);
    n:= n div 10;

  end;
  SetLength (Result, CurDigit- @(Result [1]));

end;
}

function IntToStr (n: Integer): AnsiString; inline;
const
  MaxLength= 15;

var
  CurDigit: PChar;
  l: Integer;
  Sign: Boolean;

begin
  Result:= '               ';
  CurDigit:= @(Result [MaxLength]);
  Sign:= False;

  if n< 0 then
  begin
    Sign:= True;
    n:= -n;

  end;
  if n= 0 then
    Exit ('0');


  l:= MaxLength;
  while n<> 0 do
  begin
    CurDigit^:= Char (48+ n mod 10);
    Dec (CurDigit);
    n:= n div 10;
    Dec (l);

  end;
  if Sign then
    Result [l]:= '-';

end;

{ TCNFStream }

function TCNFStream.GetCNF: TClauseCollection;
begin
  WriteLn ('TCNFStream does not provide GetCNF function');
  Result:= nil;
  Halt (1);

end;

constructor TCNFStream.Create (OutputFilename: AnsiString);
begin
  inherited Create;

  OutputStream:= TMyTextStream.Create (
     TFileStream.Create (OutputFilename, fmCreate), True);
  OutputStream.WriteLine ('                                                   ');
  SubmittedClauseCount:= 0;
  MaxVarIndex:= 0;

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
    begin
      OutputStream.WriteStr ('-');
      OutputStream.WriteStr (IntToStr (GetVar (Lit)));//To avoid the overhead of fpc_ansichar_concat

    end
    else
      OutputStream.WriteStr (IntToStr (GetVar (Lit)));


    for j:= 1 to TopConstraint.Count- 1 do
    begin
      Lit:= TopConstraint.Item [j];
      if MaxVarIndex< GetVar (Lit) then
        MaxVarIndex:= GetVar (Lit);

      if IsNegated (Lit) then
      begin
        OutputStream.WriteStr (' -');
        OutputStream.WriteStr (IntToStr (GetVar (Lit)));

      end
      else
      begin
        OutputStream.WriteStr (' ');
        OutputStream.WriteStr (IntToStr (GetVar (Lit)));

      end;

    end;

  end;

  OutputStream.WriteLine (' 0');

  inherited;

end;

function TCNFStream.Solve: Boolean;
begin
  Result:= False;
  SaveToFile;

end;

function TCNFStream.GenerateNewVariable (VariablePolrity: TVariablePolarity;
                                 Decide: Boolean): Integer;
begin
  Inc (MaxVarIndex);
  Inc (FVarCount);
  Result:= MaxVarIndex;

end;

procedure TCNFStream.SaveToFile;
var
  i: Integer;
  NewClause: TClause;

begin
  for i:= 0 to MaxVarIndex do
    if GetValue (i)= gbTrue then
    begin
       NewClause:= TClause.Create (1, GetVariableManager.FalseLiteral);
       NewClause.Item [0]:= CreateLiteral (i, False);
       Inc (SubmittedClauseCount);
       OutputStream.WriteStr (IntToStr (i));
       OutputStream.WriteStr (' 0 ');


    end
    else if GetValue (i)= gbFalse then
    begin
       NewClause:= TClause.Create (1, GetVariableManager.FalseLiteral);
       NewClause.Item [0]:= CreateLiteral (i, True);
       Inc (SubmittedClauseCount);
       OutputStream.WriteStr (IntToStr (-i));
       OutputStream.WriteStr (' 0 ');

    end;

  OutputStream.Position:= 0;
  OutputStream.WriteStr ('p cnf '+ IntToStr (MaxVarIndex)+ ' '+ IntToStr (SubmittedClauseCount));

end;

end.

