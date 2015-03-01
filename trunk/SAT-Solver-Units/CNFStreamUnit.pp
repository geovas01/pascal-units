unit CNFStreamUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SatSolverInterfaceUnit, ClauseUnit,
   StreamUnit;

type

  { TCNFStream }

  TCNFStream= class(TSATSolverInterface)
  protected
    MaxVarIndex: Integer;
    SubmittedClauseCount: Integer;
    OutputStream: TMyTextStream;
    function GetCNF: TClauseCollection; override;

  public

    constructor Create(OutputFilename: AnsiString);
    destructor Destroy; override;

    procedure SubmitClause; override;
    function Solve: Boolean; override;
    function GenerateNewVariable(VariablePolrity: TVariablePolarity; Decide: Boolean): Integer; override;

    procedure SaveToFile;
//    procedure LoadFromFile(AnStream: TMyTextStream);

  end;

implementation
uses
  TSeitinVariableUnit;
{
function IntToStr(n: Int64): AnsiString; inline;
var
  CurDigit: PChar;

begin
  if n< 0 then
    Exit('-'+ IntToStr(-n));
  if n= 0 then
    Exit('0');

  SetLength(Result, 20);
  CurDigit:= @(Result[1]);

  while n<> 0 do
  begin
    CurDigit^:= Char(48+ n mod 10);
    Inc(CurDigit);
    n:= n div 10;

  end;
  SetLength(Result, CurDigit- @(Result[1]));

end;


function IntToStr(n: Int64): AnsiString; inline;
const
  MaxLength= 15;

var
  CurDigit: PChar;
  l: Integer;
  Sign: Boolean;

begin
  Result:= '***************';
  CurDigit:= @(Result[MaxLength]);
  Sign:= False;

  if n< 0 then
  begin
    Sign:= True;
    n:= -n;

  end;
  if n= 0 then
    Exit('0');


  l:= MaxLength;
  while n<> 0 do
  begin
    CurDigit^:= Char(48+ n mod 10);
    Dec(CurDigit);
    n:= n div 10;
    Dec(l);

  end;
  if Sign then
    CurDigit^:= '-';

end;
}

{ TCNFStream }

function TCNFStream.GetCNF: TClauseCollection;
begin
  WriteLn('TCNFStream does not provide GetCNF function');
  Result:= nil;
  Halt(1);

end;

constructor TCNFStream.Create(OutputFilename: AnsiString);
begin
  inherited Create;

  OutputStream:= TMyTextStream.Create(
     TFileStream.Create(OutputFilename, fmCreate), True);
  OutputStream.WriteLine('                                                   ');
  SubmittedClauseCount:= 0;
  MaxVarIndex:= 0;

end;

destructor TCNFStream.Destroy;
begin
  OutputStream.Free;

  inherited Destroy;

end;

procedure TCNFStream.SubmitClause;

  procedure WriteInt(Lit: Integer; var OutputStringPtr: PChar);
  const
    Pow10: array[0..9] of Integer=
    (1, 10, 100, 1000, 10000, 100000, 1000000,
      10000000, 100000000, 1000000000);

    function GetLength(n: Integer): Integer;
    var
      Top, Bot, Mid: Integer;

    begin
      Bot:= 0; Top:= 9;

      if Pow10[Top]<= n then
        Exit(Top+ 1);
      if n<= Pow10[Bot] then
        Exit(Bot+ 1);

      while Bot<= Top do
      begin
        Mid:=(Top+ Bot) shr 1;

        if n< Pow10[Mid] then
          Top:= Mid- 1
        else if Pow10[Mid]<= n then
          Bot:= Mid+ 1
        else
          Exit(Mid+ 1);

      end;

      Result:= Bot;
    end;

  var
    RightPtr: PChar;

  begin
    RightPtr:= OutputStringPtr+ GetLength(Lit)- 1;
    OutputStringPtr:= RightPtr+ 1;

    while Lit<> 0 do
    begin
      RightPtr^:= char(48+(Lit mod 10));
      Lit:= Lit div 10;
      Dec(RightPtr);

    end;

  end;

  procedure WriteStr(const S: AnsiString; var OutputStringPtr: PChar);
  begin
    Move(S[1], OutputStringPtr^, Length(S));
    Inc(OutputStringPtr, Length(S));

  end;

var
  i, j: Integer;
  Lit: TLiteral;
  ActiveClause: TClause;
  OutputString: AnsiString;
  OutputStringPChar: PChar;

begin
  ActiveClause:= TopConstraint;

  SetLength(OutputString, ActiveClause.Count* 10);
  FillChar(OutputString[1], ActiveClause.Count* 10, ' ');
  OutputStringPChar:= @(OutputString[1]);

  if 0< ActiveClause.Count then
  begin
    Inc(SubmittedClauseCount);
    Lit:= ActiveClause.Items[0];

    if IsNegated(Lit) then
    begin
      OutputStringPChar^:= '-'; Inc(OutputStringPChar);// WriteStr('-', OutputString);
      WriteInt(GetVar(Lit), OutputStringPChar);
      Inc(OutputStringPChar)//WriteStr(' ', OutputString);

    end
    else
    begin
      WriteInt(GetVar(Lit), OutputStringPChar);
      Inc(OutputStringPChar)//WriteStr(' ', OutputString);

    end;

    for j:= 1 to ActiveClause.Count- 1 do
    begin
      Lit:= ActiveClause.Items[j];

      if IsNegated(Lit) then
      begin
        OutputStringPChar^:= '-'; Inc(OutputStringPChar);// WriteStr('-', OutputString);
        WriteInt(GetVar(Lit), OutputStringPChar);
        Inc(OutputStringPChar)//WriteStr(' ', OutputString);

      end
      else
      begin
        WriteInt(GetVar(Lit), OutputStringPChar);
        Inc(OutputStringPChar)//WriteStr(' ', OutputString);

      end;

    end;

  end;

  OutputStream.WriteStr(OutputString);
  OutputStream.WriteLine(' 0');

  inherited;

end;

function TCNFStream.Solve: Boolean;
begin
  Result:= False;
  SaveToFile;

end;

function TCNFStream.GenerateNewVariable(VariablePolrity: TVariablePolarity;
                                 Decide: Boolean): Integer;
begin
  Inc(MaxVarIndex);
  Inc(FVarCount);
  Result:= MaxVarIndex;

end;

procedure TCNFStream.SaveToFile;

var
  i: Integer;
  NewClause: TClause;

begin
  for i:= 0 to MaxVarIndex do
    if GetValue(i)= gbTrue then
    begin
       NewClause:= TClause.Create(1, GetVariableManager.FalseLiteral);
       NewClause.Items[0]:= CreateLiteral(i, False);
       Inc(SubmittedClauseCount);
       OutputStream.WriteStr(IntToStr(i));
       OutputStream.WriteLine(' 0 ');


    end
    else if GetValue(i)= gbFalse then
    begin
       NewClause:= TClause.Create(1, GetVariableManager.FalseLiteral);
       NewClause.Items[0]:= CreateLiteral(i, True);
       Inc(SubmittedClauseCount);
       OutputStream.WriteStr(IntToStr(-i));
       OutputStream.WriteLine(' 0 ');

    end;

  OutputStream.Position:= 0;
  OutputStream.WriteStr('p cnf '+ IntToStr(MaxVarIndex)+ ' '+ IntToStr(SubmittedClauseCount));

end;

end.

