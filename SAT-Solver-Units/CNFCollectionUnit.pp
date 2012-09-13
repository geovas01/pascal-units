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
  ParameterManagerUnit, TSeitinVariableUnit;

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
  if NoOfLiteralInTopConstraint [gbTrue]= 0 then
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

const
  Digits: array [0..9] of char= ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

procedure TCNFCollection.SaveToFile (AnStream: TMyTextStream);

  procedure WriteInt (Lit: Integer; var OutputStringPtr: PChar);
  const
    Pow10: array [0..9] of Integer=
     (1, 10, 100, 1000, 10000, 100000, 1000000,
      10000000, 100000000, 1000000000);

    function GetLength (n: Integer): Integer;
    var
      Top, Bot, Mid: Integer;

    begin
      Bot:= 0; Top:= 9;

      if Pow10 [Top]<= n then
        Exit (Top+ 1);
      if n<= Pow10 [Bot] then
        Exit (Bot+ 1);

      while Bot<= Top do
      begin
        Mid:= (Top+ Bot) shr 1;

        if n< Pow10 [Mid] then
          Top:= Mid- 1
        else if Pow10 [Mid]<= n then
          Bot:= Mid+ 1
        else
          Exit (Mid+ 1);

      end;

      Result:= Bot;
    end;

  var
    RightPtr: PChar;

  begin
    RightPtr:= OutputStringPtr+ GetLength (Lit)- 1;
    OutputStringPtr:= RightPtr+ 1;

    while Lit<> 0 do
    begin
      RightPtr^:= char (48+ (Lit mod 10));
      Lit:= Lit div 10;
      Dec (RightPtr);

    end;

  end;

  procedure WriteStr (const S: AnsiString; var OutputStringPtr: PChar);
  begin
    Move (S [1], OutputStringPtr^, Length (S));
    Inc (OutputStringPtr, Length (S));

  end;

var
  i, j: Integer;
  Lit: TLiteral;
  ActiveClause: TClause;
  MaxVarIndex: Integer;
  OutputString: AnsiString;
  OutputStringPChar: PChar;

begin
  MaxVarIndex:= -1;

  for i:= 0 to AllClauses.Count- 1 do
  begin
    ActiveClause:= AllClauses.Item [i];

    for j:= 0 to ActiveClause.Count- 1 do
      if MaxVarIndex< GetVar (ActiveClause.Item [j]) then
        MaxVarIndex:= GetVar (ActiveClause.Item [j]);

  end;

  for i:= 0 to MaxVarIndex do
    if GetValue (i)= gbTrue then
    begin
       ActiveClause:= TClause.Create (1, GetVariableManager.FalseLiteral);
       ActiveClause.Item [0]:= CreateLiteral (i, False);
       AllClauses.Add (ActiveClause);

    end
    else if GetValue (i)= gbFalse then
    begin
       ActiveClause:= TClause.Create (1, GetVariableManager.FalseLiteral);
       ActiveClause.Item [0]:= CreateLiteral (i, True);
       AllClauses.Add (ActiveClause);

    end;

  AnStream.WriteLine ('p cnf '+ IntToStr (MaxVarIndex)+ ' '+ IntToStr (AllClauses.Count));

  for i:= 0 to AllClauses.Count- 1 do
  begin
    ActiveClause:= AllClauses.Item [i];
    SetLength (OutputString, ActiveClause.Count* 10);
    FillChar (OutputString [1], ActiveClause.Count* 10, ' ');
    OutputStringPChar:= @(OutputString [1]);

    if 0< ActiveClause.Count then
    begin
      Lit:= ActiveClause.Item [0];
   
      if IsNegated (Lit) then
      begin
        OutputStringPChar^:= '-'; Inc (OutputStringPChar);// WriteStr ('-', OutputString);
        WriteInt (GetVar (Lit), OutputStringPChar);
        Inc (OutputStringPChar)//WriteStr (' ', OutputString);

      end
      else
      begin
        WriteInt (GetVar (Lit), OutputStringPChar);
        Inc (OutputStringPChar)//WriteStr (' ', OutputString);

      end;

  
      for j:= 1 to ActiveClause.Count- 1 do
      begin
        Lit:= ActiveClause.Item [j];
   
        if IsNegated (Lit) then
        begin
          OutputStringPChar^:= '-'; Inc (OutputStringPChar);// WriteStr ('-', OutputString);
          WriteInt (GetVar (Lit), OutputStringPChar);
          Inc (OutputStringPChar)//WriteStr (' ', OutputString);

        end
        else
        begin
          WriteInt (GetVar (Lit), OutputStringPChar);
          Inc (OutputStringPChar)//WriteStr (' ', OutputString);

        end;

      end;

    end;

    AnStream.WriteStr (OutputString);
    AnStream.WriteLine (' 0');

  end;

end;

procedure TCNFCollection.LoadFromFile(AnStream: TMyTextStream);
begin

end;

end.
