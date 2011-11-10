unit ParameterManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SatSolverInterfaceUnit, NameValueCollectionUnit;

type
  TVerbosityMode= (vbNone= 0, vbMedium, vbFull);
  TParameterList= specialize TGenericNameValueCollection<AnsiString>;

  { TRunTimeParameterManager }

  TRunTimeParameterManager= class (TParameterList)
  private
    function GetVerbosity: Integer;

  public
    property Verbosity: Integer read GetVerbosity;

    function GetValueByName (Name: AnsiString): AnsiString; override;
    procedure AddArgument (Name, Value: AnsiString);
    constructor Create;
    destructor Destroy; override;

  end;

procedure Initialize;
procedure Finalize;
function GetRunTimeParameterManager: TRunTimeParameterManager; inline;

implementation
uses
  StreamUnit, ExceptionUnit;

var
  RunTimeParameterManager: TRunTimeParameterManager;

procedure Initialize;
begin
  if RunTimeParameterManager= nil then
    RunTimeParameterManager:= TRunTimeParameterManager.Create;

end;

procedure Finalize;
begin
  RunTimeParameterManager.Free;

end;

function GetRunTimeParameterManager: TRunTimeParameterManager; inline;
begin
  Result:= RunTimeParameterManager;

end;

{ TRunTimeParameterManager }

function TRunTimeParameterManager.GetVerbosity: Integer;
begin
  if GetValueByName ('--Verbosity')<> '' then
    Exit (StrToInt (GetValueByName ('--Verbosity')))
  else
    Exit (0);

end;

procedure TRunTimeParameterManager.AddArgument (Name, Value: AnsiString);
begin
  AddNameValue (UpperCase (Name), Value);
  Finalize;

end;

constructor TRunTimeParameterManager.Create;
  procedure PrintHelp;
  begin
    WriteLn ('Invalid Usage!');
    WriteLn (ExtractFileName (ParamStr (0))+ ' {Name Value}^* ');

  end;
const
  ValidArguments: array [1..7] of AnsiString=
    ('--ModularEncoder',
     '--SATSolverType',
     '--Verbosity',
     '--ExtraClausesLevel',
     '--UseRandomPermutation',
     '--InputFilename',
     '--OutputFilename'
    );
  ValidArgumentsValues: array [1..7] of array [1..5] of AnsiString=
    (
//--ModularEncoder:
     ('DP', 'DC', 'CARD.DP', 'CARD.DC', 'CARD.SN'),
//     '--SATSolverType',
     ('CNFCollection', 'InternalMiniSAT', 'NONE', 'NONE', 'NONE'),
//     '--Verbosity',
     ('0', '1', 'NONE', 'NONE', 'NONE'),
//     '--ExtraClausesLevel',
     ('Off', 'Medium', 'High', 'NONE', 'NONE'),
//     '--UseRandomPermutation',
     ('0', '1', 'None', 'NONE', 'NONE'),
//     '--InputFilename',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE'),
//     '--OutputFilename'
     ('NONE', 'NONE', 'None', 'NONE', 'NONE')
    );

  procedure CheckParameter (Name, Value: AnsiString);
  var
    i, j: Integer;
    Flag: Boolean;

  begin
    Flag:= False;
    for i:= Low (ValidArguments) to High (ValidArguments)- 2 do
    begin

      if UpperCase (Name)= UpperCase (ValidArguments [i]) then
        for j:= Low (ValidArgumentsValues [i]) to High (ValidArgumentsValues [i]) do
          if UpperCase (Value)= UpperCase (ValidArgumentsValues [i][j]) then
            Exit;


      WriteLn ('Invalid Argument Value:', Name, ' ', Value);
      PrintHelp;
      Halt (1);

    end;

  end;

  procedure SetDefaultValues;
  var
    i: Integer;

  begin
    for i:= 0 to Count - 1 do ;
  end;

var
  i: Integer;
  Name, V: AnsiString;

begin
  inherited;

  if Odd (Paramcount) then
  begin
    PrintHelp;
    raise Exception.Create ('Invalid set of parameters');

  end;

  i:= 1;

  while i<= Paramcount do
  begin
    Name:= ParamStr (i);
    if Paramcount< i+ 1 then
      Break;
    V:= ParamStr (i+ 1);
    CheckParameter (Name, V);
    AddArgument (Name, V);

    Inc (i, 2);

  end;

  SetDefaultValues;

  Finalize;

end;

destructor TRunTimeParameterManager.Destroy;
begin
  inherited Destroy;

end;

function TRunTimeParameterManager.GetValueByName (Name: AnsiString): AnsiString;
begin
  try
    Result:= inherited GetValueByName (UpperCase (Name))

  except
    on e: ENameNotFound do
      Result:= '';

  end;

end;

initialization
  RunTimeParameterManager:= nil;


end.

