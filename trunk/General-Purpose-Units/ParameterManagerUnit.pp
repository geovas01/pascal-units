unit ParameterManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SatSolverInterfaceUnit, NameValueCollectionUnit;

type
  TVerbosityMode= (vbNone= 0, vbMedium, vbFull, vbEveryThing= 4);
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
{$i ValidArguments.inc }

  procedure CheckParameter (Name, Value: AnsiString);
  var
    i, j: Integer;
    Flag: Boolean;

  begin
    Flag:= False;
    for i:= Low (ValidArguments) to High (ValidArguments)- 2 do
    begin

      if UpperCase (Name)= UpperCase (ValidArguments [i]) then
      begin
        for j:= Low (ValidArgumentsValues [i]) to High (ValidArgumentsValues [i]) do
          if UpperCase (Value)= UpperCase (ValidArgumentsValues [i][j]) then
            Exit;


        WriteLn ('Invalid Argument Value:', Name, ' ', Value, '.');
        WriteLn ('Valid Arguments for ', Name, ' are');
        for j:= Low (ValidArgumentsValues [i]) to High (ValidArgumentsValues [i])  do
          Write (ValidArgumentsValues [i, j], ' , ');
        Halt (1);

      end;

    end;

    for i:= Low (ValidArguments) to High (ValidArguments) do
      if UpperCase (Name)= UpperCase (ValidArguments [i]) then
        Exit;

    WriteLn ('Invalid Name :', Name, '.');
    WriteLn ('Valid Parameters are: ');
    for i:= Low (ValidArguments) to High (ValidArguments)  do
      Write (ValidArguments [i], ' , ');
    Halt (1);


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

