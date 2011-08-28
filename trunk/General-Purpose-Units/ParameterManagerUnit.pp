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
    function GetValueByName (Name: AnsiString): AnsiString; override;

{
    FInputFilename: AnsiString;
    FOutputFilename: AnsiString;
    FVerbosity: Integer;
    FMinimalPrimes: Boolean; 
    FMorePrimes: Extended; 
    FDecisionVarMinization: Boolean;
    FPolarityMode: TVariablePolarity;
    FVarPolarityHeuristic: Boolean;
    FMinimizeTseitinVars: Boolean;
    FMinimizeTseitinVarsMethod: char;
    FMinimalPrimesMethod: char;
}
  public
{
    property Verbosity: Integer read FVerbosity;
    property MinimalPrimes: Boolean read FMinimalPrimes;
    property InputFilename: AnsiString read FInputFilename;
    property OutputFilename: AnsiString read FOutputFilename;
    property DecisionVarMinization: Boolean read FDecisionVarMinization;
    property PolarityMode: TVariablePolarity read FPolarityMode;
    property VarPolarityHeuristic: Boolean read FVarPolarityHeuristic;
    property MinimizeTseitinVars: Boolean read FMinimizeTseitinVars;
    property MinimalPrimesMethod: Char read FMinimalPrimesMethod;
    property MinimizeTseitinVarsMethod: Char read FMinimizeTseitinVarsMethod;
    property MorePrimes: Extended read FMorePrimes;
}

    procedure AddArgument (Name, Value: AnsiString);
    constructor Create;
    destructor Destroy; override;

  end;

procedure Initialize;
procedure Finalize;
function GetRunTimeParameterManager: TRunTimeParameterManager; inline;

implementation
uses
  StreamUnit;

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
    AddArgument (Name, V);
    

    Inc (i, 2);

  end;

  Finalize;

end;

destructor TRunTimeParameterManager.Destroy;
begin
  inherited Destroy;

end;

function TRunTimeParameterManager.GetValueByName (Name: AnsiString): AnsiString;
begin
  Result:= inherited GetValueByName (UpperCase (Name));

end;

initialization
  RunTimeParameterManager:= nil;


end.

