unit ParameterManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SatSolverInterfaceUnit;

type
  TVerbosityMode= (vbNone= 0, vbMedium, vbFull);

  { TRunTimeParameterManager }

  TRunTimeParameterManager= class (TStringList)
  private
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
    function GetValue(AName: AnsiString): AnsiString;

  public
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
    property Value [AName: AnsiString]: AnsiString read GetValue;

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

function TRunTimeParameterManager.GetValue (AName: AnsiString): AnsiString;
begin
//  if Self.In then
  Result:= Self.Strings [Self.IndexOf (AName)];

end;

constructor TRunTimeParameterManager.Create;
  procedure PrintHelp;
  begin
    WriteLn ('Invalid Usage!');
    WriteLn (ExtractFileName (ParamStr (0))+ ' Inputfile.pb --Verbosity {0,1,2} --MinimalPrimes {True/False} --DecisionVarMinization {False/True} ');
    WriteLn ('--VarPolarityHeuristic {True/False} --MinimizeTseitinVars {False/True} --MorePrimes {1.0,2,...}');
    WriteLn ('--MinimalPrimesMethod {Fast/Exact} --MinimizeTseitinVars {Fast/Exact} ');

  end;

var
  i: Integer;
  Name, V: AnsiString;

begin
  inherited;

  if not Odd (Paramcount) then
  begin
    PrintHelp;
    raise Exception.Create ('Invalid set of parameters');
  end;
    

  FVerbosity:= 0;
  i:= 2;
  FInputFilename:= ParamStr (1);

  FMinimalPrimes:= False;
  FDecisionVarMinization:= False;
  FVarPolarityHeuristic:= True;
  FMinimizeTseitinVars:= False;
  FMorePrimes:= 1;
  FMinimizeTseitinVarsMethod:= 'G';
  FMinimalPrimesMethod:= 'G';

  while i<= Paramcount do
  begin
    Name:= ParamStr (i);
    if Paramcount< i+ 1 then
      Break;
    V:= UpperCase (ParamStr (i+ 1));

    case UpCase (Name [9]) of
      'I'://--Verbosity or MorePrimes
        case UpCase (Name [3]) of
          'V': FVerbosity:= Ord (V [1])- Ord ('0');// Verbosity
          'M': FMorePrimes:= StrToFloat (V)// MorePrimes
          else
          begin
            PrintHelp ;
            raise Exception.Create ('Invalid parameter : "'+ Name+ ':'+ V);
    
          end;
        end; 
      'A'://--VarPolarityHeuristic
        FVarPolarityHeuristic:= V [1]= 'T';
      'F'://--OutputFile
        FOutputFilename:= V;
      'L'://--MinimalPrimes
         if Length (Name)= Length ('--MinimalPrimes') then
           FMinimalPrimes:= V [1]= 'T'
         else if Length (Name)= Length ('--MinimalPrimesMethod') then
           FMinimalPrimesMethod:= V [1];

      'O'://--DecisionVarMinization
        FDecisionVarMinization:= V [1]= 'T';
      'Z'://--MinimizeTseitinVars
      begin
         if Length (Name)= Length ('--MinimizeTseitinVars') then
           FMinimizeTseitinVars:= V [1]= 'T'
         else if Length (Name)= Length ('--MinimizeTseitinVarsMethod') then
           FMinimizeTseitinVarsMethod:= V [1]
         else
         begin
           PrintHelp ;
           raise Exception.Create ('Invalid parameter : "'+ Name+ ':'+ V);
   
         end;
   

      end
      else
      begin
        PrintHelp ;
        raise Exception.Create ('Invalid parameter : "'+ Name+ ':'+ V);

      end;

    end;

    Inc (i, 2);

  end;

  if MinimalPrimes and MinimizeTseitinVars then
  begin
    WriteLn ('MinimalPrimes and MinimizeTseitinVars can not be set at the same time');
    halt (1);

  end;

end;

destructor TRunTimeParameterManager.Destroy;
begin
  inherited Destroy;
end;

initialization
  RunTimeParameterManager:= nil;

end.

