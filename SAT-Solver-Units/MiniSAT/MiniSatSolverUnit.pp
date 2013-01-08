unit MiniSatSolverUnit;

{$mode objfpc}{$H+}


interface

uses
  Ctypes, Classes, SysUtils, AbstractSolverUnit, ClauseUnit, SatSolverInterfaceUnit;

type
  TMiniSatSolverID= Integer;

  { TMiniSatSolver }

  TMiniSatSolver= class (TAbstractSatSolver)
  private
    FSolverID: TMiniSatSolverID;
    LastVar: Integer;

    function GetIsOK: Boolean;
//    Clauses: TStringList;

  public
    property IsOK: Boolean read GetIsOK;
    constructor Create;
    destructor Destroy; override;

    function GetNewVar (VariablePolarity: TVariablePolarity= vpNone; Decide: Boolean= True): Integer; override;
    function AddClause (AClause: TClause): Boolean; override;
    function Solve: Boolean; override;
    function Solve (Literal: TLiteral): Boolean; override;
    function GetValue (x: Integer): TGroundBool; override;
    function GetValueInModel (x: Integer): TGroundBool; override;
    function NoAssigns: Integer; override;
    function NoClauses: Integer; override;
    function NoVars: Integer; override;

  end;

implementation
uses
  TSeitinVariableUnit, ParameterManagerUnit;

//{$link Interface.o}

{$linklib libMiniSatManager.so}
{$linklib c}
{$linklib stdc++}


{ TMiniSatSolver }

function cCreateNewSolver (): cTypes.cint32; cdecl; external;
procedure cDeleteSolver (ID: cTypes.cInt32); cdecl; external;
function cGetNewVar (ID: cTypes.cint32; Polarity: cTypes.cint32; Decide: cTypes.cint32): cTypes.cint32; cdecl; external;
function cSetRandomPolarity (ID: cTypes.cint32; PolarityMode: cTypes.cint32): cTypes.cint32; cdecl; external;

function cAddClause (ID: cTypes.cint32; Size: cTypes.cint32): cTypes.cint32; cdecl; external;
function cAddLiteralToClause (ID: cTypes.cint32; Lit: cTypes.cint32): cTypes.cint32; cdecl; external;

function cSolve (ID: cTypes.cint32): cTypes.cint32; cdecl; external;
function cSolve1 (ID: cTypes.cint32; Lit: cTypes.cint32): cTypes.cint32; cdecl; external;
function cSetDecisionVar (ID: cTypes.cint32; Variable: cTypes.cint32; SetFlag: cTypes.cint32): cTypes.cint32; cdecl; external;
function cGetValue (ID: cTypes.cint32; v: cTypes.cint32): cTypes.cint32; cdecl; external;
function cGetValueInModel (ID: cTypes.cint32; v: cTypes.cint32): cTypes.cint32; cdecl; external;
function cNoAssigns (ID: cTypes.cint32): cTypes.cint32; cdecl; external;
function cNoClauses (ID: cTypes.cint32): cTypes.cint32; cdecl; external;
function cNoVars (ID: cTypes.cint32): cTypes.cint32; cdecl; external;
function cIsSolverOkay (ID: cTypes.cint32): cTypes.cint32; cdecl; external;
function cFree: cTypes.cint32; cdecl; external;

function TMiniSatSolver.GetIsOK: Boolean;
begin
  Result:= cIsSolverOkay (FSolverID)<> 0;

end;

constructor TMiniSatSolver.Create;
begin
  inherited Create;
  LastVar:= 0;
  FSolverID:= cCreateNewSolver ();

  cSetRandomPolarity (FSolverID, 0);

end;

destructor TMiniSatSolver.Destroy;
begin
  cDeleteSolver (FSolverID);

  inherited Destroy;

end;

function TMiniSatSolver.GetNewVar (VariablePolarity: TVariablePolarity; Decide: Boolean): Integer;
begin
//  Result:= LastVar+ 1;
  Inc (LastVar);

  if Decide then
    Result:= MiniSatSolverUnit.cGetNewVar (FSolverID, Ord (VariablePolarity), 1)
  else
    Result:= MiniSatSolverUnit.cGetNewVar (FSolverID, Ord (VariablePolarity), 0);

end;

function TMiniSatSolver.AddClause (AClause: TClause): Boolean;
var
  i, Count: Integer;
  LiteralValue: TGroundBool;
  Lit: TLiteral;
  Temp: Integer;
  Flag: Boolean;
//  ActiveClause: AnsiString;

begin
  Count:= 0;
  Flag:= False;
//  ActiveClause:= '';

  for i:= 0 to AClause.Count- 1 do
  begin
    Lit:= AClause.Item [i];
    LiteralValue:= GetValue (GetVar (Lit));

    if IsNegated (Lit) then;
      LiteralValue:= TGroundBool (2- Ord (LiteralValue));

    case LiteralValue of
      gbUnknown:
      begin
        Inc (Count);
        Temp:= cAddLiteralToClause (FSolverID, AClause.Item [i]);
//        Write (LiteralToString (AClause.Item [i]), ' ');
//        ActiveClause+= LiteralToString (AClause.Item [i])+ ' ';
        Assert (Temp= Count);

      end;
      gbFalse:;
      gbTrue:
        Flag:= True;

    end;

  end;

  if not Flag then
  begin
//    WriteLN ('*', ActiveClause);
    if Count= 0 then
    begin
      cAddLiteralToClause (FSolverID, GetVariableManager.FalseLiteral);
      Result:= MiniSatSolverUnit.cAddClause (FSolverID, 1)<> 0;

    end
    else
      Result:= MiniSatSolverUnit.cAddClause (FSolverID, Count)<> 0;

  end
  else
    Result:= True;

  if not IsOK then
    Exit (False);

end;

function TMiniSatSolver.Solve: Boolean;
begin
  Result:= MiniSatSolverUnit.cSolve (FSolverID)<> 0;
//  Clauses.SaveToFile ('Clauses.cnf');

end;

function TMiniSatSolver.Solve (Literal: TLiteral): Boolean;
begin
  Result:= MiniSatSolverUnit.cSolve1 (FSolverID, Literal)<> 0;

end;

function TMiniSatSolver.GetValue (x: Integer): TGroundBool;
var
  v: Integer;

begin
  if GetVariableManager<> nil then
    if GetVariableManager.SimulationMode then
      if NoVars<= x then
        Exit (gbUnknown);

  while NoVars<= x do
    GetNewVar;

  v:= MiniSatSolverUnit.cGetValue (FSolverID, x);

  case v of 
    Ord ('T'): 
      Result:= gbTrue;
    Ord ('F'):
      Result:= gbFalse;
    Ord ('U'):
      Result:= gbUnknown
    else
      WriteLn ('Error!');

  end;

end;

function TMiniSatSolver.GetValueInModel (x: Integer): TGroundBool;
var
  v: Integer;

begin
  v:= MiniSatSolverUnit.cGetValueInModel (FSolverID, x);

  case v of 
    Ord ('T'): 
      Result:= gbTrue;
    Ord ('F'):
      Result:= gbFalse;
    Ord ('U'):
      Result:= gbUnknown
    else
      WriteLn ('Error!');

  end;

end;

function TMiniSatSolver.NoAssigns: Integer;
begin
  Result:= MiniSatSolverUnit.cNoAssigns (FSolverID);

end;

function TMiniSatSolver.NoClauses: Integer;
begin
  Result:= MiniSatSolverUnit.cNoClauses (FSolverID);

end;

function TMiniSatSolver.NoVars: Integer;
begin
  Result:= MiniSatSolverUnit.cNoVars (FSolverID);

end;

initialization

finalization
  cFree;

end.

