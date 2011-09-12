unit SatSolverInterfaceUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, GenericStackUnit, ClauseUnit, MyTypes;

type
  TArrayofInteger=  specialize TGenericCollectionForBuiltInData<Integer>;
  TClauseArrayOfIntegerPair= specialize TPair<TClause, TArrayofInteger>;
  TStackOfClauses= specialize TGenericStack<TClauseArrayOfIntegerPair>;
  TVariablePolarity= (vpFalse= 0, vpTrue, vpNone);

  TSolverResult= (srError, srSAT, srUNSAT);

  TSatSolverType= (ssMiniSatSolver, ssCNFCollection);

  { TSATSolverInterface }

  TSATSolverInterface= class (TObject)
  private
    FClausesStack: TStackOfClauses;
    FTopConstraint: TClause;
    FNoOfLiteralInTopConstraint: TArrayOfInteger;

    function GetNoOfLiteralInTopConstraint (gbValue: TGroundBool): Integer; inline;
    function GetTopConstarintSize: Integer; inline;

  protected
    FVarCount: Int64;
    FClauseCount: Int64;
    FSolverResult: TSolverResult;
//    Assignements: array of TGroundBool;


    function GetVarCount: Int64; virtual; 
    function GetClauseCount: Int64; virtual;
    function GetCNF: TClauseCollection; virtual; 
    function GetValue (v: Integer): TGroundBool; virtual;
    property Stack: TStackOfClauses read FClausesStack;

    procedure SyncInteractiveUPInfo; virtual;

  public
    property VarCount: Int64 read GetVarCount;
    property ClauseCount: Int64 read GetClauseCount;
    property TopConstraint: TClause read FTopConstraint;
    property TopConstarintSize: Integer read GetTopConstarintSize;
    property NoOfLiteralInTopConstraint [gbValue: TGroundBool]: Integer read GetNoOfLiteralInTopConstraint;
    property CNF: TClauseCollection read GetCNF;

    procedure AddComment (var Comment: AnsiString); virtual;

//    function GenerateNewVariable (VariablePolrity: TVariablePolarity= vpNone; Decide: Boolean= True): Integer; virtual; abstract;
    function GenerateNewVariable (VariablePolrity: TVariablePolarity; Decide: Boolean): Integer; virtual; abstract;

    function BeginConstraint: TClause; inline;
    procedure AbortConstraint; virtual; 
    procedure AddLiteral (Lit: TLiteral); virtual; 
    procedure AddClause (AClause: TClause); inline;

    procedure SubmitAndGate (p: TLiteral); virtual;
    procedure SubmitOrGate (p: TLiteral); virtual;
    procedure SubmitClause; virtual;

    function Solve: Boolean; virtual; abstract;
    function Solve (Literal: TLiteral): Boolean; virtual; abstract;
    procedure GetSolution (out Answer: AnsiString); virtual; abstract;
    function GetResult: TSolverResult;

    function GetLiteralValue (Lit: TLiteral): TGroundBool; inline;
    function GetValueInModel (v: Integer): TGroundBool; virtual;
    function GetLiteralValueInModel (Lit: TLiteral): TGroundBool; inline;
    procedure ImportModel (HighIndex: Integer; Model: TIntegerCollection); virtual;

    constructor Create;
    destructor Destroy; override;

    procedure ReportForcedVariables; virtual;

    procedure SetDecisionVar (Variable: Integer; SetFlag: Boolean); virtual; abstract;

  end;

function GetSatSolver: TSATSolverInterface;
function ReNewSatSolver (SatSolverType: TSatSolverType= ssMiniSatSolver): TSATSolverInterface;
procedure Initialize;
procedure Finalize;

implementation
uses
  TSeitinVariableUnit,  MiniSatSolverInterfaceUnit, CNFCollection,
  ParameterManagerUnit, StreamUnit;

var
  SatSolverInterface: TSATSolverInterface;

function GetSatSolver: TSATSolverInterface;
begin
  Result:= SatSolverInterface;

end;

function ReNewSatSolver (SatSolverType: TSatSolverType): TSATSolverInterface; inline;
begin
  SatSolverInterface.Free;

  case SatSolverType of
    ssMiniSatSolver: SatSolverInterface:= TMiniSatSolverInterface.Create;
    ssCNFCollection:  SatSolverInterface:= TCNFCollection.Create
    else    ;

  end;

  Result:= SatSolverInterface;

  Result.BeginConstraint;
  Result.AddLiteral (GetVariableManager.TrueLiteral);
  Result.SubmitClause;

end;

procedure Initialize;
begin
  if UpperCase (GetRunTimeParameterManager.Value ['SatSolverType'])= UpperCase ('CNFCollection') then
    SatSolverInterface:= TCNFCollection.Create
  else 
    SatSolverInterface:= TMiniSatSolverInterface.Create;

{  if GetRunTimeParameterManager.SATSolverType= ssMiniSatSolver then
     SatSolverInterface:= TMiniSatSolverInterface.Create
  else if GetRunTimeParameterManager.SATSolverType= ssCNFCollection then
      SatSolverInterface:= TCNFCollection.Create;
}

end;

{ TSATSolverInterface }


procedure TSATSolverInterface.SyncInteractiveUPInfo;
begin
  raise Exception.Create ('SyncInteractiveUPInfo');
 
end;

function TSATSolverInterface.GetValue (v: Integer): TGroundBool;
begin
  Exit (gbUnknown);

end;

function TSATSolverInterface.GetLiteralValue (Lit: TLiteral): TGroundBool; inline;
begin
  if IsNegated (Lit) then
    Exit (TGroundBool (2- Ord (GetValue (GetVar (Lit)))))
  else
    Exit (GetValue (GetVar (Lit)));

end;

function TSATSolverInterface.GetValueInModel (v: Integer): TGroundBool;
begin
  raise Exception.Create ('GetValueInModel');
  Exit (gbUnknown);

end;

function TSATSolverInterface.GetLiteralValueInModel (Lit: TLiteral): TGroundBool;
begin
  if IsNegated (Lit) then
    Exit (TGroundBool (2- Ord (GetValueInModel (GetVar (Lit)))))
  else
    Exit (GetValueInModel (GetVar (Lit)));


end;

procedure TSATSolverInterface.ImportModel (HighIndex: Integer; Model: TIntegerCollection);
var
  v: Integer;

begin
  Model.Count:= HighIndex+ 1;

  for v:= 0 to HighIndex do
    if GetValueInModel (v)= gbTrue then
      Model.Item [v]:= 1
    else
      Model.Item [v]:= -1;

end;

function TSATSolverInterface.GetNoOfLiteralInTopConstraint (gbValue: TGroundBool): Integer;
begin
  Result:= FNoOfLiteralInTopConstraint.Item [Ord (gbValue)];

end;

function TSATSolverInterface.GetTopConstarintSize: Integer;
begin
  Result:= TopConstraint.Count;

end;

function TSATSolverInterface.GetVarCount: Int64;
begin
  Result:= FVarCount;

end;

function TSATSolverInterface.GetClauseCount: Int64;
begin
  Result:= FClauseCount;

end;

function TSATSolverInterface.GetCNF: TClauseCollection;
begin
  raise Exception.Create ('GetCNF is not implemented here');

end;

procedure TSATSolverInterface.SubmitClause;
begin
  Inc (FClauseCount, 1);

  AbortConstraint;
  Inc (FClauseCount, 1);

end;

procedure TSATSolverInterface.AddComment (var Comment: AnsiString);
begin

end;

function TSATSolverInterface.BeginConstraint: TClause;
var
  Pair: TClauseArrayOfIntegerPair;

begin
  FTopConstraint:= TClause.Create;
  FNoOfLiteralInTopConstraint:= TArrayOfInteger.Create (3);
  Pair:= TClauseArrayOfIntegerPair.Create (FTopConstraint, FNoOfLiteralInTopConstraint);
  Stack.Push (Pair);
  Result:= FTopConstraint;

  FNoOfLiteralInTopConstraint.Item [0]:= 0;
  FNoOfLiteralInTopConstraint.Item [1]:= 0;
  FNoOfLiteralInTopConstraint.Item [2]:= 0;

end;

procedure TSATSolverInterface.AbortConstraint;
var
  Pair: TClauseArrayOfIntegerPair;

begin
  if Stack.Count> 0 then
  begin
    Pair:= Stack.Pop;

    Pair.Free;
    FTopConstraint:= nil;
    FNoOfLiteralInTopConstraint:= nil;

  end;

  if Stack.Count> 0 then
  begin
    Pair:= Stack.Top;
    FTopConstraint:= Pair.First;
    FNoOfLiteralInTopConstraint:= Pair.Second;

  end;

  Dec (FClauseCount);

end;

procedure TSATSolverInterface.AddLiteral (Lit: TLiteral);
var
  v: Integer;
  LiteralValue: TGroundBool;

begin
  v:= GetVar (Lit);

  while VarCount<= v do
    GenerateNewVariable (vpNone, True);

  TopConstraint.AddItem (Lit);

  LiteralValue:= GetValue (v);
  if IsNegated (Lit) then
    LiteralValue:= TGroundBool (2- Ord (LiteralValue));

  FNoOfLiteralInTopConstraint.Item [Ord (LiteralValue)]:= FNoOfLiteralInTopConstraint.Item [Ord (LiteralValue)]+ 1;

end;

procedure TSATSolverInterface.AddClause(AClause: TClause); inline;
var
  i: Integer;

begin
  BeginConstraint;

  for i:= 0 to AClause.Count- 1 do
    AddLiteral (AClause.Item [i]);

  SubmitClause;

end;

procedure TSATSolverInterface.SubmitAndGate (p: TLiteral);
var
  pV: TGroundBool;
  i: Integer;
  ActiveClause: TClause;

begin
// p <=> l_1 \land l_2 \land \cdots l_n;

  pV:= GetValue (GetVar (p));
  ActiveClause:= FTopConstraint;

  case Pv of
   gbFalse:
   begin
   //\lnot l_1\lor \lnor l_2 \lor \cdots \lor \lnot l_n
     for i:= 0 to ActiveClause.Count- 1 do
       ActiveClause.Item [i]:= NegateLiteral (ActiveClause.Item [i]);
     SubmitClause;

   end;

   gbTrue:
   begin
   //l_1\land l_2 \land \cdots \land \l_n
     for i:= 0 to ActiveClause.Count- 1 do
     begin
       BeginConstraint;
       AddLiteral (ActiveClause.Item [i]);
       SubmitClause;

     end;

     AbortConstraint;

   end;

   gbUnknown:
   begin

     for i:= 0 to ActiveClause.Count- 1 do
     begin
       BeginConstraint;
       AddLiteral (ActiveClause.Item [i]);
       AddLiteral (NegateLiteral (p));
       SubmitClause;
       ActiveClause.Item [i]:= NegateLiteral (ActiveClause.Item [i]);

     end;
     AddLiteral (p);
     SubmitClause;

   end;

  end;

end;

function CompareLiteral (P1, P2: Pointer): Integer;
begin
  Exit (Integer (P1)- Integer (P2));

end;

procedure TSATSolverInterface.SubmitOrGate (p: TLiteral);
var
  pV: TGroundBool;
  i: Integer;
  ActiveClause: TClause;

begin
  
// p <=> l_1 \lor l_2 \lor \cdots l_n;

  pv:= GetValue (GetVar (p));
  ActiveClause:= FTopConstraint;
//  ActiveClause.Sort (@CompareLiteral);

  case Pv of
   gbFalse:
   begin
     if 0< NoOfLiteralInTopConstraint [gbTrue] then//Contradiction
     begin
       BeginConstraint;
       AddLiteral (p);
       SubmitClause;
       Exit;

     end;

   //\lnot l_1\land \lnot l_2 \land \cdots \land \lnot \l_n
     for i:= 0 to ActiveClause.Count- 1 do
     begin
       BeginConstraint;
       AddLiteral (NegateLiteral (ActiveClause.Item [i]));
       SubmitClause;

     end;

     AbortConstraint;

   end;

   gbTrue:
   begin
     if ActiveClause.Count= NoOfLiteralInTopConstraint [gbFalse] then//Contradiction
     begin
       BeginConstraint;
       AddLiteral (NegateLiteral (p));
       SubmitClause;
       Exit;

     end;


   //l_1\lor l_2 \lor \cdots \lor l_n
     SubmitClause;

   end;

   gbUnknown:
   begin
     if 0< NoOfLiteralInTopConstraint [gbTrue] then
     begin
       BeginConstraint;
       AddLiteral (p);
       SubmitClause;
       Exit;

     end;

     if ActiveClause.Count= NoOfLiteralInTopConstraint [gbFalse] then//Contradiction
     begin
       BeginConstraint;
       AddLiteral (NegateLiteral (p));
       SubmitClause;
       Exit;

     end;

     for i:= 0 to ActiveClause.Count- 1 do
     begin
       BeginConstraint;
       AddLiteral (NegateLiteral (ActiveClause.Item [i]));
       AddLiteral (p);
       SubmitClause;

     end;

     AddLiteral (NegateLiteral (p));
     SubmitClause;

   end;

  end;

end;

function TSATSolverInterface.GetResult: TSolverResult;
begin
  Result:= FSolverResult;

end;

constructor TSATSolverInterface.Create;
begin
  inherited Create;

  FClausesStack:= TStackOfClauses.Create;
  FTopConstraint:= nil;

end;

destructor TSATSolverInterface.Destroy;
begin
  FClausesStack.Free;
  FTopConstraint.Free;

  inherited Destroy;

end;

procedure TSATSolverInterface.ReportForcedVariables;
var
  i: Integer;

begin
  Write ('Forced Variable Status:');

  for i:= 1 to VarCount- 1 do
    if GetValue (i)<> gbUnknown then
    begin
      if GetValue (i)= gbTrue then
        Write ('x', i, ' ')
      else
        Write ('~x', i, ' ');

    end;
  WriteLn;

end;

procedure Finalize;

begin
{  Stream:= TMyTextStream.Create (
    TFileStream.Create (GetRunTimeParameterManager.OutputFilename, fmCreate));
  (GetSatSolver as TCNFCollection).SaveToFile (Stream);
  Stream.Free;
}

  GetSatSolver.Free;

end;

finalization

end.

