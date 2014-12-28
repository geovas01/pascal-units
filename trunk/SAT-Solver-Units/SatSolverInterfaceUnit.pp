unit SatSolverInterfaceUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericStackUnit, ClauseUnit, MyTypes;

type
  TArrayofInteger = array [0..2] of Integer;
  TVariablePolarity = (vpFalse= 0, vpTrue, vpNone);

  TSolverResult = (srError, srSAT, srUNSAT);

  TSatSolverType = (ssMiniSatSolver, ssCNFCollection);

  { TSATSolverInterface }

  TSATSolverInterface = class (TObject)
  private
    type
      TNoOfLiteralsInTopConstraint = array [gbFalse..gbTrue] of Integer;
      TClauseNoOfLiteralsPair = specialize TPairForBuiltInData<TClause, TNoOfLiteralsInTopConstraint>;
      TStackOfClauses = specialize TGenericStack<TClauseNoOfLiteralsPair>;
  private
    FClausesStack: TStackOfClauses;
    FTopConstraint: TClause;
    FNoOfLiteralInTopConstraint: TNoOfLiteralsInTopConstraint;

    function GetNoOfLiteralInTopConstraint(gbValue: TGroundBool): Integer; inline;
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

    procedure AddComment(Comment: AnsiString); virtual;

//    function GenerateNewVariable(VariablePolrity: TVariablePolarity= vpNone; Decide: Boolean= True): Integer; virtual; abstract;
    function GenerateNewVariable(VariablePolrity: TVariablePolarity; Decide: Boolean): Integer; virtual; abstract;

    function BeginConstraint: TClause; inline;
    procedure AbortConstraint; virtual; 
    procedure AddLiteral(Lit: TLiteral); virtual; 
    procedure AddClause(AClause: TClause); inline;

    procedure SubmitAndGate(p: TLiteral); virtual;
    procedure SubmitOrGate(p: TLiteral); virtual;
    procedure SubmitXOrGate(p: TLiteral); virtual;
    procedure SubmitITEGate(p: TLiteral); virtual;
    procedure SubmitEquivGate(p: TLiteral); virtual;
    procedure SubmitClause; virtual;

    function GenerateAndGate: TLiteral; virtual;
    function GenerateOrGate: TLiteral; virtual;
    function GenerateXOrGate: TLiteral; virtual;

    function Solve: Boolean; virtual; abstract;
    function Solve(Literal: TLiteral): Boolean; virtual; abstract;
    procedure GetSolution(out Answer: AnsiString); virtual; abstract;
    function GetResult: TSolverResult;

    function GetLiteralValue(Lit: TLiteral): TGroundBool; inline;
    function GetValueInModel(v: Integer): TGroundBool; virtual;
    function GetLiteralValueInModel(Lit: TLiteral): TGroundBool; inline;
    procedure ImportModel(HighIndex: Integer; Model: TIntegerCollection); virtual;

    constructor Create;
    destructor Destroy; override;

    procedure ReportForcedVariables; virtual;

    procedure SetDecisionVar(Variable: Integer; SetFlag: Boolean); virtual; abstract;

  end;

function GetSatSolver: TSATSolverInterface;
function ReNewSatSolver(SatSolverType: TSatSolverType= ssMiniSatSolver): TSATSolverInterface;
procedure Initialize;
procedure Finalize;

implementation
uses
  TSeitinVariableUnit,  MiniSatSolverInterfaceUnit, CNFCollectionUnit,
  ParameterManagerUnit, CNFStreamUnit;

var
  SatSolverInterface: TSATSolverInterface;

function GetSatSolver: TSATSolverInterface;
begin
  Result:= SatSolverInterface;

end;

function ReNewSatSolver(SatSolverType: TSatSolverType): TSATSolverInterface; inline;
begin
  SatSolverInterface.Free;

  case SatSolverType of
    ssMiniSatSolver: SatSolverInterface:= TMiniSatSolverInterface.Create;
    ssCNFCollection:  SatSolverInterface:= TCNFCollection.Create
    else    ;

  end;

  Result:= SatSolverInterface;

  Result.BeginConstraint;
  Result.AddLiteral(GetVariableManager.TrueLiteral);
  Result.SubmitClause;

end;

procedure Initialize;
begin
  if UpperCase(GetRunTimeParameterManager.GetValueByName('--SatSolverType'))= UpperCase('CNFCollection') then
    SatSolverInterface:= TCNFCollection.Create
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--SatSolverType'))= UpperCase('CNFStream') then
    SatSolverInterface:= TCNFStream.Create(GetRunTimeParameterManager.GetValueByName('--OutputFilename'))
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--SatSolverType'))= UpperCase('InternalMiniSAT') then
    SatSolverInterface:= TMiniSatSolverInterface.Create
  else
  begin
    WriteLn('Invalid SatSolveType!');
    Halt(1);

{  if GetRunTimeParameterManager.SATSolverType= ssMiniSatSolver then
     SatSolverInterface:= TMiniSatSolverInterface.Create
  else if GetRunTimeParameterManager.SATSolverType= ssCNFCollection then
      SatSolverInterface:= TCNFCollection.Create;
}
  end;

end;

{ TSATSolverInterface }


procedure TSATSolverInterface.SyncInteractiveUPInfo;
begin
  raise Exception.Create('SyncInteractiveUPInfo');
 
end;

function TSATSolverInterface.GetValue(v: Integer): TGroundBool;
begin
  Exit(gbUnknown);

end;

function TSATSolverInterface.GetLiteralValue(Lit: TLiteral): TGroundBool; inline;
begin
  if IsNegated(Lit) then
    Exit(TGroundBool(2- Ord(GetValue(GetVar(Lit)))))
  else
    Exit(GetValue(GetVar(Lit)));

end;

function TSATSolverInterface.GetValueInModel(v: Integer): TGroundBool;
begin
  raise Exception.Create('GetValueInModel');
  Exit(gbUnknown);

end;

function TSATSolverInterface.GetLiteralValueInModel(Lit: TLiteral): TGroundBool;
begin
  if IsNegated(Lit) then
    Exit(TGroundBool(2- Ord(GetValueInModel(GetVar(Lit)))))
  else
    Exit(GetValueInModel(GetVar(Lit)));


end;

procedure TSATSolverInterface.ImportModel(HighIndex: Integer; Model: TIntegerCollection);
var
  v: Integer;

begin
  Model.Count:= HighIndex+ 1;

  for v:= 0 to HighIndex do
    if GetValueInModel(v)= gbTrue then
      Model.Item [v]:= 1
    else
      Model.Item [v]:= -1;

end;

function TSATSolverInterface.GetNoOfLiteralInTopConstraint(gbValue: TGroundBool): Integer;
begin
  Result:= FNoOfLiteralInTopConstraint[gbValue];

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
  Result:= nil;
  raise Exception.Create('GetCNF is not implemented here');

end;

procedure TSATSolverInterface.SubmitClause;
begin
  Inc(FClauseCount, 1);

  AbortConstraint;
  Inc(FClauseCount, 1);

end;

function TSATSolverInterface.GenerateAndGate: TLiteral;
begin
  if 0 < NoOfLiteralInTopConstraint [gbFalse] then
    Exit(GetVariableManager.FalseLiteral)
  else if NoOfLiteralInTopConstraint [gbTrue] = TopConstraint.Count then
    Exit(GetVariableManager.TrueLiteral)
  else
  begin
    Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SubmitAndGate(Result);

  end;

end;

function TSATSolverInterface.GenerateOrGate: TLiteral;
begin
  if 0 < NoOfLiteralInTopConstraint [gbTrue] then
    Exit(GetVariableManager.TrueLiteral)
  else if NoOfLiteralInTopConstraint [gbFalse] = TopConstraint.Count then
    Exit(GetVariableManager.FalseLiteral)
  else
  begin
    Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SubmitOrGate(Result);

  end;

end;

function TSATSolverInterface.GenerateXOrGate: TLiteral;
begin
  if TopConstraint.Count= 2 then
  begin
    if GetLiteralValue(TopConstraint.Item [0])= gbFalse then// False xor x => x
      Exit(TopConstraint.Item [1])
    else if GetLiteralValue(TopConstraint.Item [0])= gbFalse then// True xor x => ~x
      Exit(NegateLiteral(TopConstraint.Item [1]))
    else if GetLiteralValue(TopConstraint.Item [1])= gbFalse then// False xor x => x
        Exit(TopConstraint.Item [0])
    else if GetLiteralValue(TopConstraint.Item [1])= gbFalse then// True xor x => ~x
      Exit(NegateLiteral(TopConstraint.Item [0]))
    else
    begin
      Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
      SubmitXOrGate(Result);

    end;

  end
  else
  begin
    Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SubmitXOrGate(Result);

  end;

end;

procedure TSATSolverInterface.AddComment(Comment: AnsiString);
begin

end;

function TSATSolverInterface.BeginConstraint: TClause;
var
  Pair: TClauseNoOfLiteralsPair;

begin
  FTopConstraint:= TClause.Create(30);
  FillChar(FNoOfLiteralInTopConstraint, SizeOf(FNoOfLiteralInTopConstraint), 0);
  Pair:= TClauseNoOfLiteralsPair.Create(FTopConstraint, FNoOfLiteralInTopConstraint);
  Stack.Push(Pair);
  Result:= FTopConstraint;

end;

procedure TSATSolverInterface.AbortConstraint;
var
  Pair: TClauseNoOfLiteralsPair;

begin
  if Stack.Count> 0 then
  begin
    Pair:= Stack.Pop;

    Pair.First.Free;
    Pair.Free;
    FTopConstraint:= nil;

  end;

  if Stack.Count> 0 then
  begin
    Pair:= Stack.Top;
    FTopConstraint:= Pair.First;
    FNoOfLiteralInTopConstraint:= Pair.Second;

  end;

  Dec(FClauseCount);

end;

procedure TSATSolverInterface.AddLiteral(Lit: TLiteral);
var
  v: Integer;
  LiteralValue: TGroundBool;

begin
  v := GetVar(Lit);

  while VarCount <= v do
    GenerateNewVariable(vpNone, True);

  TopConstraint.AddItem(Lit);

  LiteralValue:= GetValue(v);
  if IsNegated(Lit) then
    LiteralValue := TGroundBool(2 - Ord(LiteralValue));

  Inc(FNoOfLiteralInTopConstraint[LiteralValue]);

end;

procedure TSATSolverInterface.AddClause(AClause: TClause); inline;
var
  i: Integer;

begin
  BeginConstraint;

  for i:= 0 to AClause.Count- 1 do
    AddLiteral(AClause.Item [i]);

  SubmitClause;

end;

procedure TSATSolverInterface.SubmitAndGate(p: TLiteral);
var
  pV: TGroundBool;
  i: Integer;
  ActiveClause: TClause;

begin
// p <=> l_1 \land l_2 \land \cdots l_n;

  pV:= GetValue(GetVar(p));
  ActiveClause:= FTopConstraint;

  case Pv of
   gbFalse:
   begin
   //\lnot l_1\lor \lnor l_2 \lor \cdots \lor \lnot l_n
     for i:= 0 to ActiveClause.Count- 1 do
       ActiveClause.Item [i]:= NegateLiteral(ActiveClause.Item [i]);
     SubmitClause;

   end;

   gbTrue:
   begin
   //l_1\land l_2 \land \cdots \land \l_n
     for i:= 0 to ActiveClause.Count- 1 do
     begin
       BeginConstraint;
       AddLiteral(ActiveClause.Item [i]);
       SubmitClause;

     end;

     AbortConstraint;

   end;

   gbUnknown:
   begin
     BeginConstraint;

     for i:= 0 to ActiveClause.Count- 1 do
     begin
       BeginConstraint;
       AddLiteral(ActiveClause.Item [i]);
       AddLiteral(NegateLiteral(p));
       SubmitClause;
       //ActiveClause.Item [i]:= NegateLiteral(ActiveClause.Item [i]);
       AddLiteral(NegateLiteral(ActiveClause.Item [i]));

     end;
     AddLiteral(p);
     SubmitClause;

     AbortConstraint;

   end;

  end;

end;

function CompareLiteral(P1, P2: Pointer): Integer;
begin
  Exit(Integer(P1)- Integer(P2));

end;

procedure TSATSolverInterface.SubmitOrGate(p: TLiteral);
var
  pV: TGroundBool;
  i: Integer;
  ActiveClause: TClause;

begin
  
// p <=> l_1 \lor l_2 \lor \cdots l_n;

  pv:= GetValue(GetVar(p));
  ActiveClause:= FTopConstraint;
//  ActiveClause.Sort(@CompareLiteral);

  case Pv of
   gbFalse:
   begin
     if 0< NoOfLiteralInTopConstraint[gbTrue] then//Contradiction
     begin
       BeginConstraint;
       AddLiteral(p);
       SubmitClause;
       AbortConstraint;

       Exit;

     end;

    //\lnot l_1\land \lnot l_2 \land \cdots \land \lnot \l_n
     BeginConstraint;
     for i:= 0 to ActiveClause.Count- 1 do
       AddLiteral(NegateLiteral(ActiveClause.Item [i]));
     SubmitClause;

     AbortConstraint;

   end;

   gbTrue:
   begin
     if ActiveClause.Count= NoOfLiteralInTopConstraint [gbFalse] then//Contradiction
     begin
       BeginConstraint;
       AddLiteral(NegateLiteral(p));
       SubmitClause;
       AbortConstraint;

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
       AddLiteral(p);
       SubmitClause;
       AbortConstraint;
       Exit;

     end;

     if ActiveClause.Count= NoOfLiteralInTopConstraint [gbFalse] then
     begin
       BeginConstraint;
       AddLiteral(NegateLiteral(p));
       SubmitClause;
       AbortConstraint;
       Exit;

     end;

     BeginConstraint;
     for i:= 0 to ActiveClause.Count- 1 do
     begin
       BeginConstraint;
       AddLiteral(NegateLiteral(ActiveClause.Item [i]));
       AddLiteral(p);
       SubmitClause;

       AddLiteral(ActiveClause.Item [i]);
//       ActiveClause.Item [i]:= ActiveClause.Item [i];

     end;

     AddLiteral(NegateLiteral(p));
     SubmitClause;
     AbortConstraint;

   end;

  end;

end;

procedure TSATSolverInterface.SubmitXOrGate(p: TLiteral);
var
  i, j: Integer;
  ActiveClause: TClause;
  Count: Integer;

begin

// p <=> l_1 \lxor l_2 \lxor ... ln;
  {
  ~l1, l2, p
  l1, ~l2, p
  ~l1,~l2, ~p
  l1, l2, ~p
  }

  ActiveClause:= FTopConstraint;

  for i:= 0 to(1 shl ActiveClause.Count)- 1 do
  begin

    BeginConstraint;
    Count:= 0;
    for j:= 0 to ActiveClause.Count- 1 do
      if(i and(1 shl j))= 0 then
      begin
        Inc(Count);
        AddLiteral(NegateLiteral(ActiveClause.Item[j]));
      end
      else
        AddLiteral(ActiveClause.Item [j]);

    if Count mod 2 = 1 then
      AddLiteral(p)
    else
      AddLiteral(NegateLiteral(p));

    SubmitClause;

  end;

  AbortConstraint;
end;

procedure TSATSolverInterface.SubmitITEGate(p: TLiteral);
var
  ActiveClause: TClause;
  s, t, f: TLiteral;

begin

// p <=>(s \land t) \lor(\lnot s \land f);
  {
  ~s, ~t, p
  ~s, t, ~p
  s,~f, p
  s, f, ~p
  ~t, ~f, p
  t, f, ~p
  }

  ActiveClause:= FTopConstraint;
  Assert(ActiveClause.Count= 3);

//  ActiveClause.Sort(@CompareLiteral);

  s:= ActiveClause.Item [0];
  t:= ActiveClause.Item [1];
  f:= ActiveClause.Item [2];

  BeginConstraint;
  AddLiteral(NegateLiteral(s));
  AddLiteral(NegateLiteral(t));
  AddLiteral(p);
  SubmitClause;

  BeginConstraint;
  AddLiteral(NegateLiteral(s));
  AddLiteral(t);
  AddLiteral(NegateLiteral(p));
  SubmitClause;

  BeginConstraint;
  AddLiteral(NegateLiteral(t));
  AddLiteral(NegateLiteral(f));
  AddLiteral(p);
  SubmitClause;

  BeginConstraint;
  AddLiteral(t);
  AddLiteral(f);
  AddLiteral(NegateLiteral(p));
  SubmitClause;

  BeginConstraint;
  AddLiteral(s);
  AddLiteral(NegateLiteral(f));
  AddLiteral(p);
  SubmitClause;

  BeginConstraint;
  AddLiteral(s);
  AddLiteral(f);
  AddLiteral(NegateLiteral(p));
  SubmitClause;

  AbortConstraint;

end;

procedure TSATSolverInterface.SubmitEquivGate(p: TLiteral);
var
  i: Integer;
  a, b: TLiteral;
begin
  Assert(TopConstraint.Count = 2);
  {p <=> a <-> b
  1) a \land b -> p
  2) ~a \land b -> ~p
  3) a \land ~b -> ~p
  4) ~a \land ~b -> p
  }
  a := TopConstraint.Item[0];
  b := TopConstraint.Item[1];

  for i := 0 to 3 do
  begin
    BeginConstraint;
    if (i and 1) = 0 then
      AddLiteral(NegateLiteral(a))
    else
      AddLiteral(a);
    if (i and 2) = 0 then
      AddLiteral(NegateLiteral(b))
    else
      AddLiteral(b);


    if i in [0, 3] then
      AddLiteral(p)
    else
      AddLiteral(NegateLiteral(p));

    SubmitClause;
  end;

  AbortConstraint;

end;

{
procedure TSATSolverInterface.SubmitFACarryGate(p: TLiteral);
var
  i, j: Integer;
  ActiveClause: TClause;

begin
  ActiveClause:= TopConstraint;
  Assert(ActiveClause.Count= 3);

  for i:= 0 to ActiveClause.Count - 1 do
    for j:= i+ 1 to ActiveClause.Count - 1 do
    begin
      BeginConstraint;

      AddLiteral(NegateLiteral(ActiveClause.Item [i]));
      AddLiteral(NegateLiteral(ActiveClause.Item [j]));
      AddLiteral(p);

      SubmitClause;

    end;


  for i:= 0 to ActiveClause.Count - 1 do
    for j:= i+ 1 to ActiveClause.Count - 1 do
    begin
      BeginConstraint;

      AddLiteral(ActiveClause.Item [i]);
      AddLiteral(ActiveClause.Item [j]);
      AddLiteral(NegateLiteral(p));

      SubmitClause;

    end;
  AbortConstraint;

end;
}

function TSATSolverInterface.GetResult: TSolverResult;
begin
  Result:= FSolverResult;

end;

constructor TSATSolverInterface.Create;
begin
  inherited Create;

  FClausesStack:= TStackOfClauses.Create;
  FTopConstraint:= nil;
  FVarCount:= 0;
  FClauseCount:= 0;

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
  Write('Forced Variable Status:');

  for i:= 1 to VarCount- 1 do
    if GetValue(i)<> gbUnknown then
    begin
      if GetValue(i)= gbTrue then
        Write('x', i, ' ')
      else
        Write('~x', i, ' ');

    end;
  WriteLn;

end;

procedure Finalize;

begin
{  Stream:= TMyTextStream.Create(
    TFileStream.Create(GetRunTimeParameterManager.OutputFilename, fmCreate));
 (GetSatSolver as TCNFCollection).SaveToFile(Stream);
  Stream.Free;
}

  GetSatSolver.Free;

end;

initialization

finalization

end.

