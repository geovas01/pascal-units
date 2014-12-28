unit TSeitinVariableUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClauseUnit, SatSolverInterfaceUnit, MyTypes,
     GenericCollectionUnit, StreamUnit;

type
  TName= Integer;

  TTseitinVariable= Integer;
  { TVariableManager }

  TVariableManager= class(TObject)
  private
    FSimulationMode: Boolean;
    FTrueVariable: TTseitinVariable;
    FTrueLiteral: TLiteral;
    FFalseLiteral: TLiteral;
    FLastUsedCNFIndex: Integer;
    FDecisionForNewVariable: Boolean;

    {
    Keep track of LastVariableIndex when the Variable Generator enter the simulation mode
    }
    SimulationModeStack: TIntegerCollection;
    {
    Keep track of SimulationMode when the Variable Generator enter/exit the simulation mode
    }
    SimulationModeStateStack: TIntegerCollection;

//    AndDicTree, OrDicTree: TClauseDicTree;

    function GetSatSolver: TSATSolverInterface; inline;


  public
    property TrueVariable: TTseitinVariable read FTrueVariable;
    property TrueLiteral: TLiteral read FTrueLiteral;
    property FalseLiteral: TLiteral read FFalseLiteral;
    property SatSolver: TSATSolverInterface read GetSatSolver;
    property LastUsedCNFIndex: Integer read FLastUsedCNFIndex;
    property DecisionForNewVariable: Boolean read FDecisionForNewVariable write FDecisionForNewVariable;
    property SimulationMode: Boolean read FSimulationMode;

    constructor Create;
    destructor Destroy; override;

    procedure CreateTrueVariable;
    function CreateNewVariable(VariablePolarity: TVariablePolarity= vpNone; Decide: Boolean= True): TTseitinVariable; inline;
//    function CreateNewVariable(VariablePolarity: TVariablePolarity; Decide: Boolean): TTseitinVariable;

    function CreateVariableDescribingAND(Literals: TLiteralCollection; Size: Integer= MaxInt; Simplify: Boolean= True): TLiteral;
    function CreateVariableDescribingAND(l1, l2: TLiteral; Simplify: Boolean= True): TLiteral;
    function CreateVariableDescribingOR(Literals: TLiteralCollection; Size: Integer= MaxInt; Simplify: Boolean= True): TLiteral;
    function CreateVariableDescribingOR(l1, l2: TLiteral; Simplify: Boolean= True): TLiteral;
    function CreateVariableDescribingXOR(l1, l2: TLiteral; Simplify: Boolean= True): TLiteral;
    function CreateVariableDescribingXOR(Literals: TLiteralCollection; Size: Integer= MaxInt; Simplify: Boolean= True): TLiteral;

    {
      Result:= ITE(s, t, f) means
      if(s)
        Result:= t
      else
        Result:= f
    }
    function CreateVariableDescribingITE(s, t, f: TLiteral): TLiteral;

    {
     its output, x, is the carry pin of a full-adder, i.e., x= l1+ l2+ l3>=2
    }
//    function CreateVariableDescribingFACarry(l1, l2, l3: TLiteral): TLiteral;

    procedure SetSimulationMode;
    procedure ResetSimulationMode;

//    procedure SetLastVariableIndex(Value: Integer);

    procedure DescribeAND(Literals: TLiteralCollection; ResultLit: TLiteral; Size: Integer= MaxInt);
    procedure DescribeAND(l1, l2: TLiteral; ResultLit: TLiteral);
    procedure DescribeOR(Literals: TLiteralCollection; ResultLit: TLiteral; Size: Integer= MaxInt);
    procedure DescribeOR(l1, l2: TLiteral; ResultLit: TLiteral);

    procedure DescribeXOR(Literals: TLiteralCollection; ResultLit: TLiteral; Size: Integer= MaxInt);
    procedure DescribeXOR(l1, l2: TLiteral; ResultLit: TLiteral);

    procedure DescribeITE(s, t, f: TLiteral; ResultLit: TLiteral);
//    procedure DescribeFACarry(l1, l2, l3: TLiteral; ResultLit: TLiteral);

  end;

  _TAssignments= specialize TGenericCollectionForBuiltInData<TGroundBool>;

  { TAssignments }

  TAssignments= class(_TAssignments)
  private
    FStream: TMyTextStream;
    function GetFalseVarCount: Integer;
    function GetTrueVarCount: Integer;
    function GetUnknownVarCount: Integer;

    procedure SetCount(AValue: Integer); override;

  public
    property TrueVarCount: Integer read GetTrueVarCount;
    property FalseVarCount: Integer read GetFalseVarCount;
    property UnknownVarCount: Integer read GetUnknownVarCount;

    function GetValue(Lit: TLiteral): TGroundBool;
    constructor Load(Stream: TStream);//Stream will be freed by destructor
    destructor Destroy; override;

    function ToString: AnsiString; override;

  end;

function GetVariableManager: TVariableManager; 
procedure Initialize;
procedure Finalize;

implementation
uses
  Math, ParameterManagerUnit;

var
  VariableManager: TVariableManager;

function GetVariableManager: TVariableManager;
begin
  Result:= VariableManager;

end;

{ TAssignments }

function TAssignments.GetFalseVarCount: Integer;
var
  i: Integer;

begin
  Result:= 0;
  for i:= 0 to Count- 1 do
    if Item[i]=  gbFalse then
      Inc(Result);

end;

function TAssignments.GetTrueVarCount: Integer;
var
  i: Integer;

begin
  Result:= 0;
  for i:= 0 to Count- 1 do
    if Item[i]=  gbTrue then
      Inc(Result);


end;

function TAssignments.GetUnknownVarCount: Integer;
var
  i: Integer;

begin
  Result:= 0;
  for i:= 0 to Count- 1 do
    if Item[i]=  gbUnknown then
      Inc(Result);


end;

procedure TAssignments.SetCount(AValue: Integer);
var
  l, i: Integer;

begin
  l:= Count;

  inherited SetCount(AValue);
  for i:= l to Count- 1 do
    Items[i]:= gbUnknown;

end;

function TAssignments.GetValue(Lit: TLiteral): TGroundBool;
var
  v: TVariable;

begin
  v:= GetVar(Lit);
  if Count<= v then
    Exit(gbUnknown);

  if IsNegated(Lit) then
    Result:= TGroundBool(2- Ord(Item[v]))
  else
    Result:= Item[v];

end;

constructor TAssignments.Load(Stream: TStream);
var
  S: AnsiString;
  i, n: Integer;
  Sign: Boolean;

begin
  inherited Create;

  FStream:= TMyTextStream.Create(Stream, True);
  S:= FStream.ReadLine;

  while(S= '') or(UpCase(S[1])<> UpCase('v')) do
    S:= FStream.ReadLine;

  i:= 2;
  while i<= Length(S) do
  begin

    while S[i]= ' ' do
    begin
      Inc(i);
      if Length(S)< i then
        break;

    end;

    if Length(S)< i then
      break;

    Sign:= True;
    if S[i]= '-' then
    begin
      Sign:= False;
      Inc(i);

    end;

    if S[i]= 'x' then
      Inc(i);
    n:= 0;
    while S[i]<> ' ' do
    begin
      n*= 10;
      n+= Ord(S[i])- 48;

      Inc(i);
      if Length(S)< i then
        break;

    end;

    if Count< n+ 1 then
      Count:= n+ 1;
    if Sign then
      Item[n]:= gbTrue
    else
      Item[n]:= gbFalse;

    Inc(i);
    if Length(S)< i then
      break;

  end;

end;

destructor TAssignments.Destroy;
begin
  FStream.Free;

  inherited Destroy;
end;

function TAssignments.ToString: AnsiString;
var
  i: Integer;

begin
  Result:= '';

  for i:= 0 to Count- 1 do
    if Item[i]= gbTrue then
      Result+= ' x'+ IntToStr(i)
    else if Item[i]= gbFalse then
      Result+= ' ~x'+ IntToStr(i)
    else
    Result+= ' ?x'+ IntToStr(i);

end;

{ TVariableManager }

function TVariableManager.GetSatSolver: TSATSolverInterface;
begin
  Result:= SatSolverInterfaceUnit.GetSatSolver;

end;

constructor TVariableManager.Create;
begin

  inherited Create;

  FLastUsedCNFIndex:= 0;
  FDecisionForNewVariable:= True;

  SimulationModeStack:= TIntegerCollection.Create;
  SimulationModeStateStack:= TIntegerCollection.Create;
  FSimulationMode:= False;

  CreateTrueVariable;
{  AndDicTree:= TClauseDicTree.Create;
  OrDicTree:= TClauseDicTree.Create;
}
end;

destructor TVariableManager.Destroy;
begin
{  AndDicTree.Free;
  OrDicTree.Free;
}

  SimulationModeStack.Free;
  SimulationModeStateStack.Free;

  inherited Destroy;

end;

procedure TVariableManager.CreateTrueVariable;
begin
  FTrueVariable:= CreateNewVariable(vpTrue, True);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(CreateLiteral(FTrueVariable, False));
  SatSolver.SubmitClause;

  FTrueLiteral:= CreateLiteral(FTrueVariable, False);
  FFalseLiteral:= CreateLiteral(FTrueVariable, True);

end;

function TVariableManager.CreateNewVariable(VariablePolarity: TVariablePolarity; Decide: Boolean): TTseitinVariable;
var
  Temp: Integer;

begin
  Inc(FLastUsedCNFIndex);
  Result:= LastUsedCNFIndex;


  if not SimulationMode then
  begin
    Temp:= GetSatSolver.GenerateNewVariable(VariablePolarity, Decide and DecisionForNewVariable);
    Assert(FLastUsedCNFIndex= Temp);

  end;

end;

function CompareLiteral(Item1, Item2: Pointer): Integer;
begin
  Result:= TLiteral(Item1)- TLiteral(Item2);

end;

function TVariableManager.CreateVariableDescribingAND(Literals: TLiteralCollection; Size: Integer; Simplify: Boolean): TLiteral;
var
  i, j: Integer;

begin
///  Literals.Sort(@CompareLiteral);

  if Simplify then
  begin
    j:= 0;
    for i:= 0 to Math.Min(Literals.Count, Size)- 1 do
      case SatSolver.GetLiteralValue(Literals.Item[i]) of
        gbTrue:;
        gbFalse:
          Exit(FalseLiteral);
        gbUnknown:
        begin
          Literals.Item[j]:= Literals.Item[i];
          Inc(j);
          {TODO: It can be improved. ..}

        end;

      end;

  end
  else
    j:= Math.Min(Literals.Count, Size);

  if j= 0 then
    Exit(TrueLiteral);
  if j= 1 then
    Exit(Literals.Item[0]);

{
  Result:= AndDicTree.Search(Literals, j);
  if Result.FRawValue= 0 then
  begin
}
  Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);

  DescribeAND(Literals, Result, j);

{
    AndDicTree.Insert(Literals, Result, j);

  end
  else
    WRiteLn('Reused!', Literals.ToString, ':', LiteralToString(Result));
}

end;

function TVariableManager.CreateVariableDescribingAND(l1, l2: TLiteral; Simplify: Boolean): TLiteral;
begin
  if Simplify then
  begin
    if SatSolver.GetLiteralValue(l1)= gbFalse then
      Exit(FalseLiteral)
    else if SatSolver.GetLiteralValue(l1)= gbTrue then
      Exit(l2)
    else if SatSolver.GetLiteralValue(l2)= gbFalse then
      Exit(FalseLiteral)
    else if SatSolver.GetLiteralValue(l2)= gbTrue then
      Exit(l1)
    else
    begin
      Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);

      DescribeAND(l1, l2, Result);

    end;

  end
  else
  begin
    Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
    DescribeAND(l1, l2, Result);

  end;


end;


function TVariableManager.CreateVariableDescribingOR(
  Literals: TLiteralCollection; Size: Integer; Simplify: Boolean): TLiteral;
var
  i, j: Integer;

begin
//  Literals.Sort(@CompareLiteral);

  j:= 0;
  for i:= 0 to Math.Min(Literals.Count, Size)- 1 do
    case SatSolver.GetLiteralValue(Literals.Item[i]) of
      gbTrue:
        Exit(TrueLiteral);
      gbFalse: ;
      gbUnknown:
      begin
        Literals.Item[j]:= Literals.Item[i];
        Inc(j);
        {TODO: It can be improved. ..}

      end;

    end;

  if j= 0 then
    Exit(FalseLiteral);
  if j= 1 then
    Exit(Literals.Item[0]);

{
  Result:= OrDicTree.Search(Literals, j);
  if Result.FRawValue= 0 then
  begin
}

  Result:= CreateLiteral(GetVariableManager.CreateNewVariable(vpNone, True), False);

  DescribeOR(Literals, Result, j);

{
    OrDicTree.Insert(Literals, Result, j);

  end
  else
    WRiteLn('Reused!', Literals.ToString, ':', LiteralToString(Result));
}
end;

function TVariableManager.CreateVariableDescribingOr(l1, l2: TLiteral; Simplify: Boolean): TLiteral;
var
  Temp: TLiteral;

begin
  if Simplify then
  begin
    if l2< l1 then
    begin
      Temp:= l1;
      l1:= l2;
      l2:= Temp;

    end;

    if SatSolver.GetLiteralValue(l1)= gbFalse then
      Exit(l2)
    else if SatSolver.GetLiteralValue(l1)= gbTrue then
      Exit(TrueLiteral)
    else if SatSolver.GetLiteralValue(l2)= gbFalse then
      Exit(l1)
    else if SatSolver.GetLiteralValue(l2)= gbTrue then
      Exit(TrueLiteral)
    else
    begin
      Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);

      DescribeOR(l1, l2, Result);

    end;

  end
  else
  begin
    Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);

    DescribeOR(l1, l2, Result);

  end;

end;

function TVariableManager.CreateVariableDescribingXOR(l1, l2: TLiteral; Simplify: Boolean): TLiteral;
begin
  if Simplify then
  begin
    if SatSolver.GetLiteralValue(l1)= gbFalse then
      Exit(l2)
    else if SatSolver.GetLiteralValue(l1)= gbTrue then
      Exit(NegateLiteral(l2))
    else if SatSolver.GetLiteralValue(l2)= gbFalse then
      Exit(l1)
    else if SatSolver.GetLiteralValue(l2)= gbTrue then
      Exit(NegateLiteral(l1))
    else
    begin
      Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);

      DescribeXOR(l1, l2, Result);

    end;

  end
  else
  begin
    Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);

    DescribeXOR(l1, l2, Result);

  end;


end;

function TVariableManager.CreateVariableDescribingXOR(
  Literals: TLiteralCollection; Size: Integer; Simplify: Boolean): TLiteral;
var
  i, j: Integer;

begin
  if Simplify then
  begin
//  Literals.Sort(@CompareLiteral);
  j:= 0;
  for i:= 0 to Math.Min(Literals.Count, Size)- 1 do
    case SatSolver.GetLiteralValue(Literals.Item[i]) of
      gbTrue:
        Exit(TrueLiteral);
      gbFalse: ;
      gbUnknown:
      begin
        Literals.Item[j]:= Literals.Item[i];
        Inc(j);
        {TODO: It can be improved. ..}

      end;

    end;
  end
  else
    j:= Math.Min(Literals.Count, Size)- 1;

  if j= 0 then
    Exit(FalseLiteral);
  if j= 1 then
    Exit(Literals.Item[0]);

{
  Result:= OrDicTree.Search(Literals, j);
  if Result.FRawValue= 0 then
  begin
}

  Result:= CreateLiteral(GetVariableManager.CreateNewVariable(vpNone, True), False);

  DescribeXOR(Literals, Result, j);

{
    OrDicTree.Insert(Literals, Result, j);

  end
  else
    WRiteLn('Reused!', Literals.ToString, ':', LiteralToString(Result));
}

end;

function TVariableManager.CreateVariableDescribingITE(s, t, f: TLiteral): TLiteral;
begin
  if SatSolver.GetLiteralValue(s)= gbTrue then
    Exit(t)
  else if SatSolver.GetLiteralValue(s)= gbFalse then
    Exit(f)
  else
  begin
    Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);

    DescribeITE(s, t, f, Result);

  end;

end;
{
function TVariableManager.CreateVariableDescribingFACarry(l1, l2, l3: TLiteral): TLiteral;
begin
  Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);

  DescribeFACarry(l1, l2, l3, Result);

end;
}

procedure TVariableManager.SetSimulationMode;
begin
  FSimulationMode:= True;
  SimulationModeStack.AddItem(FLastUsedCNFIndex);
  SimulationModeStateStack.AddItem(1);

end;

procedure TVariableManager.ResetSimulationMode;
begin
  FLastUsedCNFIndex:= SimulationModeStack.Item[0];

  SimulationModeStack.Delete(0);
  SimulationModeStateStack.Delete(0);
  FSimulationMode:=(SimulationModeStateStack.Count<> 0);

end;

procedure TVariableManager.DescribeAND(l1, l2: TLiteral; ResultLit: TLiteral);
begin
  if SimulationMode then
    Exit;

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(l1);
  SatSolver.AddLiteral(l2);

//  WriteLn('False=', SatSolver.NoOfLiteralInTopConstraint[gbFalse],
//           'True=', SatSolver.NoOfLiteralInTopConstraint[gbTrue],
//           'Unknown=', SatSolver.NoOfLiteralInTopConstraint[gbUnknown]);

  if 0< SatSolver.NoOfLiteralInTopConstraint[gbFalse] then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> FalseLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(NegateLiteral(ResultLit));
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint[gbUnknown]= 0 then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> TrueLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(ResultLit);
      SatSolver.SubmitClause;

    end;
    Exit;

  end;

  SatSolver.SubmitAndGate(ResultLit);

end;


procedure TVariableManager.DescribeAND(Literals: TLiteralCollection; ResultLit: TLiteral; Size: Integer);
var
  i: Integer;

begin
  if SimulationMode then
    Exit;

  SatSolver.BeginConstraint;
  for i:= 0 to Math.Min(Literals.Count, Size)- 1 do
    SatSolver.AddLiteral(Literals.Item[i]);

//  WriteLn('False=', SatSolver.NoOfLiteralInTopConstraint[gbFalse],
//           'True=', SatSolver.NoOfLiteralInTopConstraint[gbTrue],
//           'Unknown=', SatSolver.NoOfLiteralInTopConstraint[gbUnknown]);

  if 0< SatSolver.NoOfLiteralInTopConstraint[gbFalse] then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> FalseLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(NegateLiteral(ResultLit));
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint[gbUnknown]= 0 then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> TrueLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(ResultLit);
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint[gbUnknown]= 1 then
  begin
    SatSolver.AbortConstraint;

    for i:= 0 to Literals.Count- 1 do
      if SatSolver.GetLiteralValue(Literals.Item[i])= gbUnknown then
      begin    //ResultLit= Literals.Item[i]
        SatSolver.BeginConstraint;
        SatSolver.AddLiteral(ResultLit);
        SatSolver.AddLiteral(NegateLiteral(Literals.Item[i]));
        SatSolver.SubmitClause;

        SatSolver.BeginConstraint;
        SatSolver.AddLiteral(NegateLiteral(ResultLit));
        SatSolver.AddLiteral(Literals.Item[i]);
        SatSolver.SubmitClause;

      end;

    Exit;

  end;

  SatSolver.SubmitAndGate(ResultLit);

end;

procedure TVariableManager.DescribeOR(l1, l2: TLiteral; ResultLit: TLiteral);
begin
  if SimulationMode then
    Exit;

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(l1);
  SatSolver.AddLiteral(l2);

//  WriteLn('False=', SatSolver.NoOfLiteralInTopConstraint[gbFalse],
//           'True=', SatSolver.NoOfLiteralInTopConstraint[gbTrue],
//           'Unknown=', SatSolver.NoOfLiteralInTopConstraint[gbUnknown]);
  if 0< SatSolver.NoOfLiteralInTopConstraint[gbTrue] then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> TrueLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(ResultLit);
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint[gbUnknown]= 0 then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> FalseLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(NegateLiteral(ResultLit));
      SatSolver.SubmitClause;

    end;
    Exit;

  end;

  SatSolver.SubmitOrGate(ResultLit);

end;

procedure TVariableManager.DescribeXOR(Literals: TLiteralCollection;
  ResultLit: TLiteral; Size: Integer);
var
  i: Integer;

begin
  if SimulationMode then
    Exit;

  SatSolver.BeginConstraint;

  for i:= 0 to Literals.Count- 1 do
    SatSolver.AddLiteral(Literals.Item[i]);

  SatSolver.SubmitXOrGate(ResultLit);

end;

procedure TVariableManager.DescribeXOR(l1, l2: TLiteral; ResultLit: TLiteral);
begin
  if SimulationMode then
    Exit;

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(l1);
  SatSolver.AddLiteral(l2);

  SatSolver.SubmitXOrGate(ResultLit);

end;

procedure TVariableManager.DescribeITE(s, t, f: TLiteral; ResultLit: TLiteral);
begin
  if SimulationMode then
    Exit;

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(s);
  SatSolver.AddLiteral(t);
  SatSolver.AddLiteral(f);

  SatSolver.SubmitITEGate(ResultLit);

end;

{
procedure TVariableManager.DescribeFACarry(l1, l2, l3: TLiteral;
    ResultLit: TLiteral);
var
  i, j: Integer;
  Literals: TLiteralCollection;

begin
  if SimulationMode then
    Exit;

  {
  b & c=> x
  a& c=> x
  a& b=>x

  ~b & ~c=> ~x
  ~a & ~c=> ~x
  ~a & ~b=> ~x
  }

  Literals:= TLiteralCollection.Create(3, VariableManager.FalseLiteral);
  Literals.Item[0]:= l1;
  Literals.Item[1]:= l1;
  Literals.Item[2]:= l1;


  SatSolver.BeginConstraint;
  for i:= 0 to Literals.Count- 1 do
    SatSolver.AddLiteral(Literals.Item[i]);

  Literals.Free;
  SatSolver.SubmitFACarryGate(ResultLit);

end;
 }

procedure TVariableManager.DescribeOR(Literals: TLiteralCollection; ResultLit: TLiteral; Size: Integer);
var
  i: Integer;

begin
  if SimulationMode then
    Exit;

  SatSolver.BeginConstraint;
  for i:= 0 to Math.Min(Literals.Count, Size)- 1 do
    SatSolver.AddLiteral(Literals.Item[i]);

  if 0< SatSolver.NoOfLiteralInTopConstraint[gbTrue] then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> TrueLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(ResultLit);
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint[gbUnknown]= 0 then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> FalseLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(NegateLiteral(ResultLit));
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint[gbUnknown]= 1 then
  begin
    SatSolver.AbortConstraint;
    for i:= 0 to Literals.Count- 1 do
      if SatSolver.GetLiteralValue(Literals.Item[i])= gbUnknown then
      begin

        SatSolver.BeginConstraint;
        SatSolver.AddLiteral(ResultLit);
        SatSolver.AddLiteral(NegateLiteral(Literals.Item[i]));
        SatSolver.SubmitClause;

        SatSolver.BeginConstraint;
        SatSolver.AddLiteral(NegateLiteral(ResultLit));
        SatSolver.AddLiteral(Literals.Item[i]);
        SatSolver.SubmitClause;

      end;

    Exit;

  end;

  SatSolver.SubmitOrGate(ResultLit);

end;

{
procedure TVariableManager.SetLastVariableIndex(Value: Integer);
begin
  FLastUsedCNFIndex:= Value;

end;
}

procedure Initialize;
begin
  VariableManager:= TVariableManager.Create;

end;

procedure Finalize;
begin
  GetVariableManager.Free;

end;

finalization

end.

