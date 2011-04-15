unit TSeitinVariableUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClauseUnit, SatSolverInterfaceUnit;//, ClauseDicTreeUnit;

type
  TName= Integer;

  TTseitinVariable= Integer;
  { TVariableManager }

  TVariableManager= class (TObject)
  private
    FTrueVariable: TTseitinVariable;
    FTrueLiteral: TLiteral;
    FFalseLiteral: TLiteral;
    FLastUsedCNFIndex: Integer;
    FDecisionForNewVariable: Boolean;

//    AndDicTree, OrDicTree: TClauseDicTree;

    function GetSatSolver: TSATSolverInterface; inline;


  public
    property TrueVariable: TTseitinVariable read FTrueVariable;
    property TrueLiteral: TLiteral read FTrueLiteral;
    property FalseLiteral: TLiteral read FFalseLiteral;
    property SatSolver: TSATSolverInterface read GetSatSolver;
    property LastUsedCNFIndex: Integer read FLastUsedCNFIndex;
    property DecisionForNewVariable: Boolean read FDecisionForNewVariable write FDecisionForNewVariable;

    constructor Create;
    destructor Destroy; override;

//    function CreateNewVariable (VariablePolarity: TVariablePolarity= vpNone; Decide: Boolean= True): TTseitinVariable;
    function CreateNewVariable (VariablePolarity: TVariablePolarity; Decide: Boolean): TTseitinVariable;
    function CreateVariableDescribingAND (Literals: TLiteralCollection; Size: Integer= MaxInt): TLiteral;
    function CreateVariableDescribingOR (Literals: TLiteralCollection; Size: Integer= MaxInt): TLiteral;

    procedure DescribeAND (Literals: TLiteralCollection; ResultLit: TLiteral; Size: Integer= MaxInt);
    procedure DescribeOR (Literals: TLiteralCollection; ResultLit: TLiteral; Size: Integer= MaxInt);

    procedure SetLastVariableIndex (Value: Integer);
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
  FTrueVariable:= CreateNewVariable (vpTrue, True);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral (CreateLiteral (FTrueVariable, False));
  SatSolver.SubmitClause;

  FTrueLiteral:= CreateLiteral (FTrueVariable, False);
  FFalseLiteral:= CreateLiteral (FTrueVariable, True);

{  AndDicTree:= TClauseDicTree.Create;
  OrDicTree:= TClauseDicTree.Create;
}
end;

destructor TVariableManager.Destroy;
begin
{  AndDicTree.Free;
  OrDicTree.Free;
}
  inherited Destroy;

end;

function TVariableManager.CreateNewVariable (VariablePolarity: TVariablePolarity; Decide: Boolean): TTseitinVariable;
begin
  Inc (FLastUsedCNFIndex);
  Result:= LastUsedCNFIndex;

  GetSatSolver.GenerateNewVariable (VariablePolarity, Decide and DecisionForNewVariable);

end;

function CompareLiteral (Item1, Item2: Pointer): Integer;
begin
  Result:= TLiteral (Item1)- TLiteral (Item2);

end;

function TVariableManager.CreateVariableDescribingAND (Literals: TLiteralCollection; Size: Integer): TLiteral;
var
  i, j: Integer;

begin
  Literals.Sort (@CompareLiteral);

  j:= 0;
  for i:= 0 to Math.Min (Literals.Count, Size)- 1 do
    case SatSolver.GetLiteralValue (Literals.Item [i]) of
      gbTrue:;
      gbFalse:
        Exit (FalseLiteral);
      gbUnknown:
      begin
        Literals.Item [j]:= Literals.Item [i];
        Inc (j);
        {TODO: It can be improved. ..}

      end;

    end;

  if j= 0 then
    Exit (TrueLiteral);
  if j= 1 then
    Exit (Literals.Item [0]);

{
  Result:= AndDicTree.Search (Literals, j);
  if Result.FRawValue= 0 then
  begin
}
    if GetRunTimeParameterManager.VarPolarityHeuristic then
      Result:= CreateLiteral (GetVariableManager.CreateNewVariable (vpTrue, True), False)
    else
      Result:= CreateLiteral (GetVariableManager.CreateNewVariable (vpNone, True), False);

    DescribeAND (Literals, Result, j);
{
    AndDicTree.Insert (Literals, Result, j);

  end
  else
    WRiteLn ('Reused!', Literals.ToString, ':', LiteralToString (Result));
}
end;

function TVariableManager.CreateVariableDescribingOR (Literals: TLiteralCollection; Size: Integer): TLiteral;
var
  i, j: Integer;

begin
  Literals.Sort (@CompareLiteral);

  j:= 0;
  for i:= 0 to Math.Min (Literals.Count, Size)- 1 do
    case SatSolver.GetLiteralValue (Literals.Item [i]) of
      gbTrue:
        Exit (TrueLiteral);
      gbFalse: ;
      gbUnknown:
      begin
        Literals.Item [j]:= Literals.Item [i];
        Inc (j);
        {TODO: It can be improved. ..}

      end;

    end;

  if j= 0 then
    Exit (FalseLiteral);
  if j= 1 then
    Exit (Literals.Item [0]);

{
  Result:= OrDicTree.Search (Literals, j);
  if Result.FRawValue= 0 then
  begin
}

  if GetRunTimeParameterManager.VarPolarityHeuristic then
    Result:= CreateLiteral (GetVariableManager.CreateNewVariable (vpFalse, True), False)
  else
    Result:= CreateLiteral (GetVariableManager.CreateNewVariable (vpNone, True), False);

  DescribeOR (Literals, Result, j);

{
    OrDicTree.Insert (Literals, Result, j);

  end
  else
    WRiteLn ('Reused!', Literals.ToString, ':', LiteralToString (Result));
}
end;

procedure TVariableManager.DescribeAND (Literals: TLiteralCollection; ResultLit: TLiteral; Size: Integer);
var
  i: Integer;

begin
  SatSolver.BeginConstraint;
  for i:= 0 to Math.Min (Literals.Count, Size)- 1 do
    SatSolver.AddLiteral (Literals.Item [i]);

//  WriteLn ('False=', SatSolver.NoOfLiteralInTopConstraint [gbFalse],
//           'True=', SatSolver.NoOfLiteralInTopConstraint [gbTrue],
//           'Unknown=', SatSolver.NoOfLiteralInTopConstraint [gbUnknown]);

  if 0< SatSolver.NoOfLiteralInTopConstraint [gbFalse] then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> FalseLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral (NegateLiteral (ResultLit));
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint [gbUnknown]= 0 then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> TrueLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral (ResultLit);
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint [gbUnknown]= 1 then
  begin
    SatSolver.AbortConstraint;

    for i:= 0 to Literals.Count- 1 do
      if SatSolver.GetLiteralValue (Literals.Item [i])= gbUnknown then
      begin    //ResultLit= Literals.Item [i]
        SatSolver.BeginConstraint;
        SatSolver.AddLiteral (ResultLit);
        SatSolver.AddLiteral (NegateLiteral (Literals.Item [i]));
        SatSolver.SubmitClause;

        SatSolver.BeginConstraint;
        SatSolver.AddLiteral (NegateLiteral (ResultLit));
        SatSolver.AddLiteral (Literals.Item [i]);
        SatSolver.SubmitClause;

      end;

    Exit;

  end;

  SatSolver.SubmitAndGate (ResultLit);

end;

procedure TVariableManager.DescribeOR (Literals: TLiteralCollection; ResultLit: TLiteral; Size: Integer);
var
  i: Integer;

begin
  SatSolver.BeginConstraint;
  for i:= 0 to Math.Min (Literals.Count, Size)- 1 do
    SatSolver.AddLiteral (Literals.Item [i]);

  if 0< SatSolver.NoOfLiteralInTopConstraint [gbTrue] then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> TrueLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral (ResultLit);
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint [gbUnknown]= 0 then
  begin
    SatSolver.AbortConstraint;
    if ResultLit<> FalseLiteral then
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral (NegateLiteral (ResultLit));
      SatSolver.SubmitClause;

    end;
    Exit;

  end
  else if SatSolver.NoOfLiteralInTopConstraint [gbUnknown]= 1 then
  begin
    SatSolver.AbortConstraint;
    for i:= 0 to Literals.Count- 1 do
      if SatSolver.GetLiteralValue (Literals.Item [i])= gbUnknown then
      begin

        SatSolver.BeginConstraint;
        SatSolver.AddLiteral (ResultLit);
        SatSolver.AddLiteral (NegateLiteral (Literals.Item [i]));
        SatSolver.SubmitClause;

        SatSolver.BeginConstraint;
        SatSolver.AddLiteral (NegateLiteral (ResultLit));
        SatSolver.AddLiteral (Literals.Item [i]);
        SatSolver.SubmitClause;

      end;

    Exit;

  end;

  SatSolver.SubmitOrGate (ResultLit);

end;

procedure TVariableManager.SetLastVariableIndex (Value: Integer);
begin
  FLastUsedCNFIndex:= Value;

end;

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

