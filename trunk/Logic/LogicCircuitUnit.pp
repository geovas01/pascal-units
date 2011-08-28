unit LogicCircuitUnit;
{$mode objfpc}

interface
uses
  Classes, GenericCollectionUnit, ClauseUnit, SatSolverInterfaceUnit, CNFCollection, TSeitinVariableUnit;

type 
  TNodeKind= (nkAnd, nkOr, nkNot, nkVariable, nkNone);
  TVariable= Int64;
  
  TLogicCircuitNode= class;

  TNodes= specialize TGenericCollection<TLogicCircuitNode>;
  TVariables= specialize TGenericCollectionForBuiltInData<TVariable>;

  TLogicCircuitNode= class (TObject)
  private
    FNodeID: Int64;
    FNodeKind: TNodeKind;
    FChildren: TNodes;
    FVarIndex: Integer;
    FFreeVariables: TVariables;
    FCorrespondingLiteral: TLiteral;

    procedure AddChild (NewNode: TLogicCircuitNode);
    function GetChildCount: Integer;
    function GetChild (Index: Integer): TLogicCircuitNode;
    function GetFreeVariable (Index: Integer): TVariable; 
    function GetFreeVariables: TVariables; 
    function GetFreeVariablesCount: Integer;

    function GenerateCNF (VariableManager: TVariableManager): TLiteral;

  public
    property NodeID: Int64 read FNodeID;
    property NodeKind: TNodeKind read FNodeKind;
    property VarIndex: Integer read FVarIndex;
    property ChildCount: Integer read GetChildCount;
    property Child [Index: Integer]: TLogicCircuitNode read GetChild;
    property FreeVariables: TVariables read GetFreeVariables;
    property FreeVariable [Index: Integer]: TVariable read GetFreeVariable;
    property FreeVariablesCount: Integer read GetFreeVariablesCount;

    constructor Create (ID: Int64; Kind: TNodeKind);
    constructor Create (ID: Int64);
    destructor Destroy; override;

    function ToXML (Indent: AnsiString= ''): AnsiString;

  end;

  TLogicCircuit= class (TObject)
  private
    FRoot: TLogicCircuitNode;
    AllNodes: TNodes;

    function GetNodeByID (ID: Int64): TLogicCircuitNode;

  public
    property Root: TLogicCircuitNode read FRoot;
    property NodeByID [ID: Int64]: TLogicCircuitNode read GetNodeByID;

{
  The input format is as follows:
  NodeID NodeKind LeftChild ID RightChild ID
  NodeID NodeKind LeftChild ID RightChild ID
}
    procedure Load (StrList: TStringList);
    
    constructor Create; 
    destructor Destroy; override;

    function ToXML: AnsiString;
    function ToCNF: TClauseCollection;

    function Solve: TStringList;

  end;

implementation

uses
  SysUtils;

const
  NodeKindString: array [nkAnd..nkNone] of AnsiString=
   ('nkAnd', 'nkOr', 'nkNot', 'nkVariable', 'nkNone');

//---------------TLogicCircuitNode-------------------------------

constructor TLogicCircuitNode.Create (ID: Int64; Kind: TNodeKind);
begin
  inherited Create;

  FChildren:= TNodes.Create;
  FNodeKind:= Kind;
  FCorrespondingLiteral:= -1;
  FNodeID:= ID;
  FFreeVariables:= nil;

end;

constructor TLogicCircuitNode.Create (ID: Int64);
begin
  inherited Create;

  FChildren:= TNodes.Create;
  FNodeKind:= nkNone;
  FCorrespondingLiteral:= -1;
  FNodeID:= ID;
  FFreeVariables:= nil;

end;


destructor TLogicCircuitNode.Destroy;
begin
  FChildren.Clear;
  FChildren.Free;
  FFreeVariables.Free;

  inherited;

end;

function TLogicCircuitNode.ToXML (Indent: AnsiString): AnsiString;
var
  i: Integer;

begin
  if NodeKind= nkVariable then
  begin
    Result:= Indent+ '<'+ NodeKindString [NodeKind]+ ' VarIndex= "'+ IntToStr (VarIndex)+ '" CorrespondingLiteral= "';
    if FCorrespondingLiteral= -1 then
      Result+= 'Not Set"/>'#10
    else
      Result+= LiteralToString (FCorrespondingLiteral)+ '"/>'#10

  end
  else
  begin

    Result:= Indent+ '<'+ NodeKindString [NodeKind]+ ' ID= "'+ IntToStr (NodeID)+'" CorrespondingLiteral= "';
    if FCorrespondingLiteral= -1 then
      Result+= 'Not Set"/>'#10
    else
      Result+= LiteralToString (FCorrespondingLiteral)+ '"/>'#10;
 
    for i:= 0 to ChildCount- 1 do
      if Indent<> '' then
        Result+= Child [i].ToXML (Indent+ '  ')
      else
        Result+= Child [i].ToXML ('');
  
    Result+= Indent+ '</'+ NodeKindString [NodeKind]+ '>'#10;
 
  end;
 
end;

function TLogicCircuitNode.GetChild (Index: Integer): TLogicCircuitNode;
begin
  Result:= FChildren.Item [Index];

end;

function TLogicCircuitNode.GenerateCNF (VariableManager: TVariableManager): TLiteral;
var
  Literals: TLiteralCollection;
  i: Integer;

begin
  if FCorrespondingLiteral<> -1 then
    Exit (FCorrespondingLiteral);

  Literals:= TLiteralCollection.Create;

  case NodeKind of
    nkAnd:
    begin
      for i:= 0 to ChildCount- 1 do
        Literals.AddItem (Child [i].GenerateCNF (VariableManager));

      FCorrespondingLiteral:= VariableManager.CreateVariableDescribingAND (Literals);

    end;
    nkOr:
    begin
      for i:= 0 to ChildCount- 1 do
        Literals.AddItem (Child [i].GenerateCNF (VariableManager));

      FCorrespondingLiteral:= VariableManager.CreateVariableDescribingOR (Literals);

    end;
    nkNot:
    begin
      FCorrespondingLiteral:= NegateLiteral (Child [0].GenerateCNF (VariableManager));
    end;
  
    nkVariable:
    begin
      FCorrespondingLiteral:= CreateLiteral (FVarIndex, False);
//      CNFGenerator.AddLiteral (FCorrespondingLiteral);

    end;
 
  end;

  Result:= FCorrespondingLiteral;
  Literals.Free;
  
end;

procedure TLogicCircuitNode.AddChild (NewNode: TLogicCircuitNode);
begin
  FChildren.AddItem (NewNode);

end;

function TLogicCircuitNode.GetChildCount: Integer;
begin
  Exit (FChildren.Count);

end;

function TLogicCircuitNode.GetFreeVariable (Index: Integer): TVariable; 
begin
 Result:= FreeVariables.Item [Index];

end;

function CompareVariable (a, b: Pointer): Integer;
begin
  Result:= TLiteral (a)- TLiteral (b);

end;

function TLogicCircuitNode.GetFreeVariables: TVariables; 
var
  i, j: Integer;

begin
  if FFreeVariables= nil then
  begin
    FFreeVariables:= TVariables.Create;
    if NodeKind= nkVariable then
      FFreeVariables.AddItem (VarIndex)
    else
      for i:= 0 to ChildCount- 1 do
        FFreeVariables.AddAnotherCollection (Child [i].FreeVariables);

    FFreeVariables.Sort (@CompareVariable);
    j:= 0;

    for i:= 0 to FFreeVariables.Count- 1 do
      if FFreeVariables.Item [j]<> FFreeVariables.Item [i] then
      begin
        Inc (j);
        FFreeVariables.Item [j]:= FFreeVariables.Item [i];

      end;

    for i:= FFreeVariables.Count- 1 downto j+ 1 do
      FFreeVariables.Delete (i);
      

  end;

  Result:= FFreeVariables;

end;

function TLogicCircuitNode.GetFreeVariablesCount: Integer;
begin
  Result:= FreeVariables.Count;

end;


{ TLogicCircuit }

procedure TLogicCircuit.Load (StrList: TStringList);

  function LogicCircuitNodeLoad (S: AnsiString): TLogicCircuitNode;
  var
    RightChild, LeftChild: TLogicCircuitNode;
  
  begin
    
    S:= S+ ' ';

    Result:= GetNodeByID (StrToInt (Copy (S, 1, Pos (' ', S)- 1)));
    Delete (S, 1, Pos (' ', S));
  
    case UpperCase (Copy (S, 1, Pos (' ', S)- 1)) [1] of
      'A'://ND' then
      begin
        Delete (S, 1, Pos (' ', S));
        Result.FNodeKind:= nkAnd;
  
        LeftChild:= GetNodeByID (StrToInt (Copy (S, 1, Pos (' ', S)- 1)));
        Delete (S, 1, Pos (' ', S));
        RightChild:= GetNodeByID (StrToInt (Copy (S, 1, Pos (' ', S)- 1)));
        Delete (S, 1, Pos (' ', S));

        Result.AddChild (LeftChild);
        Result.AddChild (RightChild);
    
      end;
      'O':
      begin
        Delete (S, 1, Pos (' ', S));
        Result.FNodeKind:= nkOr;

        LeftChild:= GetNodeByID (StrToInt (Copy (S, 1, Pos (' ', S)- 1)));
        Delete (S, 1, Pos (' ', S));
        RightChild:= GetNodeByID (StrToInt (Copy (S, 1, Pos (' ', S)- 1)));
        Delete (S, 1, Pos (' ', S));

        Result.AddChild (LeftChild);
        Result.AddChild (RightChild);
  
      end;
      'N':
      begin
        Delete (S, 1, Pos (' ', S));
        Result.FNodeKind:= nkNot;
  
        Result.AddChild (GetNodeByID (StrToInt (Copy (S, 1, Pos (' ', S)- 1))));
    
      end;
   
      'V':
      begin
        Delete (S, 1, Pos (' ', S));
        Result.FNodeKind:= nkVariable;
        Delete (S, Length (S), 1);
        Delete (S, 1, Pos (' ', S));
        Assert (S [1]= 'v');
        Result.FVarIndex:= GetVariableManager.CreateNewVariable (vpNone, True);//StrToInt (Copy (S, 2, Length (S)- 1));
    
      end;
  
    end;
   
  end;
  
var
  i: Integer;
  S: AnsiString;

begin
  for i:= 0 to StrList.Count- 1 do
  begin
    S:= StrList.Strings [i]+ ' ';
    AllNodes.Add (TLogicCircuitNode.Create (StrToInt (Copy (S, 1, Pos (' ', S)- 1))));

  end;

  for i:= 0 to StrList.Count- 1 do
    LogicCircuitNodeLoad (StrList.Strings [i]);

  S:= StrList.Strings [0]+ ' ';
  AllNodes.Add (TLogicCircuitNode.Create (StrToInt (Copy (S, 1, Pos (' ', S)- 1))));
  FRoot:= GetNodeByID (StrToInt (Copy (S, 1, Pos (' ', S)- 1)));

end;

function TLogicCircuit.ToXML: AnsiString;
begin
  Result:= '<LogicCircuit>'+ #10;
  Result+= FRoot.ToXML ('  ');

  Result+= '</LogicCircuit>';

end;

constructor TLogicCircuit.Create;
begin
  inherited Create;

  FRoot:= nil;
  AllNodes:= TNodes.Create;

end;

destructor TLogicCircuit.Destroy;
begin
  AllNodes.Free; 

end;

function TLogicCircuit.ToCNF: TClauseCollection;
begin
  ReNewSatSolver (ssCNFCollection);
  Root.GenerateCNF (GetVariableManager);

  Result:= GetVariableManager.SatSolver.CNF.Copy;//CNFCollection.CNF.Copy;

end;

function TLogicCircuit.GetNodeByID (ID: Int64): TLogicCircuitNode;
var
  i: Integer;

begin
  Result:= nil;

  for i:= 0 to AllNodes.Count- 1 do
    if AllNodes.Item [i].NodeID= ID then
      Exit (AllNodes.Item [i]);

end;

function TLogicCircuit.Solve: TStringList;
var
  i: Integer;

begin
  ReNewSatSolver (ssMiniSatSolver);
  Root.GenerateCNF (GetVariableManager);

  Result:= TStringList.Create;

  if GetVariableManager.SatSolver.Solve (Root.FCorrespondingLiteral) then
    for i:= 0 to Root.FreeVariablesCount- 1 do
      if GetVariableManager.SatSolver.GetValueInModel (Root.FreeVariable [i])= gbTrue then
        Result.Add ('v'+ IntToStr (Root.FreeVariable [i]))
      else
        Result.Add ('~v'+ IntToStr (Root.FreeVariable [i]))
  else
    Result.Add ('No Solution!');

end;


begin
end.
