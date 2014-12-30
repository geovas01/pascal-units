unit ProblemDescriptionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, PBConstraintUnit, TSeitinVariableUnit, ClauseUnit;

type
  TSpecMode= (smDecision, smOptimization, smWeightedOptimization);

  TConstraintCollection= specialize TGenericCollection<TPBConstraint>;
  TInputVariableCollection= specialize TGenericCollectionForBuiltInData<TTseitinVariable>;

  { TPBSpecification }

  TPBSpecification= class (TObject)
  private
    AllConstraints: TConstraintCollection;
    AllClauses: TClauseCollection;


  protected
    FSizeOfProduct, FSoftCount, FMinCost,
    FMaxCost, FSumCost: Integer;
    FInputVariables: TInputVariableCollection;
    FNonLinearVariableDescriptions: TList;
    FCorrespondingLinearVariables: TLiteralCollection;
    FVarCount, FConsCount, FProdCount: Integer;


    function GetConstraintCount: Integer; virtual;
    function GetConstraint (Index: Integer): TPBConstraint;  virtual;
    function GetInputVariable (Index: Integer): Integer; virtual;
    function GetSpecMode: TSpecMode; virtual;
    function GetVarCount: Integer; virtual;
    function GetClause (Index: Integer): TClause; virtual;
    function GetClauseCount: Integer; virtual;
    function GetAllClauses: TClauseCollection; virtual;
  public
    property NonLinearVariableDescriptions: TList read FNonLinearVariableDescriptions;
    property CorrespondingLinearVariables: TLiteralCollection read FCorrespondingLinearVariables;

    property ConstraintCount: Integer read GetConstraintCount;
    property Constraint[Index: Integer]: TPBConstraint read GetConstraint;
    property Clause[Index: Integer]: TClause read GetClause;
    property InputVariableCount: Integer read GetVarCount;
    property InputVariable[Index: Integer]: Integer read GetInputVariable;
    property CNFClauses: TClauseCollection read GetAllClauses;
    property SpecMode: TSpecMode read GetSpecMode;
    property ClauseCount: Integer read GetClauseCount;

    constructor Create  (VarCount, ConsCount, ProdCount,
                     SizeOfProduct, SoftCount, MinCost, MaxCost, SumCost: Integer;
                       InputVaribles: TInputVariableCollection;
                       _NonLinearVariableDescriptions: TList;
                       _CorrespondingLinearVariables: TLiteralCollection); overload;
    constructor Create (InputVaribles: TInputVariableCollection); overload;
    destructor Destroy; override;

    procedure AddNewConstraint (NewConstraint: TPBConstraint); virtual;
    procedure AddNewClause (NewClause: TClause); virtual;

    function ToXML: AnsiString; virtual;
    function ToString: AnsiString;
    procedure ReduceToSAT; virtual; abstract;

    function MergeAndSimplify: Boolean; virtual;

    procedure SetConstraint (Index: Integer; AConstraint: TPBConstraint);  virtual;
    procedure DeleteConstraint (Index: Integer);  virtual;

    function MaxVarInProblem: Integer;  virtual;
    procedure Finalize;  virtual;

    procedure DescribeNonLinearVariables; virtual;

  end;

implementation
uses
  SatSolverInterfaceUnit;

{ TPBSpecification }

function TPBSpecification.GetAllClauses: TClauseCollection;
begin
  Result:= AllClauses;

end;

function TPBSpecification.GetVarCount: Integer;
begin
  Result:= FVarCount;

end;

function TPBSpecification.GetClause (Index: Integer): TClause;
begin
  Result:= AllClauses.Item[Index];

end;

function TPBSpecification.GetClauseCount: Integer;
begin
  Result:= AllClauses.Count;

end;

function TPBSpecification.GetSpecMode: TSpecMode;
begin
  Result:= smDecision;

end;

function TPBSpecification.GetConstraint (Index: Integer): TPBConstraint;
begin
  assert (Index< ConstraintCount);

  Result:= AllConstraints.Item[Index];

end;

function TPBSpecification.GetConstraintCount: Integer;
begin
  Result:= AllConstraints.Count;

end;

function TPBSpecification.GetInputVariable (Index: Integer): Integer;
begin
  Result:= FInputVariables.Item[Index];

end;

constructor TPBSpecification.Create (VarCount, ConsCount, ProdCount,
  SizeOfProduct, SoftCount, MinCost, MaxCost, SumCost: Integer;
  InputVaribles: TInputVariableCollection;
  _NonLinearVariableDescriptions: TList;
  _CorrespondingLinearVariables: TLiteralCollection);
begin
  inherited Create;

  FVarCount:= VarCount;
  FConsCount:= ConsCount;
  FProdCount:= ProdCount;
  FSizeOfProduct:= SizeOfProduct;
  FSoftCount:= SoftCount;
  FMinCost:= MinCost;
  FMaxCost:= MaxCost;
  FSumCost:= SumCost;

  AllConstraints:= TConstraintCollection.Create;
  AllClauses:= TClauseCollection.Create;

  FInputVariables:= InputVaribles;
  FNonLinearVariableDescriptions:= _NonLinearVariableDescriptions;
  FCorrespondingLinearVariables:= _CorrespondingLinearVariables;

end;

constructor TPBSpecification.Create (InputVaribles: TInputVariableCollection);
begin
  inherited Create;

  FVarCount:= 0;
  FConsCount:= 0;
  FProdCount:= 0;
  FSizeOfProduct:= 0;
  FSoftCount:= 0;
  FMinCost:= 0;
  FMaxCost:= 0;
  FSumCost:= 0;

  AllConstraints:= TConstraintCollection.Create;
  AllClauses:= TClauseCollection.Create;

  FNonLinearVariableDescriptions:= TList.Create;
  FCorrespondingLinearVariables:= TLiteralCollection.Create;

  FInputVariables:= InputVaribles;
  FVarCount:= FInputVariables.Count- 1;

end;

destructor TPBSpecification.Destroy;
var
  i: Integer;

begin
  AllConstraints.Free;
  FInputVariables.Free;

  for i:= 0 to NonLinearVariableDescriptions.Count- 1 do
    TLiteralCollection (NonLinearVariableDescriptions.Items[i]).Free;
  NonLinearVariableDescriptions.Clear;
  NonLinearVariableDescriptions.Free;

  CorrespondingLinearVariables.Free;
  AllClauses.Free;

  inherited Destroy;

end;

procedure TPBSpecification.AddNewConstraint (NewConstraint: TPBConstraint);
begin
  AllConstraints.AddItem (NewConstraint);

end;

procedure TPBSpecification.AddNewClause (NewClause: TClause);
begin
  AllClauses.AddItem (NewClause);

end;

function TPBSpecification.ToXML: AnsiString;
var
  i: Integer;

begin
   Result:= '<PBSpecification Mode= "'+ IntToStr (Ord (SpecMode))+ '" >';

   for i:= 0 to AllConstraints.Count- 1 do
     Result+= '<Constraint Weight= "-1">'+ Constraint[i].ToXML
             +'</Constraint>';

   Result+= '<NonLinearVariables>';
   {
   for i:= 0 to NonLinearVariableDescriptions.Count- 1 do
   begin
     for j:= 0 to NonLinearVariableDescriptions.Items[i].Count - 1 do
     Result+= '<NonLinearVariable Variable="'+ IntToStr (NonLinearVariableDescriptions.Items[].Item[i])+ '">'+ AllConstraints.Item[i].ToXML
             +'</Constraint>';

   end;
   }
   Result+= '</NonLinearVariables>';

   Result+= '</PBSpecification>';

end;

function TPBSpecification.ToString: AnsiString;
var
  i, j: Integer;
  ActiveClause: TLiteralCollection;

begin
  Result:= 'Problem:'#10;

  for i:= 0 to CorrespondingLinearVariables.Count- 1 do
  begin
    Result+= LiteralToString (CorrespondingLinearVariables.Items[i])+ '<=> ';

    Activeclause:= TLiteralCollection (NonLinearVariableDescriptions[i]);
    for j:= 0 to ActiveClause.Count- 1 do
      Result+= LiteralToString (ActiveClause.Items[j])+ ' ';
    Result+= #10;

  end;

  for i:= 0 to ClauseCount- 1 do
  begin
    Result+= Clause[i].ToString;
    Result+= #10;
    if i= 10 then
       break;

  end;

  for i:= 0 to ConstraintCount- 1 do
    Result+= Constraint[i].ToString+ #10;

  Result+= '.'#10;

end;

function TPBSpecification.MergeAndSimplify: Boolean;
var
  i: Integer;
{
TODO: more complicated Merge and simplify is possible!
}
  j: Integer;
  HCount: Integer;
  G: array of array of Boolean;

begin

  for i:= 0 to AllConstraints.Count- 1 do
    AllConstraints.Item[i].Finalize;

  HCount:= ConstraintCount;;
  SetLength (G, HCount);
  for i:= 0 to HCount- 1 do
    SetLength (G[i], HCount);

  for i:= 0 to HCount- 1 do
    for j:= 0 to HCount- 1 do
      if Constraint[i].IsWeaker (Constraint[j]) then
        G[i, j]:= True;

  Result:= False;
  for i:= 0 to HCount- 1 do
    SetLength (G[i], 0);
  SetLength (G, 0);

end;

procedure TPBSpecification.SetConstraint (Index: Integer; AConstraint: TPBConstraint);
begin
  AllConstraints.Item[Index]:= AConstraint;

end;

procedure TPBSpecification.DeleteConstraint (Index: Integer);
begin
  AllConstraints.Item[Index].Free;
  AllConstraints.Delete (Index);

end;

function TPBSpecification.MaxVarInProblem: Integer;
var
  i, j: Integer;
  ActiveClause: TClause;
  ActiveConstraint: TPBConstraint;

begin
  Result:= -1;

  for i:= 0 to CNFClauses.Count- 1 do
  begin
    ActiveClause:= CNFClauses.Item[i];

    for j:= 0 to ActiveClause.Count- 1 do
      if Result< GetVar (ActiveClause.Items[j]) then
        Result:= GetVar (ActiveClause.Items[j]);

  end;

  for i:= 0 to ConstraintCount- 1 do
  begin
    ActiveConstraint:= Constraint[i];

    for j:= 0 to ActiveConstraint.LHS.Count- 1 do
    begin
      if Result< GetVar (ActiveConstraint.LHS.item[j].Literal) then
        Result:= GetVar (ActiveConstraint.LHS.item[j].Literal);

    end;

  end;

end;

procedure TPBSpecification.Finalize;
var
  i: Integer;

begin
  for i:= 0 to ConstraintCount- 1 do
    Constraint[i].Finalize;

end;

procedure TPBSpecification.DescribeNonLinearVariables;
var
  i: Integer;
  ActiveLit: TLiteral;
  ActiveLiteralCollection: TLiteralCollection;

begin

  for i:= 0 to NonLinearVariableDescriptions.Count- 1 do
  begin
    ActiveLiteralCollection:= TLiteralCollection (NonLinearVariableDescriptions.Items[i]);
    ActiveLit:= CorrespondingLinearVariables.Items[i];

    GetVariableManager.DescribeAND (ActiveLiteralCollection, ActiveLit);

  end;

end;


end.

