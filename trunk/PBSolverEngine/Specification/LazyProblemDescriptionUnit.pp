unit LazyProblemDescriptionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProblemDescriptionUnit, PBParserUnit,
  PBConstraintUnit, ClauseUnit;

type
  { TLazyPBSpecification }

  TLazyPBSpecification= class (TPBSpecification)
  private
    LazyParser: TLazyPBParser;
    FClauseCount: Integer;
    LastReturnedConstraint: TPBConstraint;
    LastReturnedClause: TClause;
    LastReturnedConstraintPosition, LastReturnedConstraintIndex: Integer;
    LastReturnedClausePosition, LastReturnedClauseIndex: Integer;


  protected
    function GetConstraintCount: Integer; override;
    function GetConstraint (Index: Integer): TPBConstraint;  override;
    function GetClause (Index: Integer): TClause; override;
    function GetClauseCount: Integer; override;
    function GetAllClauses: TClauseCollection; override;

  public
    constructor Create  (VarCount, ConsCount, ProdCount,
                        SizeOfProduct, SoftCount, MinCost, MaxCost, SumCost: Integer;
                       InputVaribles: TInputVariableCollection;
                       _NonLinearVariableDescriptions: TList;
                       _CorrespondingLinearVariables: TLiteralCollection;
                       _LazyParser: TLazyPBParser
                     );

    constructor Create (InputVaribles: TInputVariableCollection;
                        _ClauseCount: Integer;
                        _ConsCount: Integer;
                        _LazyParser: TLazyPBParser);
    destructor Destroy; override;

{
    procedure AddNewConstraint (NewConstraint: TPBConstraint); virtual;
    procedure AddNewClause (NewClause: TClause); virtual;
    procedure AddNewCardConstraint (NewConstraint: TPBConstraint); virtual;
    procedure AddNewSoftConstraint (NewConstraint: TPBConstraint; Weight: Integer); virtual;

    function ToXML: AnsiString; virtual;
    function ToString: AnsiString;
    procedure ReduceToSAT; virtual; abstract;

    function MergeAndSimplify: Boolean; virtual;

    procedure SetConstraint (Index: Integer; AConstraint: TPBConstraint);  virtual;
    procedure DeleteConstraint (Index: Integer);  virtual;

    function MaxVarInProblem: Integer;  virtual;
    procedure Finalize;  virtual;

    procedure DescribeNonLinearVariables; virtual;
}
  end;

implementation

{ TLazyPBSpecification }

function TLazyPBSpecification.GetAllClauses: TClauseCollection;
begin
  Result:= nil;
  assert (False);
  raise Exception.Create ('Should not reach here!');

end;

function TLazyPBSpecification.GetConstraintCount: Integer;
begin
  Result:= FConsCount;

end;

function TLazyPBSpecification.GetConstraint (Index: Integer): TPBConstraint;
var
  i: Integer;

begin
  LastReturnedConstraint.Free;

  if LastReturnedConstraintIndex= -1 then
  begin
    LazyParser.ResetStream;
    LastReturnedConstraintPosition:= LazyParser.StreamPosition;

    Result:= LazyParser.ParseConstraint;

    for i:= 1 to Index do
    begin
      Result.Free;
      LastReturnedConstraintPosition:= LazyParser.StreamPosition;
      Result:= LazyParser.ParseConstraint;

    end;
    LastReturnedConstraintIndex:= Index;

  end
  else
  begin
    if Index< LastReturnedConstraintIndex then
    begin
      LastReturnedConstraintIndex:= -1;
      LastReturnedConstraint:= nil;
      Exit (GetConstraint (Index));

    end;

    LazyParser.StreamPosition:= LastReturnedConstraintPosition;
    Result:= nil;

    for i:= LastReturnedConstraintIndex to Index do
    begin
      Result.Free;

      LastReturnedConstraintPosition:= LazyParser.StreamPosition;
      Result:= LazyParser.ParseConstraint;

    end;
    LastReturnedConstraintIndex:= Index;

  end;

  LastReturnedConstraint:= Result;

end;

function TLazyPBSpecification.GetClause (Index: Integer): TClause;
var
  i: Integer;

begin
  LastReturnedClause.Free;

  if LastReturnedClauseIndex= -1 then
  begin
    LazyParser.ResetStream;
    LastReturnedClausePosition:= LazyParser.StreamPosition;

    Result:= LazyParser.ParseClause;

    for i:= 1 to Index do
    begin
      Result.Free;
      LastReturnedClausePosition:= LazyParser.StreamPosition;
      Result:= LazyParser.ParseClause;

    end;
    LastReturnedClauseIndex:= Index;

  end
  else
  begin
    if Index< LastReturnedClauseIndex then
    begin
      LastReturnedClauseIndex:= -1;
      LastReturnedClause:= nil;
      Exit (GetClause (Index));

    end;

    LazyParser.StreamPosition:= LastReturnedClausePosition;
    Result:= nil;

    for i:= LastReturnedClauseIndex to Index do
    begin
      Result.Free;

      LastReturnedClausePosition:= LazyParser.StreamPosition;
      Result:= LazyParser.ParseClause;

    end;
    LastReturnedClauseIndex:= Index;

  end;

  LastReturnedClause:= Result;

end;

function TLazyPBSpecification.GetClauseCount: Integer;
begin
  Result:= FClauseCount;

end;

constructor TLazyPBSpecification.Create (VarCount, ConsCount, ProdCount,
  SizeOfProduct, SoftCount, MinCost, MaxCost, SumCost: Integer;
  InputVaribles: TInputVariableCollection;
  _NonLinearVariableDescriptions: TList;
  _CorrespondingLinearVariables: TLiteralCollection;
   _LazyParser: TLazyPBParser);

begin
  inherited Create (VarCount, ConsCount, ProdCount,
              SizeOfProduct, SoftCount, MinCost, MaxCost, SumCost,
              InputVaribles, _NonLinearVariableDescriptions,
                _CorrespondingLinearVariables);

  FClauseCount:= 0;
  FConsCount:= ConsCount;
  Self.LazyParser:= _LazyParser;
  LastReturnedConstraint:= nil;
  LastReturnedClause:= nil;
  LastReturnedClauseIndex:= -1;
  LastReturnedConstraintIndex:= -1;


end;

constructor TLazyPBSpecification.Create(
  InputVaribles: TInputVariableCollection; _ClauseCount: Integer;
  _ConsCount: Integer; _LazyParser: TLazyPBParser);
begin
  inherited Create (InputVaribles);

  FClauseCount:= _ClauseCount;
  FConsCount:= _ConsCount;
  Self.LazyParser:= _LazyParser;
  LastReturnedConstraint:= nil;
  LastReturnedClause:= nil;
  LastReturnedClauseIndex:= -1;
  LastReturnedConstraintIndex:= -1;

end;


destructor TLazyPBSpecification.Destroy;
begin
  LastReturnedClause.Free;
  LastReturnedConstraint.Free;

  LazyParser.Free;

  inherited Destroy;

end;

end.

