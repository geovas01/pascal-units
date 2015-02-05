unit PBConstraintUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BinarySearchTreeUnit, BigInt,
  ClauseUnit, MyTypes, GenericCollectionUnit, SatSolverInterfaceUnit;

type
  TSumMode=(smLinear, smNonLinear);

  TLiteralIntPair= specialize TPairForBuiltInData<TLiteral, TBigInt>;

  { TTerm }

  TTerm= class(TLiteralIntPair)
  private
    procedure SetCoef(NewValue: TBigInt); inline;
    procedure SetLiteral(NewValue: TLiteral); inline;

  public
    property Literal: TLiteral read First;
    property Coef: TBigInt read Second;

    function Copy: TTerm;
    constructor Create;
    constructor Create(lit: TLiteral; b: TBigInt);

    destructor Destroy; override;

  end;

  TPairBST= specialize TBSTree<TLiteral, TBigInt>;

  TPBSumList= specialize TGenericCollection<TTerm>;
  { TPBSum }

  TPBSum= class(TPBSumList)
  private
//    AllTermsInBST: TPairBST;
    FConstantTerm: TBigInt;
    Finalized: Boolean;

    function GetCoef(Index: Integer): TBigInt;
    function GetLiteral(Index: Integer): TLiteral;
    procedure SetConstantTerm(AnInteger: TBigInt);

  public
    property IsFinalized: Boolean read Finalized;
    property Coef [Index: Integer]: TBigInt read GetCoef;
    property Literal [Index: Integer]: TLiteral read GetLiteral;
    property ConstantTerm: TBigInt read FConstantTerm;

    constructor Create;
    destructor Destroy; override;

    function ToXML: AnsiString; virtual;
    function ToString: AnsiString; override;

    {After finalization, there cannot be any change in the constraint.
      The Sum will be changed to \sum c_i l_i- C where all constants are positive integers.
      It returns Self.}
    function Finalize: TPBSum; virtual;

    {Adds a new term to constraint}
    procedure AddNewTerm(Term: TTerm);

    {Creates a copy}
    function Copy: TPBSum;

    {
      Divides all the coefficients by n and returns a new TPBSum
    }
    function Divide(n: Int64): TPBSum;
    {
      Divides all the coefficients by n and returns a new TPBSum
    }
    function Divide(n: TBigInt): TPBSum;

    {
      Returns the value of Sum under active model.
    }
    function Evaluate(CNFGenerator: TSATSolverInterface; var IsPositive: Boolean): TBigInt;

    {
      Returns nil if Lit is not used in Sum and appropriate BigInt otherwise.
    }
    function GetCoefByLiteral(Lit: TLiteral): TBigInt;

    {
      Returns a new TBigInt whose value is equal to sum of all the coefficients
    }
    function SumOfCoefs: TBigInt;

  end;

  { TLinearPBSum }
{
  TLinearPBSum= class(TPBSum)
  private
  public

    function ToXML: AnsiString; override;
    procedure Finalize; override;

  end;
}
  { TNonLinearPBSum }
{
  TNonLinearPBSum= class(TPBSum)
  private

  public

    function ToXML: AnsiString; override;
    procedure AddTerm(c: AnsiString; Literals: TStringList); override;
    procedure Finalize; override;

  end;
}

  TComparisionOperator=(coEquality, coGreaterThanOrEqual, coLessThanOrEqual);

  { TPBConstraint }

  TPBConstraint= class(TObject)
  private
    FCompareOperator: TComparisionOperator;
    FLHS: TPBSum;
    FRHS: TBigInt;
    FRHSSign: Boolean;
    Finalized: Boolean;
    FSimplificationOf: TPBConstraint;
    procedure SetSimplificationOf(AValue: TPBConstraint);

  public
    property CompareOperator: TComparisionOperator read FCompareOperator;
    property LHS: TPBSum read FLHS;
    property RHS: TBigInt read FRHS;
    {RHSSign is true iff RHS is positive}
    property RHSSign: Boolean read FRHSSign;
    property SimplificationOf: TPBConstraint read FSimplificationOf write SetSimplificationOf;

    function ToXML: AnsiString; virtual;
    function ToString: AnsiString; override;
    constructor Create;
    constructor Create(Sum: TPBSum; ComparisionOperator:
               TComparisionOperator; _RHSSign: Boolean; RHSValue: TBigInt);
    constructor Create(Sum: TPBSum; ComparisionOperator: AnsiString;
                     _RHSSign: Boolean; RHSValue: TBigInt);
    destructor Destroy; override;

    procedure Finalize;
    function IsWeaker(AnotherConstraint: TPBConstraint): Boolean; virtual;

    function Copy: TPBConstraint;

  end;

implementation
uses
  ParameterManagerUnit;

{ TPBConstraint }

function CompareInteger(const a, b: Integer): Integer;
begin
  Exit(a- b);

end;

function CompareTerms(Item1, Item2: Pointer): Integer;
begin
  Exit(TTerm(Item1).First- TTerm(Item2).First);

end;

{
procedure TLinearPBSum.Finalize;

  procedure BuildBST(Start, Fin: Integer);
  begin
    
    if Start= Fin then
      AllTermsInBST.Add(TTerm(Self.Items [Start]).First,
                         TTerm(Self.Items [Start]).Second)
    else
    begin
      BuildBST(Start,(Start+ Fin) div 2);
      BuildBST((Start+ Fin) div 2+ 1, Fin);

    end;

  end;

begin
  Finalized:= True;

  Self.Sort(@CompareTerms);

  BuildBST(1, Self.Count);
  assert(AllTermsInBST.Root^.ChildrenCount+ 1= Self.Count);

end;
}

{ TPBConstraint }
const
  CompareOperatorString: array [coEquality..coLessThanOrEqual] of AnsiString=
    ('=', '>=', '<=');

procedure TPBConstraint.SetSimplificationOf(AValue: TPBConstraint);
begin
  if AValue.SimplificationOf<> nil then
    SimplificationOf:= AValue.SimplificationOf
  else
    FSimplificationOf:= AValue;

end;

function TPBConstraint.ToXML: AnsiString;

begin
  Finalize;

  Result:= '<PBConstraint Operator= "'+ CompareOperatorString [CompareOperator]+
    '" Finalized= "'+ BoolToStr(Finalized, True)+ '" RHSSign= "'+ BoolToStr(RHSSign, True)+ '" >';
  Result+= LHS.ToXML;
  Result+= '<RHS v= "'+ RHS.ToString+ '"/>';
  if SimplificationOf<> nil then
    Result+= '<SimplificationOf>'+ SimplificationOf.ToXML+ '</SimplificationOf>';

  Result+= '</PBConstraint>';

end;

function TPBConstraint.ToString: AnsiString;
begin
  Result:= LHS.ToString+ CompareOperatorString [CompareOperator];
  if RHSSign then
    Result+= RHS.ToString
  else
    Result+= '-'+ RHS.ToString

end;


constructor TPBConstraint.Create;
begin
  inherited Create;

end;

constructor TPBConstraint.Create(Sum: TPBSum;
       ComparisionOperator: TComparisionOperator; _RHSSign: Boolean;
       RHSValue: TBigInt);
begin
  inherited Create;

  if ComparisionOperator= coGreaterThanOrEqual then
    Create(Sum, '>=', _RHSSign, RHSValue)
  else if ComparisionOperator= coEquality then
    Create(Sum, '=', _RHSSign, RHSValue)
  else if ComparisionOperator= coLessThanOrEqual then
    Create(Sum, '<=', _RHSSign, RHSValue)
  else
    Halt(1);

end;

constructor TPBConstraint.Create(Sum: TPBSum; ComparisionOperator: AnsiString; _RHSSign: Boolean; RHSValue: TBigInt);
begin
  inherited Create;

  FLHS:= Sum;
  FRHS:= RHSValue;
  FRHSSign:= _RHSSign;
  Finalized:= False;
  FSimplificationOf:= nil;

  if ComparisionOperator= '>=' then
    FCompareOperator:= coGreaterThanOrEqual
  else if ComparisionOperator= '=' then
    FCompareOperator:= coEquality
  else if ComparisionOperator= '<=' then
  begin
    Assert(Sum.ConstantTerm.IsZero);

    FCompareOperator:= coLessThanOrEqual;
  end
  else
  begin
    Assert(False, 'Invalid Comparison Operator!');
    Halt(1);

  end;

end;

destructor TPBConstraint.Destroy;
begin
  LHS.Free;
  BigIntFactory.ReleaseMemeber(RHS);
  FSimplificationOf:= nil;

  inherited Destroy;

end;

procedure TPBConstraint.Finalize;
  function GCD(a, b: Int64): Int64;
  var
    c: Integer;

  begin
    if a< b then
      Exit(GCD(b, a));
    if b= 0 then
      Exit(1);

    Result:= b;
    while(a mod b)<> 0 do
    begin
      Result:= a mod b;
      a:= b;
      b:= Result;

    end;

  end;

var
  NewRHSValue: TBigInt;
  NewRHSSign: Boolean;
  NewLHS: TPBSum;
  LHSConstantTerm: TBigInt;
  GcdLeft: TBigInt;
  i: Integer;
  Temp: TBigInt;
  ActiveTerm: TTerm;

begin
  if Finalized then
    Exit;

  LHS.Finalize;
  {
   So now, the LHS is
     Sum_i P_i l_i- C
    where p_i and c are positive
  }

  LHSConstantTerm:= LHS.ConstantTerm;

  NewRHSValue:= nil;

  if RHSSign then// RHS is positive
  begin//RHSValue= C+ RHC
    NewRHSValue:= RHS.Copy.Add(LHSConstantTerm);
    NewRHSSign:= True;

  end
  else//// RHS is negative
  begin//RHSValue:= C- RHS
   
    if LHS.ConstantTerm.CompareWith(RHS)< 0 then//c< RHS
    begin
      NewRHSValue:= RHS.Copy.Sub(LHS.ConstantTerm);
      NewRHSSign:= False;

    end
    else//RHS< c
    begin
      NewRHSValue:= LHS.ConstantTerm.Copy.Sub(RHS);
      NewRHSSign:= True;

    end

  end;

  BigIntFactory.ReleaseMemeber(FRHS);
  FRHS:= NewRHSValue;
  FRHSSign:= NewRHSSign;

  LHS.ConstantTerm.SetValue(0);

  if LHS.Count<> 0 then
  begin
    GcdLeft:= LHS.Item [0].Coef.Copy;
    for i:= 1 to LHS.Count- 1 do
    begin
      Temp:= GcdLeft.gcd(LHS.Item [i].Coef);
      BigIntFactory.ReleaseMemeber(GcdLeft);
      GcdLeft:= Temp;

    end;

    if not RHS.IsZero then
    begin
      Temp:= GcdLeft.gcd(RHS);
      BigIntFactory.ReleaseMemeber(GcdLeft);
      GcdLeft:= Temp;

    end;


    if(GcdLeft.Length<> 1) or(GcdLeft.Digits [0]<> 1) then
    begin
      NewLHS:= LHS.Divide(GcdLeft);
      FLHS.Free;
      FLHS:= NewLHS;
      LHS.Finalize;

      Temp:= RHS.Divide(GcdLeft);
      BigIntFactory.ReleaseMemeber(RHS);
      FRHS:= Temp;

    end;
    BigIntFactory.ReleaseMemeber(GCDLeft);

  end;

  Finalized:= True;

end;

function TPBConstraint.IsWeaker(AnotherConstraint: TPBConstraint): Boolean;
begin
  Exit(False);

end;

function TPBConstraint.Copy: TPBConstraint;
begin
  inherited Create;

  Result:= TPBConstraint.Create(LHS.Copy, CompareOperator, RHSSign, RHS.Copy);

end;

{ TPBSum }

function TPBSum.GetCoef(Index: Integer): TBigInt;
begin
  Assert(Index>= 0);

  Result:= Self.Item [Index].Coef;

end;

procedure TPBSum.AddNewTerm(Term: TTerm);
begin
  Finalized:= False;
  Self.AddItem(Term);

end;

function TPBSum.Copy: TPBSum;
var
  i: Integer;

begin
  Result:= TPBSum.Create;

  for i:= 0 to Count- 1 do
    Result.AddItem(TTerm(Items [i]).Copy);

  BigIntFactory.ReleaseMemeber(Result.FConstantTerm);
  Result.FConstantTerm:= Self.ConstantTerm.Copy;

end;

function TPBSum.Divide(n: Int64): TPBSum;
var
  i: Integer;
  nBigInt: TBigInt;

begin
  nBigInt:= BigIntFactory.GetNewMember.SetValue(n);

  Result:= TPBSum.Create;

  for i:= 0 to Count- 1 do
    Result.AddItem(TTerm.Create(Self.Item [i].Literal, Self.Item [i].Coef.Divide(nBigInt)));
  Result.FConstantTerm:= ConstantTerm.Divide(nBigInt);
  nBigInt.Free;

end;

function TPBSum.Divide(n: TBigInt): TPBSum;
var
  i: Integer;

begin
  Result:= TPBSum.Create;

  for i:= 0 to Count- 1 do
    Result.AddItem(TTerm.Create(Self.Item [i].Literal, Self.Item [i].Coef.Divide(n)));
  BigIntFactory.ReleaseMemeber(Result.ConstantTerm);
  Result.FConstantTerm:= ConstantTerm.Divide(n);

end;

function TPBSum.Evaluate(CNFGenerator: TSATSolverInterface; var IsPositive: Boolean): TBigInt;
var
  i: Integer;
  PositiveCoef, NegativeCoef: Int64;
  Temp: TBigInt;

begin
  Assert(Finalized);
  Result:= BigIntFactory.GetNewMember.SetValue(0);
  IsPositive:= True;

  for i:= 0 to Count- 1 do
    if CNFGenerator.GetValueInModel(Item [i].Literal)= gbTrue then
      Result.Add(Item [i].Coef);

  if ConstantTerm.CompareWith(Result)<= 0 then
    Result.Sub(ConstantTerm)
  else
  begin
    Temp:= Result.Copy;
    Result.Free;
    Result:= ConstantTerm.Copy.Sub(Temp);
    Temp.Free;
    IsPositive:= False;

  end;

end;

function TPBSum.GetCoefByLiteral(Lit: TLiteral): TBigInt;
var
  Bot, Top: Integer;
  Mid: Integer;

begin
  Assert(Finalized);

  Top:= Self.Count- 1;
  Bot:= 0;

  while Bot<= Top do
  begin
    Mid:=(Top+ Bot) div 2;

    if Literal [Mid]< Lit then
      Bot:= Mid+ 1
    else if Lit< Literal [Mid] then
      Top:= Mid- 1
    else
      Exit(Coef [Mid]);

  end;
  Result:= nil;

end;

function TPBSum.SumOfCoefs: TBigInt;
var
  i: Integer;

begin
  Assert(Finalized);
  Result:= BigIntFactory.GetNewMember.SetValue(0);

  for i:= 0 to Self.Count- 1 do
    Result.Add(Item [i].Coef);

end;

function TPBSum.GetLiteral(Index: Integer): TLiteral;
begin
  Result:= Item [Index].Literal;

end;

procedure TPBSum.SetConstantTerm(AnInteger: TBigInt);
begin
  Assert(AnInteger.IsZero);

  FConstantTerm:= AnInteger.Copy;

end;

function CompareLiterals(const l1, l2: TLiteral): Integer;
begin
  Exit(l1- l2);

end;

constructor TPBSum.Create;
begin
  inherited Create;

//  AllTermsInBST:= TPairBST.Create(@CompareLiterals);
  Finalized:= False;

  FConstantTerm:= BigIntFactory.GetNewMember.SetValue(0);

end;

destructor TPBSum.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    Item[i].Free;

//  AllTermsInBST.Free;
  BigIntFactory.ReleaseMemeber(FConstantTerm);
  Clear;

  inherited Destroy;

end;

function TPBSum.ToXML: AnsiString;
var
  i: Integer;

begin
  Result:=  '<LinearSum Finalized= "'+  BoolToStr(Finalized, True)+ '" >';

  for i:= 0 to Count- 1 do
    Result+= '<Term>'+ '<Variable Lit= "'+ LiteralToString(Literal [i])+
        '"/>'+ '<Coef v="'+ Coef [i].ToString+ '"/>'+
          '</Term>';
  Result+= '<ContantTerm V= "'+ ConstantTerm.ToString+ '"/>';
  Result+= '</LinearSum>';

end;

function TPBSum.ToString: AnsiString;
var
  i: Integer;

begin
  if not Finalized then
    Finalize;

  Result:= '';
  if 0< Count then
  begin
    for i:= 0 to Self.Count- 2 do
      Result+= Coef [i].ToString+ ' '+ LiteralToString(Literal [i])+ '+';
    i:= Self.Count- 1;
    Result+= Coef [i].ToString+ ' '+ LiteralToString(Literal [i]);

  end;

  Result+= '-'+ ConstantTerm.ToString;

end;

function TPBSum.Finalize: TPBSum;

{  procedure BuildBST(Start, Fin: Integer);
  begin
    if Start= Fin then
      AllTermsInBST.Add(TTerm(Self.Items [Start]).First,
                         TTerm(Self.Items [Start]).Second)
    else
    begin
      BuildBST(Start,(Start+ Fin) div 2);
      BuildBST((Start+ Fin) div 2+ 1, Fin);

    end;

  end;
}
var
  i: Integer;

begin
  if Finalized then
    Exit(Self);

  Finalized:= True;

{
  for i:= 0 to Self.Count- 1 do
    if Self.Item [i].Coef< 0 then
    begin
      Self.Item [i].SetCoef(-Self.Item [i].Coef);
      Self.Item [i].SetLiteral(NegateLiteral(Self.Item [i].Literal));
      FConstantTerm+= Self.Item [i].Coef;

    end
    else if Self.Item [i].Ceof= 0 then
      Delete(i);
}
  for i:= Self.Count- 1 downto 0 do
    if Self.Item [i].Coef.IsZero then
    begin
      Self.Item [i].Free;
      Self.Delete(i);

    end;

  Self.Sort(@CompareTerms);
  if Self.Count= 0 then
    Exit(Self);

{  BuildBST(0, Self.Count- 1);}
//  assert(AllTermsInBST.Root^.ChildrenCount+ 1= Self.Count);
  Result:= Self;

end;

{ TTerm }

procedure TTerm.SetCoef(NewValue: TBigInt); inline;
begin
  Second:= NewValue;

end;

procedure TTerm.SetLiteral(NewValue: TLiteral); inline;
begin
  First:= NewValue;

end;

constructor TTerm.Create;
begin
  inherited Create(0, nil);

end;

constructor TTerm.Create(lit: TLiteral; b: TBigInt);
begin
  inherited Create(lit, b);

end;


function TTerm.Copy: TTerm;
begin
  Result:= TTerm.Create(First, Second.Copy);

end;

destructor TTerm.Destroy;
begin
  BigIntFactory.ReleaseMemeber(Coef);

  inherited Destroy;
end;

end.

