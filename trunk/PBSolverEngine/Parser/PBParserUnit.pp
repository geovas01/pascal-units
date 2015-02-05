unit PBParserUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamUnit, ProblemDescriptionUnit, PBConstraintUnit,
   TSeitinVariableUnit, ClauseUnit;

type
  EInvalidSyntax= class (Exception);

  { TPBParser }

  TPBParser= class (TObject)
  private
    InputVariables: TInputVariableCollection;
    MyStream: TMyTextStream;
    LastPosition: Int64;
    NonLinearVariableDescriptions: TList;
    CorrespondingLinearVariables: TLiteralCollection;

    function Expect (S: AnsiString): Boolean;

    function GetNextChar: Char;
    function GetNextWord: AnsiString;
    procedure Rewind (Position: Int64);
    procedure Rewind;
    procedure ParseOneOrMoreSpace;
    procedure ParseZeroOrMoreSpace;

    function ParseOneCommentLine: AnsiString;

    procedure ParseInteger (out Int: AnsiString);
    procedure ParseUnsignedInt (out Int: AnsiString);
    function ParseSum: TPBSum;
    function ParseTerm: TStringList;

    function ParseVersion2: TPBSpecification; virtual;
    function ParseClause: TClause; virtual;
    function ParseConstraint: TPBConstraint; virtual;
    function ParseCardConstraint: TPBConstraint; virtual;

  public
    constructor Create (AnStream: TStream);
    {
      It should be called after descructor of TPBSpecification
    }
    destructor Destroy; override;

    function ParsePB: TPBSpecification; virtual;

  end;

  { TPBVersion2Parser }

  TPBVersion2Parser= class (TPBParser)
  public
    function ParsePB: TPBSpecification; override;

  end;

  { TLazyPBParser }

  TLazyPBParser= class (TPBParser)
  private
    Version: Integer;
    InputStream: TStringStream;

    function GetPosition: Int64;
    procedure SetPosition (AValue: Int64);

  public
    property StreamPosition: Int64 read GetPosition write SetPosition;

    constructor Create (AnStream: TStream);
    {
      ??
    }
    destructor Destroy; override;

    function ParsePB: TPBSpecification; override;

    function ParseConstraint: TPBConstraint; override;
    function ParseClause: TClause; override;
    procedure ResetStream;

  end;

  { TLazyVersion2PBParser }

  TLazyVersion2PBParser= class (TLazyPBParser)
  public
    //??
    destructor Destroy; override;

    function ParsePB: TPBSpecification; override;

    function ParseConstraint: TPBConstraint; override;
    function ParseClause: TClause; override;
    procedure ResetStream;

  end;


implementation
uses
  GenericCollectionUnit, BigInt, SatSolverInterfaceUnit,
  LazyProblemDescriptionUnit;

type
  EExpectationFailed= class (Exception);

{ TPBVersion2Parser }

function TPBVersion2Parser.ParsePB: TPBSpecification;
begin
  Result:= inherited ParsePB;

end;

{ TLazyVersion2PBParser }

destructor TLazyPBParser.Destroy;
begin
  InputStream.Free;

  inherited Destroy;

end;

function TLazyVersion2PBParser.ParsePB: TPBSpecification;
var
  FirstCommentLine: AnsiString;
  VarCount, ConsCount, ClauseCount, CardCount: Integer;
  Ch: Char;
  S: AnsiString;
  j, Position: Integer;
  v: Integer;

begin
  FirstCommentLine:= ParseOneCommentLine+ ' ';

  VarCount:= 0; ConsCount:= 0; ClauseCount:= 0; CardCount:= 0;

  FirstCommentLine:= ParseOneCommentLine+ ' ';
  Delete (FirstCommentLine, 1, Length ('* #variable= '));
  VarCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

  Delete (FirstCommentLine, 1, Length ('#clause= '));
  ClauseCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

  Delete (FirstCommentLine, 1, Length ('#cardinality= '));
  CardCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

  Delete (FirstCommentLine, 1, Length ('#constraint= '));
  ConsCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

  WriteLn ('c ', VarCount, ' ', ClauseCount, ' ', CardCount, ' ', ConsCount);

  InputVariables:= TInputVariableCollection.Create;
  InputVariables.AddItem (0);

  for j:= 1 to VarCount do
  begin
    v:= GetVariableManager.CreateNewVariable (vpNone, True);
    InputVariables.AddItem (v);//GetVariableManager.CreateNewVariable);

  end;
  GetVariableManager.CreateTrueVariable;

  Result:= TLazyPBSpecification.Create (InputVariables, ClauseCount, ConsCount,
    Self);

end;


function TLazyVersion2PBParser.ParseConstraint: TPBConstraint;
var
  Position: Integer;
  Line: AnsiString;

begin
  Position:= MyStream.Position;
  Line:= Trim (MyStream.ReadLine);

  while (Line= '') or (Line [1]<> '@') do
  begin
    if MyStream.Size= MyStream.Position then
      raise EInvalidSyntax.Create ('');

    Position:= MyStream.Position;
    Line:= MyStream.ReadLine;

  end;
  MyStream.Position:= Position+ 1;

  Result:= inherited ParseConstraint;

end;

function TLazyVersion2PBParser.ParseClause: TClause;
var
  Position: Integer;
  Line: AnsiString;
  S: String;
  n, i: Integer;
  v: Integer;
  Ch: Char;
  Lit: TLiteral;

begin
  Position:= MyStream.Position;
  Line:= MyStream.ReadLine;

  while (Line= '') or (Line [1]<> 'c') do
  begin
    Position:= MyStream.Position;
    Line:= MyStream.ReadLine;
    if MyStream.Position= MyStream.Size  then
      Break;

  end;
  MyStream.Position:= Position+ 1;
  if MyStream.Position< MyStream.Size then
  begin
    ParseInteger (S);
    n:= StrToInt (S);

    ParseOneOrMoreSpace;
    S:= '';

    Result:= TClause.Create;

    for i:= 1 to n do
    begin
      Ch:= GetNextChar;

      if Ch= 'x' then
      begin
        ParseUnsignedInt (S);
        v:= InputVariables.Item [StrToInt (S)];

      end
      else
      begin
        Expect ('x');
        ParseUnsignedInt (S);
        v:= InputVariables.Item [StrToInt (S)];

      end;

      Lit:= CreateLiteral (v, Ch= '~');

      Result.PushBack(Lit);
      ParseZeroOrMoreSpace;

    end;

  end
  else
    Exit (nil);

end;

procedure TLazyVersion2PBParser.ResetStream;
begin
  MyStream.Position:= 0;

end;

{ TPBParser }

function TPBParser.Expect (S: AnsiString): Boolean;
var
  i: Integer;

begin
  for i:= 1 to Length (S) do
    if GetNextChar<> S [i] then
    begin
      Rewind;
      WriteLn ('c Parser Error "', S, '"[', i, ']<>', GetNextChar);
      raise EExpectationFailed (S);

    end;
  Result:= True;

end;

function TPBParser.GetNextChar: Char;
begin
  LastPosition:= MyStream.Position;
  Result:= MyStream.ReadCh;

  while Result in [#10, #13] do
  begin
    if MyStream.EoStream then
      Exit (#$FF);

    Result:= MyStream.ReadCh;
  end;

end;

function TPBParser.GetNextWord: AnsiString;
var
  Ch: Char;
  Position: Int64;

begin
  Result:= '';
  Position:= MyStream.Position;

  Ch:= GetNextChar;
  assert (Ch<> ' ');

  while Ch<> ' ' do
  begin
    Result+= Ch;
    Ch:= GetNextChar;

  end;
  Rewind;
  LastPosition:= Position;

end;

procedure TPBParser.Rewind (Position: Int64);
begin
  MyStream.Position:= Position;

end;

procedure TPBParser.Rewind;
begin
  assert (LastPosition<> -1);
  Rewind (LastPosition);
  LastPosition:= -1;

end;

procedure TPBParser.ParseOneOrMoreSpace;
begin
  Expect (' ');

  while GetNextChar= ' ' do
    ;
  Rewind;

end;

procedure TPBParser.ParseZeroOrMoreSpace;
begin
  while GetNextChar= ' ' do
    ;
  Rewind;

end;

function TPBParser.ParseOneCommentLine: AnsiString;
begin
  Result:= MyStream.ReadLine;

end;

procedure TPBParser.ParseInteger (out Int: AnsiString);
var
  Ch: Char;

begin
  ParseZeroOrMoreSpace;
  Ch:= GetNextChar;

  if not (Ch in ['+', '-', '0'..'9']) then
    raise EExpectationFailed.Create (Ch);

  if Ch in ['+', '-'] then
    Int:= Ch
  else
    Int:= '+'+ Ch;

  Ch:= GetNextChar;

  while Ch in ['0'..'9'] do
  begin
    Int+= Ch;
    Ch:= GetNextChar;

  end;

  Rewind;

end;

procedure TPBParser.ParseUnsignedInt (out Int: AnsiString);
var
  Ch: Char;

begin
  Int:= '';

  Ch:= GetNextChar;
  while Ch in ['0'..'9'] do
  begin
    Int+= Ch;
    Ch:= GetNextChar;

  end;

  Rewind;

end;

type
  TListOfStringList= specialize TGenericCollection<TStringList>;

function TPBParser.ParseClause: TClause;
var
  Ch: Char;
  i, n, v: Integer;
  S: AnsiString;
  Lit: TLiteral;

begin
  ParseOneOrMoreSpace;

  ParseInteger (S);
  n:= StrToInt (S);

  ParseOneOrMoreSpace;
  S:= '';

  Result:= TClause.Create;

  for i:= 1 to n do
  begin
    Ch:= GetNextChar;

    if Ch= 'x' then
    begin
      ParseUnsignedInt (S);
      v:= InputVariables.Item [StrToInt (S)];

    end
    else
    begin
      Expect ('x');
      ParseUnsignedInt (S);
      v:= InputVariables.Item [StrToInt (S)];

    end;

    Lit:= CreateLiteral (v, Ch= '~');

    Result.PushBack(Lit);
    ParseZeroOrMoreSpace;

  end;

end;

function TPBParser.ParseSum: TPBSum;

  function CreateTerm (TermStr: TStringList; var TargetSumConstTerm: Int64;
       var TargetPBSum: TPBSum): TTerm;
  var
    c: AnsiString;
    Lit: TLiteral;
    Literals: TLiteralCollection;
    v: TTseitinVariable;
    vStr: AnsiString;
    S: AnsiString;
    i: Integer;
    CoefSign: Char;

  begin
    S:= TermStr [0];
    CoefSign:= S [1];

    Assert (CoefSign in ['+', '-']);
    c:= S;

    if TermStr.Count= 2 then
    begin
      vStr:= TermStr [1];
      if vStr [1]= '~' then
        v:= InputVariables.Item [StrToInt (Copy (vStr, 3, Length (vStr)))]
      else
        v:= InputVariables.Item [StrToInt (Copy (vStr, 2, Length (vStr)))];

     Lit:= CreateLiteral (v, (vStr [1]= '~') xor (CoefSign= '-'));

    end
    else
    begin
      Literals:= TLiteralCollection.Create;

      for i:= 1 to TermStr.Count- 1 do
      begin
        vStr:= TermStr [i];
        if vStr [1]= '~' then
          v:= InputVariables.Item [StrToInt (Copy (vStr, 3, Length (vStr)))]
        else
          v:= InputVariables.Item [StrToInt (Copy (vStr, 2, Length (vStr)))];

       Lit:= CreateLiteral (v, (vStr [1]= '~') xor (CoefSign= '-'));
       Literals.PushBack(Lit);

      end;

      Lit:= CreateLiteral (GetVariableManager.CreateNewVariable (vpNone, True), False);

      NonLinearVariableDescriptions.Add(Literals);
      CorrespondingLinearVariables.PushBack(Lit);

    end;

    Result:= TTerm.Create (Lit, BigIntFactory.GetNewMemeber.LoadFromString (@c [2]));
    if CoefSign= '-' then
      TargetPBSum.ConstantTerm.Add (Result.Coef);

  end;

var
  ActiveTermStr: TStringList;
  Terms: TListOfStringList;
  ActiveTerm: TTerm;

  Ch: Char;
  i: Integer;
  ConstantTerm: Int64;

begin
  Result:= TPBSum.Create;
  Terms:= TListOfStringList.Create;

  ActiveTermStr:= ParseTerm;
  Terms.PushBack(ActiveTermStr);

  Ch:= GetNextChar;
  while Ch in ['+', '-', '0'..'9'] do
  begin
    Rewind;

    ActiveTermStr:= ParseTerm;
    Terms.PushBack(ActiveTermStr);

    Ch:= GetNextChar;

  end;

  Rewind;
  ConstantTerm:= 0;

  for i:= 0 to Terms.Count- 1 do
  begin
    ActiveTermStr:= Terms.Item [i];
    ActiveTerm:= CreateTerm (ActiveTermStr, ConstantTerm, Result);
    Result.AddNewTerm (ActiveTerm);

  end;

  Terms.Free;

end;

function TPBParser.ParseCardConstraint: TPBConstraint;
var
  Literals: TClause;
  S: AnsiString;
  RHS: AnsiString;
  PBSum: TPBSum;
  i: Integer;
  Lit: TLiteral;
  ActiveTerm: TTerm;
  Ch: Char;

begin
  ParseOneOrMoreSpace;
  Ch:= GetNextChar;
  if Ch<> '=' then
  begin
    WriteLn ('Parser is not complete for ', Ch, ' as comparison operator in a CardinalityConstraint');
    raise EInvalidSyntax.Create ('Invalid Comparison Operator');

  end;

  ParseOneOrMoreSpace;
  ParseInteger (RHS);

  Literals:= ParseClause;

  PBSum:= TPBSum.Create;

  for i:= 0 to Literals.Count- 1 do
  begin
    Lit:= Literals.Items[i];
    ActiveTerm:= TTerm.Create (Lit, BigIntFactory.GetNewMemeber.SetValue (1));
    PBSum.AddItem (ActiveTerm);

  end;

  Result:= TPBConstraint.Create (PBSum, '=', True,
          BigIntFactory.GetNewMemeber.LoadFromString (@ RHS [2]));

end;

function TPBParser.ParseConstraint: TPBConstraint;
var
  PBSum: TPBSum;
  ConstraintOperator: AnsiString;
  S: AnsiString;
  RHS: AnsiString;

begin
  PBSum:= ParseSum;

  ParseZeroOrMoreSpace;
  ConstraintOperator:= GetNextChar;
  if ConstraintOperator= '>' then
    ConstraintOperator+= GetNextChar
  else if ConstraintOperator= '<' then
    ConstraintOperator+= GetNextChar;

  ParseZeroOrMoreSpace;
  ParseInteger (S);

  ParseZeroOrMoreSpace;
  Expect (';');

  RHS:= S;
  Result:= TPBConstraint.Create (PBSum, ConstraintOperator, RHS [1]= '+',
          BigIntFactory.GetNewMemeber.LoadFromString (@ RHS [2]));

end;

function TPBParser.ParseTerm: TStringList;
var
  c: AnsiString;
  Ch: Char;
  S: AnsiString;

begin
  Result:= TStringList.Create;

  ParseInteger (c);
  Result.Add (c);

  ParseOneOrMoreSpace;
  Ch:= GetNextChar;
  S:= '';

  while Ch in ['x', '~'] do
  begin

    if Ch= 'x' then
    begin
      ParseUnsignedInt (S);
      S:= 'x'+ S;

    end
    else
    begin
      Expect ('x');
      ParseUnsignedInt (S);
      S:= '~x'+ S;

    end;
    Result.Add (S);
    ParseOneOrMoreSpace;
    Ch:= GetNextChar;

  end;
  Rewind;

end;

constructor TPBParser.Create (AnStream: TStream);
begin
  inherited Create;

  MyStream:= TMyTextStream.Create (AnStream);

end;

destructor TPBParser.Destroy;
begin
  MyStream.Free;

  inherited Destroy;

end;

function TPBParser.ParseVersion2: TPBSpecification;
var
  FirstCommentLine: AnsiString;
  VarCount, ConsCount, ClauseCount, CardCount: Integer;
  Ch: Char;
  S: AnsiString;
  j: Integer;
  v: Integer;

begin
  VarCount:= 0; ConsCount:= 0; ClauseCount:= 0; CardCount:= 0;

  FirstCommentLine:= ParseOneCommentLine+ ' ';
  Delete (FirstCommentLine, 1, Length ('* #variable= '));
  VarCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

  Delete (FirstCommentLine, 1, Length ('#clause= '));
  ClauseCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

  Delete (FirstCommentLine, 1, Length ('#cardinality= '));
  CardCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

  Delete (FirstCommentLine, 1, Length ('#constraint= '));
  ConsCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

  WriteLn ('c ', VarCount, ' ', ClauseCount, ' ', CardCount, ' ', ConsCount);

  InputVariables:= TInputVariableCollection.Create;
  InputVariables.AddItem (0);

  for j:= 1 to VarCount do
  begin
    v:= GetVariableManager.CreateNewVariable (vpNone, True);
    InputVariables.AddItem (v);//GetVariableManager.CreateNewVariable);

  end;
  GetVariableManager.CreateTrueVariable;

//  VisitedConstraints:= 0; VisitedCardCons:= 0; VisitedClause:= 0;

  Result:= TPBSpecification.Create (InputVariables);

  while not MyStream.EoStream do
  begin
    Ch:= GetNextChar;
    if MyStream.EoStream then
      Break;

    case Ch of
      '*':
         ParseOneCommentLine;

      'c':
        Result.AddNewClause (ParseClause);

      '#':
        Result.AddNewConstraint (ParseCardConstraint);

      '@':
        Result.AddNewConstraint (ParseConstraint)
 
      else
        raise EInvalidSyntax.Create ('Invalid File');

    end;

  end;

//  GetVariableManager.SetLastVariableIndex (Result.MaxVarInProblem);
  Result.Finalize;

end;

function TPBParser.ParsePB: TPBSpecification;
const
  MinString: AnsiString= 'min:';

var
  FirstCommentLine: AnsiString;

  VarCount, ConsCount, ProdCount, SizeOfProduct,
  SoftCount, MinCost, MaxCost, SumCost: Integer;
  TargetSoft, Weight: Integer;
  VisitedConstraints, VisitedSoftConstraints: Integer;

  InputMode: Integer;

//  PBSum: TPBSum;
  PBConstraint: TPBConstraint;
//  ConstraintOperator: AnsiString;
//  ConstraintRHS: AnsiString;
  Ch: Char;
  S: AnsiString;
  j: Integer;
  v: Integer;

begin
  VarCount:= 0; ConsCount:= 0; ProdCount:= 0; SizeOfProduct:= 0;
  SoftCount:= 0; MinCost:= 0; MaxCost:= 0; SumCost:= 0;

  FirstCommentLine:= ParseOneCommentLine+ ' ';
  Delete (FirstCommentLine, 1, Pos ('*', FirstCommentLine)+ 1);

  if UpperCase (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1))= 'VERSION' then
  begin
    Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

    case StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1)) of
      2:
        Exit (ParseVersion2)
      else
      begin
        WriteLn ('Invalid Version ...!!!');
        Exit (nil);

      end;
    end;

  end
  else
  begin
    Delete (FirstCommentLine, 1, Length ('#variable= '));
    VarCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
    Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
    Delete (FirstCommentLine, 1, Length ('#constraint= '));
    ConsCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
    Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
  
    InputMode:= 1;
  
    if (Pos ('product', FirstCommentLine)<> 0) and  (Pos ('soft', FirstCommentLine)= 0) then
    begin
      Delete (FirstCommentLine, 1, Length ('#product= '));
      ProdCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('sizeproduct= '));
      SizeOfProduct:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      FirstCommentLine:= Trim (FirstCommentLine);
  
      InputMode:= 2;
  
    end
    else if (Pos ('product', FirstCommentLine)= 0) and (Pos ('soft', FirstCommentLine)<> 0) then
    begin
      Delete (FirstCommentLine, 1, Length ('#soft= '));
      SoftCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('mincost= '));
      MinCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('maxcost= '));
      MaxCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('sumcost= '));
      SumCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  
      InputMode:= 3;
  
    end
    else if (Pos ('product', FirstCommentLine)<> 0) and (Pos ('soft', FirstCommentLine)<> 0) then
    begin
      Delete (FirstCommentLine, 1, Length ('#product= '));
      ProdCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('sizeproduct= '));
      SizeOfProduct:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  
      Delete (FirstCommentLine, 1, Length ('#soft= '));
      SoftCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('mincost= '));
      MinCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('maxcost= '));
      MaxCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('sumcost= '));
      SumCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
  
      InputMode:= 4;
  
    end;
  
    WriteLn ('c ', VarCount, ' ', ConsCount, ' ', ProdCount, ' ', SizeOfProduct, ' ',
         SoftCount, ' ', MinCost, ' ', MaxCost, ' ', SumCost);
  
    InputVariables:= TInputVariableCollection.Create;
    InputVariables.AddItem (0);
  
    for j:= 1 to VarCount do
    begin
      v:= GetVariableManager.CreateNewVariable (vpNone, True);
      InputVariables.AddItem (v);//GetVariableManager.CreateNewVariable);
  
    end;
    GetVariableManager.CreateTrueVariable;

    VisitedConstraints:= 0; VisitedSoftConstraints:= 0;
    NonLinearVariableDescriptions:= TList.Create;
    CorrespondingLinearVariables:= TLiteralCollection.Create;
  
    Result:= TPBSpecification.Create (VarCount, ConsCount, ProdCount,
                         SizeOfProduct, SoftCount, MinCost, MaxCost, SumCost, InputVariables,
                         NonLinearVariableDescriptions, CorrespondingLinearVariables);
  
    while not MyStream.EoStream do
    begin
      Ch:= GetNextChar;
      case Ch of
        '*':
           ParseOneCommentLine;
  
        'm':
        begin
          raise EInvalidSyntax.Create ('Optimization is not supported');
          Halt (1);

          Rewind;
          Expect (MinString);
          ParseZeroOrMoreSpace;
//          Result.ObjectiveFunction:= ParseSum;
          Expect (';');
          MyStream.ReadLine;
  
        end;
        's':
        begin
          raise EInvalidSyntax.Create ('Soft Constraints are not supported');
          Rewind;
          Expect ('soft: ');
          ParseUnsignedInt (S);
          TargetSoft:= StrToInt (S);
          ParseZeroOrMoreSpace;
          Rewind;
          Expect (';');
          MyStream.ReadLine;
//          Result.Target:= TargetSoft;;
  
  
        end;
        '[':
        begin
          raise EInvalidSyntax.Create ('Soft Constraints are not supported');

          ParseZeroOrMoreSpace;
          ParseUnsignedInt (S);
          Weight:= StrToInt (S);
          ParseZeroOrMoreSpace;
          Expect (']');
          ParseZeroOrMoreSpace;
  
          PBConstraint:= ParseConstraint;
          MyStream.ReadLine;
          Inc (VisitedSoftConstraints);
  
//          Result.AddNewSoftConstraint (PBConstraint, Weight);
  {        WriteLn (VisitedSoftConstraints, ':[', Weight, ']', PBSum.ToXML, ConstraintOperator,
            ConstraintRHS);
  }
  
        end
        else//It is a constraint
        begin
  
          while Ch in [' '] do
            Ch:= GetNextChar;
  
          if Ch in ['+', '-', '0'..'9'] then
          begin
            if Ch= '-' then
            begin
//              WriteLn ('For now, all coefficient should be positive!');
  {ToDo: all coefficient should be positive!}
//               Halt (1);
  
            end;
            Rewind;
  
            Inc (VisitedConstraints);
            PBConstraint:= ParseConstraint;
            Result.AddNewConstraint (PBConstraint);
{            if VisitedConstraints mod 1000= 0 then
               WriteLn (VisitedConstraints, ':', PBConstraint.ToString);
}
            MyStream.ReadLine;
  
          end;
  
        end;
  
      end;
  
    end;
  
  
    if VisitedConstraints+ VisitedSoftConstraints<> ConsCount then
      WriteLn ('c Error in Parser!');
  
//    GetVariableManager.SetLastVariableIndex (Result.MaxVarInProblem);
  
    Result.Finalize;

  end;

end;

function TLazyPBParser.GetPosition: Int64;
begin
  Result:= MyStream.Position;

end;

procedure TLazyPBParser.SetPosition (AValue: Int64);
begin
  MyStream.Position:= AValue;

end;

constructor TLazyPBParser.Create (AnStream: TStream);
var
  Buffer: array [0..1000] of Char;
  n: Integer;

begin
  InputStream:= TStringStream.Create ('');

  while AnStream.Position< AnStream.Size do
  begin
    n:= AnStream.Read (Buffer, 1000);
    InputStream.WriteBuffer (Buffer, n);

  end;
  InputStream.Position:= 0;

  inherited Create (InputStream);

end;

destructor TLazyVersion2PBParser.Destroy;
begin
//  InputStream.Free;

  inherited Destroy;
end;

function TLazyPBParser.ParsePB: TPBSpecification;
const
  MinString: AnsiString= 'min:';

var
  FirstCommentLine: AnsiString;

  VarCount, ConsCount, ProdCount, SizeOfProduct,
  SoftCount, MinCost, MaxCost, SumCost: Integer;
  TargetSoft, Weight: Integer;
  VisitedConstraints, VisitedSoftConstraints: Integer;

  InputMode: Integer;
  PBConstraint: TPBConstraint;
  Ch: Char;
  S: AnsiString;
  j: Integer;
  v: Integer;

begin
  VarCount:= 0; ConsCount:= 0; ProdCount:= 0; SizeOfProduct:= 0;
  SoftCount:= 0; MinCost:= 0; MaxCost:= 0; SumCost:= 0;

  FirstCommentLine:= ParseOneCommentLine+ ' ';
  Delete (FirstCommentLine, 1, Pos ('*', FirstCommentLine)+ 1);

  if UpperCase (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1))= 'VERSION' then
  begin
    Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

    case StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1)) of
      2:
        Exit (ParseVersion2)
      else
      begin
        WriteLn ('Invalid Version ...!!!');
        Exit (nil);

      end;
    end;

  end
  else
  begin
    Delete (FirstCommentLine, 1, Length ('#variable= '));
    VarCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
    Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
    Delete (FirstCommentLine, 1, Length ('#constraint= '));
    ConsCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
    Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));

    InputMode:= 1;

    if (Pos ('product', FirstCommentLine)<> 0) and  (Pos ('soft', FirstCommentLine)= 0) then
    begin
      Delete (FirstCommentLine, 1, Length ('#product= '));
      ProdCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('sizeproduct= '));
      SizeOfProduct:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      FirstCommentLine:= Trim (FirstCommentLine);

      InputMode:= 2;

    end
    else if (Pos ('product', FirstCommentLine)= 0) and (Pos ('soft', FirstCommentLine)<> 0) then
    begin
      Halt (1);
      Delete (FirstCommentLine, 1, Length ('#soft= '));
      SoftCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('mincost= '));
      MinCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('maxcost= '));
      MaxCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('sumcost= '));
      SumCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));

      InputMode:= 3;

    end
    else if (Pos ('product', FirstCommentLine)<> 0) and (Pos ('soft', FirstCommentLine)<> 0) then
    begin
      Halt (1);
      Delete (FirstCommentLine, 1, Length ('#product= '));
      ProdCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('sizeproduct= '));
      SizeOfProduct:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));

      Delete (FirstCommentLine, 1, Length ('#soft= '));
      SoftCount:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('mincost= '));
      MinCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('maxcost= '));
      MaxCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));
      Delete (FirstCommentLine, 1, Pos (' ', FirstCommentLine));
      Delete (FirstCommentLine, 1, Length ('sumcost= '));
      SumCost:= StrToInt (Copy (FirstCommentLine, 1, Pos (' ', FirstCommentLine)- 1));

      InputMode:= 4;

    end;

    WriteLn ('c ', VarCount, ' ', ConsCount, ' ', ProdCount, ' ', SizeOfProduct, ' ',
         SoftCount, ' ', MinCost, ' ', MaxCost, ' ', SumCost);

    InputVariables:= TInputVariableCollection.Create;
    InputVariables.AddItem (0);

    for j:= 1 to VarCount do
    begin
      v:= GetVariableManager.CreateNewVariable (vpNone, True);
      InputVariables.AddItem (v);//GetVariableManager.CreateNewVariable);

    end;
    GetVariableManager.CreateTrueVariable;

    VisitedConstraints:= 0; VisitedSoftConstraints:= 0;
    NonLinearVariableDescriptions:= TList.Create;
    CorrespondingLinearVariables:= TLiteralCollection.Create;

    Result:= TLazyPBSpecification.Create (VarCount, ConsCount, ProdCount,
                         SizeOfProduct, SoftCount, MinCost, MaxCost, SumCost, InputVariables,
                         NonLinearVariableDescriptions, CorrespondingLinearVariables,
                         Self);

  end;

end;

function TLazyPBParser.ParseConstraint: TPBConstraint;
var
  Position: Integer;
  Line: AnsiString;

begin
  Position:= MyStream.Position;
  Line:= Trim (MyStream.ReadLine);

  while (Line= '') or
        (not (Line [1] in ['+', '-', '0'..'9'])) do
  begin
    Position:= MyStream.Position;
    Line:= MyStream.ReadLine;

  end;
  MyStream.Position:= Position;

  Result:= inherited ParseConstraint;

end;

function TLazyPBParser.ParseClause: TClause;
begin
  Halt (1);

end;

procedure TLazyPBParser.ResetStream;
begin
  MyStream.Position:= 0;

end;

end.

