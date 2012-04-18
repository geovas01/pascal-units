unit ClauseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, MyTypes, StreamUnit;

type
  TGroundBool= (gbFalse= 0, gbUnknown= 1, gbTrue= 2);// TODO: Change the default Values ..

  { TLiteral }

  TLiteral= Integer;
  TVariable= Integer;
  {TODO: Integer => UInteger}

  { TLiteralCollection }
  TSpecializeTGenericCollectionForBuiltInDataTLiteral= specialize TGenericCollectionForBuiltInData<TLiteral>;

  TLiteralCollection= class (TSpecializeTGenericCollectionForBuiltInDataTLiteral)
  private
    FLiteralCount: Integer;
    FMaxVar: TVariable;

  public
    {
    MaxVar is always greater or equal to maximum variable in the TLiteralCollection
    }
    property MaxVar: TVariable read FMaxVar;

    function ToXML: AnsiString;
    function ToString: AnsiString; override;

    function Copy: TLiteralCollection;

    function IsExist (Lit: TLiteral): Boolean;
    procedure AddItem (Lit: TLiteral); override;
    procedure SetItem (Index: Integer; const Lit: TLiteral); override;

    procedure Reset;
  end;

  TListOfLiteralCollection= specialize TGenericCollection<TLiteralCollection>;

  TClause= TLiteralCollection;
  TGenericCollectionTClause= specialize TGenericCollection<TClause>;

  { TClauseCollection }

  TClauseCollection= class (TGenericCollectionTClause)
  private
    FMaxVar: TVariable;
    FLiteralCount: Integer;

    function GetCount: Integer;
    function GetMaxVar: TVariable;
    procedure SetItem (Index: Integer; const AClause: TClause); override;

  public
    property MaxVar: TVariable read GetMaxVar;
    property ClauseCount: Integer read GetCount;
    property LiteralCount: Integer read FLiteralCount;

    constructor Create;

    function Copy: TClauseCollection;
    function ToString: AnsiString; override;

    procedure AddItem (NewClause: TClause); override;
    procedure Load (Stream: TMyTextStream); override;

  end;



  function GetVar (Lit: TLiteral): TVariable; inline;
  function IsNegated (Lit: TLiteral): Boolean;
  function GetValue (Lit: TLiteral): Integer; inline;
  function NegateLiteral   (Lit: TLiteral): TLiteral; inline;
  function CopyLiteral   (Lit: TLiteral): TLiteral; inline;
//  function CreateLiteral: TLiteral; inline;
  function CreateLiteral (VarValue: Integer; IsNegated: Boolean): TLiteral; inline;
  function LiteralToString (Lit: TLiteral): AnsiString;


implementation
uses
  TSeitinVariableUnit, Math;

function TLiteralCollection.ToXML: AnsiString;
var
  i: Integer;

begin
  Result:= '<LiteralCollection>';
  for i:= 0 to Count- 1 do
    Result+= '<Literal v= "'+ LiteralToString (Item [i])+ '"/>';

  Result+= '</LiteralCollection>';

end;

function TLiteralCollection.ToString: AnsiString;
var
  i: Integer;

begin
  Result:= '';
  for i:= 0 to Count- 1 do
    Result+= LiteralToString (Item [i])+ ' ';

end;

function TLiteralCollection.Copy: TLiteralCollection;
var
  i: Integer;

begin
  Result:= TLiteralCollection.Create (Self.Count, 3);

  for i:= 0 to Self.Count- 1 do
    Result.Item [i]:= Self.Item [i];
  Result.Count:= Self.Count;

end;

function TLiteralCollection.IsExist (Lit: TLiteral): Boolean;
var
  i: Integer;

begin
  Result:= True;

  for i:= 0 to Count- 1 do
    if Item [i]= Lit then
      Exit;

  Result:= False;

end;

procedure TLiteralCollection.AddItem (Lit: TLiteral);
begin
  FMaxVar:= Math.Max (GetVar (Lit), MaxVar);

  inherited AddItem(Lit);
end;

procedure TLiteralCollection.SetItem (Index: Integer; const Lit: TLiteral);
begin
  FMaxVar:= Math.Max (GetVar (Lit), MaxVar);

  inherited SetItem (Index, Lit);

end;

procedure TLiteralCollection.Reset;
begin
  Count:= 0;

end;

function GetVar (Lit: TLiteral): Integer; inline;
begin
  Exit (Lit shr 1);

end;

function IsNegated (Lit: TLiteral): Boolean;
begin
  Exit ((Lit and 1)= 1);

end;

function GetValue (Lit: TLiteral): Integer; inline;
begin
  if IsNegated (Lit) then
    Exit (-GetVar (Lit))
  else
    Exit (GetVar (Lit));

end;

function NegateLiteral (Lit: TLiteral): TLiteral; inline;
begin
  Result:= Lit;
  Result:= Result xor 1;

end;

{
function CreateLiteral: TLiteral; inline;
begin
  Result:= 0;

end;
}

function CopyLiteral (Lit: TLiteral): TLiteral;
begin
  case TSeitinVariableUnit.GetVariableManager.SatSolver.GetLiteralValue (Lit) of
    gbFalse:
      Result:= GetVariableManager.FalseLiteral;
    gbTrue:
      Result:= GetVariableManager.TrueLiteral;
    gbUnknown:
      Result:= Lit;

  end;

end;

function CreateLiteral (VarValue: Integer; IsNegated: Boolean): TLiteral; inline;
begin
  Result:= (VarValue shl 1);
  if IsNegated then
    Result:= Result or 1;

end;

function LiteralToString (Lit: TLiteral): AnsiString;
begin
  if IsNegated (Lit) then
    Result:= '~'
  else
    Result:= '';

  Result+= 'x'+ IntToStr (GetVar (Lit));

end;

{ TClauseCollection }

function TClauseCollection.GetMaxVar: TVariable;
var
  i, j: Integer;
  ActiveClause: TClause;
  MaxLit: TLiteral;

begin
  if FMaxVar<> -1 then
    Exit (FMaxVar);

  MaxLit:= 0;
  for i:= 0 to Count- 1 do
  begin
    ActiveClause:= Item [i];

    for j:= 0 to ActiveClause.Count- 1 do
      if MaxLit< ActiveClause.Item [j] then
        MaxLit:= ActiveClause.Item [j];

  end;

  FMaxVar:= GetVar (MaxLit);
  Result:= FMaxVar;

end;

function TClauseCollection.GetCount: Integer;
begin
  Result:= Count;
end;

procedure TClauseCollection.SetItem (Index: Integer; const AClause: TClause);
var
  i: Integer;

begin
  FMaxVar:= Math.Max (FMaxVar, AClause.MaxVar);
  FLiteralCount+= AClause.Count;

  inherited SetItem (Index, AClause);

end;

constructor TClauseCollection.Create;
begin
  inherited Create;

  //FMaxVar:= 0;
  //FLiteralCount:= 0;

end;

function TClauseCollection.Copy: TClauseCollection;
var
  i: Integer;

begin
  Result:= TClauseCollection.Create;

  for i:= 0 to Self.Count- 1 do
    Result.AddItem (Self.Item [i].Copy);

end;

function TClauseCollection.ToString: AnsiString;
var
  i: Integer;

begin
  Result:= '';

  for i:= 0 to Self.Count- 1 do
    Result+= Self.Item [i].ToString+ #10;

end;

procedure TClauseCollection.AddItem (NewClause: TClause);
var
  i: Integer;

begin
  for i:= 0 to NewClause.Count- 1 do
    if FMaxVar< GetVar (NewClause.Item [i]) then
      FMaxVar:= GetVar (NewClause.Item [i]);
  FLiteralCount+= NewClause.Count;

  inherited AddItem (NewClause);

end;

procedure TClauseCollection.Load (Stream: TMyTextStream);
var
  S: AnsiString;
  ActiveClause: TClause;
  n: Integer;
  Lit: TLiteral;

begin
  S:= Stream.ReadLine;
  if System.Copy (S, 1, Length ('p cnf'))<> 'p cnf' then
  begin
    WriteLn ('Invalid file!');
    Halt (1);

  end;

  while not Stream.EoStream do
  begin
    ActiveClause:= TClause.Create;

    n:= Stream.ReadInteger;

    while n<> 0 do
    begin
      Lit:= CreateLiteral (abs (n), n< 0);
      ActiveClause.AddItem (Lit);

      n:= Stream.ReadInteger;

    end;
    Stream.ReadLine;

    Self.AddItem (ActiveClause);

  end;

end;

end.

