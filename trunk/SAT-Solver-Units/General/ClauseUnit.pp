unit ClauseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit;

type
  TGroundBool= (gbFalse= 0, gbUnknown= 1, gbTrue= 2);// TODO: Change the default Values ..

  { TLiteral }

  TLiteral= Integer;
  {TODO: Integer => UInteger}

  { TLiteralCollection }
  TSpecializeTGenericCollectionForBuildInDataTLiteral= specialize TGenericCollectionForBuildInData<TLiteral>;

  TLiteralCollection= class (TSpecializeTGenericCollectionForBuildInDataTLiteral)
  public
    function ToXML: AnsiString;
    function ToString: AnsiString;

    function Copy: TLiteralCollection;

    function IsExist (Lit: TLiteral): Boolean;

  end;

  TClause= TLiteralCollection;
  TGenericCollectionTClause= specialize TGenericCollection<TClause>;

  { TClauseCollection }

  TClauseCollection= class (TGenericCollectionTClause)
  private
    FMaxVar: Integer;

    function GetMaxVar: Integer;

  public
    property MaxVar: Integer read GetMaxVar;

    constructor Create;

    function Copy: TClauseCollection;

  end;


  function GetVar (Lit: TLiteral): Integer; inline;
  function IsNegated (Lit: TLiteral): Boolean;
  function GetValue (Lit: TLiteral): Integer; inline;
  function NegateLiteral   (Lit: TLiteral): TLiteral; inline;
  function CreateLiteral: TLiteral; inline;
  function CreateLiteral (VarValue: Integer; IsNegated: Boolean): TLiteral; inline;
  function LiteralToString (Lit: TLiteral): AnsiString;


implementation

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
  Result:= TLiteralCollection.Create (Self.Count);

  for i:= 0 to Self.Count- 1 do
    Result.Item [i]:= Self.Item [i];

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

function CreateLiteral: TLiteral; inline;
begin
  Result:= 0;

end;

function CreateLiteral (VarValue: Integer; IsNegated: Boolean): TLiteral; inline;
begin
  Result:= (VarValue shl 1);
  if IsNegated then
    Inc (Result);

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

function TClauseCollection.GetMaxVar: Integer;
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

constructor TClauseCollection.Create;
begin
  inherited Create;

  FMaxVar:= -1;

end;

function TClauseCollection.Copy: TClauseCollection;
var
  i: Integer;

begin
  Result:= TClauseCollection.Create;

  for i:= 0 to Self.Count- 1 do
    Result.AddItem (Self.Item [i].Copy);

end;

end.

