unit ClauseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit;

type
  TGroundBool= (gbFalse= 0, gbUnknown= 1, gbTrue= 2);// TODO: Change the default Values ..

  { TLiteral }

  TLiteral= Integer;
  TVariable= Integer;
  {TODO: Integer => UInteger}

  { TLiteralCollection }
  TSpecializeTGenericCollectionForBuiltInDataTLiteral= specialize TGenericCollectionForBuildInData<TLiteral>;

  TLiteralCollection= class (TSpecializeTGenericCollectionForBuiltInDataTLiteral)
  public
    function ToXML: AnsiString;
    function ToString: AnsiString; override;

    function Copy: TLiteralCollection;

    function IsExist (Lit: TLiteral): Boolean;

  end;

  TListOfLiteralCollection= specialize TGenericCollection<TLiteralCollection>;

  TClause= TLiteralCollection;
  TGenericCollectionTClause= specialize TGenericCollection<TClause>;

  { TClauseCollection }

  TClauseCollection= class (TGenericCollectionTClause)
  private
    FMaxVar: TVariable;

    function GetMaxVar: TVariable;

  public
    property MaxVar: TVariable read GetMaxVar;

    constructor Create;

    function Copy: TClauseCollection;
    function ToString: AnsiString; override;

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
  TSeitinVariableUnit;

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

function TClauseCollection.ToString: AnsiString;
var
  i: Integer;

begin
  Result:= '';

  for i:= 0 to Self.Count- 1 do
    Result+= Self.Item [i].ToString+ #10;

end;

end.

