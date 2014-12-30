unit PBModEncoderDCUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractPBModEncoderUnit, PBConstraintUnit,
  ClauseUnit, TSeitinVariableUnit, CollectionUnit;

type
  { TPBModEncoderDC }

  TPBModEncoderDC= class (TAbstractPBModEncoder)
  private
  protected
    Memory: array of TListOfLiteralCollection;

   public

    destructor Destroy; override;
    constructor Create (_OrigConsrraint: TPBConstraint; _VariableManager: TVariableManager;
                          _Coefs: TInt64Collection; _b: Int64;
                          _OrigSum: TPBSum;
                           _Modulo: Integer);

  end;

  { TPBModEncoderDCUsingDirect }

  TPBModEncoderDCUsingDirect= class (TPBModEncoderDC)
  protected
    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function EncodePBMod: TLiteral; override;

  end;

  { TPBModEncoderDCUsingTseitin }

  TPBModEncoderDCUsingTseitin= class (TPBModEncoderDC)
  protected
    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function EncodePBMod: TLiteral; override;

  end;



implementation
uses
  ParameterManagerUnit, GenericFactoryUnit;

{ TPBModEncoderDCUsingTseitin }

procedure TPBModEncoderDCUsingTseitin.AddExtraClauses_Medium;
var
  l, i: Integer;
  b1, b2: Integer;
  ActiveAnswer: TLiteralCollection;

begin

  { Recursive Construction}

  for l:= 0 to High (Memory) do
    for i:= 0 to Memory[l].Count- 1 do
      begin
        ActiveAnswer:= Memory[l].Item[i];

        for b1:= 0 to Modulo- 1 do
          if GetVar (ActiveAnswer.Items[b1])<> 0 then
            for b2:= b1+ 1 to Modulo- 1 do
              if GetVar (ActiveAnswer.Items[b2])<> 0 then
                if (ActiveAnswer.Items[b1]<> VariableGenerator.FalseLiteral) and
                   (ActiveAnswer.Items[b2]<> VariableGenerator.FalseLiteral) then
                begin
                  VariableGenerator.SatSolver.BeginConstraint;

                  VariableGenerator.SatSolver.AddLiteral (NegateLiteral (ActiveAnswer.Items[b1]));
                  VariableGenerator.SatSolver.AddLiteral (NegateLiteral (ActiveAnswer.Items[b2]));

                  VariableGenerator.SatSolver.SubmitClause;// Result[i]=> \lnot Result[j]

                end;

      end;

end;

procedure TPBModEncoderDCUsingTseitin.AddExtraClauses_High;
var
  l, i: Integer;
  b1: Integer;
  Ignore: Boolean;
  ActiveAnswer: TLiteralCollection;

begin

  for l:= 0 to High (Memory) do
    for i:= 0 to Memory[l].Count- 1 do
      begin
        ActiveAnswer:= Memory[l].Item[i];

        VariableGenerator.SatSolver.BeginConstraint;

        for b1:= 0 to Modulo- 1 do
          if GetVar (ActiveAnswer.Items[b1])<> 0 then
            VariableGenerator.SatSolver.AddLiteral (ActiveAnswer.Items[b1]);

        VariableGenerator.SatSolver.SubmitClause;// Result[0] \lor Result[1] \lor Result[M-1]

      end;


end;

type
  TLiteralCollectionFactory= specialize TGenericFactoy<TLiteralCollection>;


function TPBModEncoderDCUsingTseitin.EncodePBMod: TLiteral;
var
  LiteralCollectionFactory: TLiteralCollectionFactory;

  function EncodeUsingTseitin (Index: Integer; Len: Integer): TLiteralCollection;
  {
    Create an answer for InputLiterals[Index, ..., Index+ Len- 1]
  }
  var
    Temp: TLiteralCollection;
    Left, Right: TLiteralCollection;
    i, j: Integer;

  begin

    if Len= 1 then
    begin
      Result:= TLiteralCollection.Create (Modulo, VariableGenerator.FalseLiteral);

      if Coefs.Item[Index]<> 0 then
      begin
        Result.Items[Coefs.Item[Index]]:= CopyLiteral (OrigSum.Item[Index].Literal);
        Result.Items[0]:= CopyLiteral (NegateLiteral (OrigSum.Item[Index].Literal));

      end
      else
        Result.Items[0]:= VariableGenerator.TrueLiteral;

      Memory[Len].AddItem (Result);

    end
    else
    begin
      Left:= EncodeUsingTseitin (Index, Len div 2);
      Right:= EncodeUsingTseitin (Index+ Len div 2, Len- Len div 2);

      Result:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);

//      Temp:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);
      Temp:= LiteralCollectionFactory.GetNewMemeber;
      Temp.Count:= Modulo;

      for i:= 0 to Modulo- 1 do
      begin

        for j:= 0 to Modulo- 1 do
          Temp.Items[j]:= VariableGenerator.CreateVariableDescribingAND
                               (
                               Left.Items[j],
                               Right.Items[(i- j+ Modulo) mod Modulo]
                               );
        Result.Items[i]:= VariableGenerator.CreateVariableDescribingOR (Temp);

      end;

      LiteralCollectionFactory.ReleaseMemeber (Temp);

{
      Left.Free;
      Right.Free;
}
      Memory[Len].AddItem (Result);

    end;

  end;

var
  Temp: TLiteralCollection;

begin
  LiteralCollectionFactory:= TLiteralCollectionFactory.Create;

  if OrigSum.Count= 0 then
  begin
    if b= 0 then
      Result:=  VariableGenerator.TrueLiteral
    else
      Result:=  VariableGenerator.FalseLiteral;

  end
  else
  begin
      Temp:= EncodeUsingTseitin (0, OrigSum.Count);

    Result:= Temp.Items[b];

  end;
  LiteralCollectionFactory.Free;

end;

{ TPBModEncoderDCUsingDirect }

procedure TPBModEncoderDCUsingDirect.AddExtraClauses_Medium;
begin
end;

procedure TPBModEncoderDCUsingDirect.AddExtraClauses_High;
begin
end;

function TPBModEncoderDCUsingDirect.EncodePBMod: TLiteral;
var
  LiteralCollectionFactory: TLiteralCollectionFactory;

  function EncodeDirectly (Index: Integer; Len: Integer): TLiteralCollection;
  {
    Create an answer for InputLiterals[Index, ..., Index+ Len- 1]
  }
  var
    Left, Right: TLiteralCollection;
    i, k, k1, k2: Integer;

  begin

    if Len= 1 then
    begin
      Result:= TLiteralCollection.Create (Modulo, VariableGenerator.FalseLiteral);

      if Coefs.Item[Index]<> 0 then
      begin
        Result.Items[Coefs.Item[Index]]:= CopyLiteral (OrigSum.Item[Index].Literal);
        Result.Items[0]:= CopyLiteral (NegateLiteral (OrigSum.Item[Index].Literal));

      end
      else
        Result.Items[0]:= VariableGenerator.TrueLiteral;

      Memory[Len].AddItem (Result);

    end
    else
    begin
      Left:= EncodeDirectly (Index, Len div 2);
      Right:= EncodeDirectly (Index+ Len div 2, Len- Len div 2);

      Result:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);
      for i:= 0 to Modulo- 1 do
        Result.Items[i]:= CreateLiteral (GetVariableManager.CreateNewVariable, False);

      for k:= 0 to Modulo- 1 do
        for i:= 0 to Modulo- 1 do
        begin
          CNFGenerator.BeginConstraint;{Left[i] \land Right[k] => Result[i+ k]}

          CNFGenerator.AddLiteral (NegateLiteral (Left.Items[i]));
          CNFGenerator.AddLiteral (NegateLiteral (Right.Items[k]));
          CNFGenerator.AddLiteral (Result.Items[(i+ k) mod Modulo]);

          CNFGenerator.SubmitClause;

        end;

      for k1:= 0 to Modulo- 1 do
        for k2:= k1+ 1 to Modulo- 1 do
        begin
          CNFGenerator.BeginConstraint;{Result[k1]=> ~Result[k2]}

          CNFGenerator.AddLiteral (NegateLiteral (Result.Items[k1]));
          CNFGenerator.AddLiteral (NegateLiteral (Result.Items[k2]));

          CNFGenerator.SubmitClause;

        end;

      CNFGenerator.BeginConstraint;{Result[0], Result[1], ..., Result[Modulo- 1]}
      for i:= 0 to Modulo- 1 do
        CNFGenerator.AddLiteral (Result.Items[i]);
      CNFGenerator.SubmitClause;

{
      Left.Free;
      Right.Free;
}
      Memory[Len].AddItem (Result);

    end;

  end;

var
  Temp: TLiteralCollection;

begin
  LiteralCollectionFactory:= TLiteralCollectionFactory.Create;

  if OrigSum.Count= 0 then
  begin
    if b= 0 then
      Result:=  VariableGenerator.TrueLiteral
    else
      Result:=  VariableGenerator.FalseLiteral;

  end
  else
  begin
    Temp:= EncodeDirectly (0, OrigSum.Count);

    Result:= Temp.Items[b];

  end;
  LiteralCollectionFactory.Free;

end;

{ TPBModEncoderDC }

destructor TPBModEncoderDC.Destroy;
var
  i: Integer;

begin

  for i:= 0 to High (Memory) do
    Memory[i].Free;
  SetLength (Memory, 0);

  inherited;

end;

constructor TPBModEncoderDC.Create (_OrigConsrraint: TPBConstraint; _VariableManager: TVariableManager;
  _Coefs: TInt64Collection; _b: Int64; _OrigSum: TPBSum;
  _Modulo: Integer);
var
  i: Integer;

begin
  inherited;

  SetLength (Memory, OrigSum.Count+ 1);
  for i:= 0 to High (Memory) do
    Memory[i]:= TListOfLiteralCollection.Create;

end;

end.

