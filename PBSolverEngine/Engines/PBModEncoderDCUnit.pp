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
    TMemory: array of TListOfClauseCollection;

    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function EncodePBMod: TLiteral; override;

    destructor Destroy; override;
    constructor Create (_OrigConsrraint: TPBConstraint; _VariableManager: TVariableManager;
                          _Coefs: TInt64Collection; _b: Int64;
                          _OrigSum: TPBSum;
                           _Modulo: Integer);

  end;


implementation
uses
  ParameterManagerUnit, GenericFactoryUnit;

{ TPBModEncoderDC }

procedure TPBModEncoderDC.AddExtraClauses_Medium;
{
var
  l, i: Integer;
  ActiveAnswer: TLiteralCollection;
  b1, b2: Integer;
  Ignore: Boolean;
}
begin
  {
  These sets of clauses are already there!
  for l:= 0 to High (Memory) do
    for i:= 0 to Memory [l].Count- 1 do
      begin
        ActiveAnswer:= Memory [l].Item [i];

        for b1:= 0 to Modulo- 1 do
          if GetVar (ActiveAnswer.Item [b1])<> 0 then
            for b2:= b1+ 1 to Modulo- 1 do
              if GetVar (ActiveAnswer.Item [b2])<> 0 then
                if (ActiveAnswer.Item [b1]<> VariableGenerator.FalseLiteral) and
                   (ActiveAnswer.Item [b2]<> VariableGenerator.FalseLiteral) then
                begin
                  VariableGenerator.SatSolver.BeginConstraint;

                  VariableGenerator.SatSolver.AddLiteral (NegateLiteral (ActiveAnswer.Item [b1]));
                  VariableGenerator.SatSolver.AddLiteral (NegateLiteral (ActiveAnswer.Item [b2]));

                  VariableGenerator.SatSolver.SubmitClause;// Result[i]=> \lnot Result [j]

                end;

      end;
   }

{
  These sets of clauses are already there!
  for l:= 0 to High (Memory) do
    for i:= 0 to Memory [l].Count- 1 do
    begin
      ActiveAnswer:= Memory [l].Item [i];
      Ignore:= False;

      VariableGenerator.SatSolver.BeginConstraint;
      for b1:= 0 to Modulo- 1 do
        if ActiveAnswer.Item [b1]= VariableGenerator.FalseLiteral then
        else if ActiveAnswer.Item [b1]= VariableGenerator.TrueLiteral then
          begin
            VariableGenerator.SatSolver.AbortConstraint;
            Ignore:= True;
            Break;

          end
          else
            VariableGenerator.SatSolver.AddLiteral (ActiveAnswer.Item [b1]);

      if not Ignore then
        VariableGenerator.SatSolver.SubmitClause;// Result [0] or  Result [1] or ... or Result [Modulo- 1]

    end;
 }
end;

procedure TPBModEncoderDC.AddExtraClauses_High;
begin
// Do Nothing ..
end;

type
  TLiteralCollectionFactory= specialize TGenericFactoy<TLiteralCollection>;

function TPBModEncoderDC.EncodePBMod: TLiteral;
var
  LiteralCollectionFactory: TLiteralCollectionFactory;

  function EncodeUsingTseitin (Index: Integer; Len: Integer): TLiteralCollection;
  {
    Create an answer for InputLiterals [Index, ..., Index+ Len- 1]
  }
  var
//    Temp: TLiteralCollection;
    T: TClauseCollection;
    Left, Right: TLiteralCollection;
    i, j: Integer;

  begin

    if Len= 1 then
    begin
      Result:= TLiteralCollection.Create (Modulo, VariableGenerator.FalseLiteral);

      if Coefs.Item [Index]<> 0 then
      begin
        Result.Item [Coefs.Item [Index]]:= CopyLiteral (OrigSum.Item [Index].Literal);
        Result.Item [0]:= CopyLiteral (NegateLiteral (OrigSum.Item [Index].Literal));

      end
      else
        Result.Item [0]:= VariableGenerator.TrueLiteral;

      Memory [Len].AddItem (Result);

    end
    else
    begin
     Left:= EncodeUsingTseitin (Index, Len div 2);
      Right:= EncodeUsingTseitin (Index+ Len div 2, Len- Len div 2);

      Result:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);

//      Temp:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);
      T:= TClauseCollection.Create;
      T.Count:= Modulo;
      for i:= 0 to Modulo- 1 do
      begin
        T.Item [i]:= LiteralCollectionFactory.GetNewMemeber;
        T.Item [i].Count:= Modulo;

      end;



      for i:= 0 to Modulo- 1 do
        for j:= 0 to Modulo- 1 do
          T.Item [i].Item [j]:= VariableGenerator.CreateVariableDescribingAND
                               (
                               Left.Item [i],
                               Right.Item [j]
                               );
      for i:= 0 to Modulo- 1 do
      begin
        CNFGenerator.BeginConstraint;

        for j:= 0 to Modulo- 1 do
          CNFGenerator.AddLiteral (T.Item [i].Item [(j- i+ Modulo) mod Modulo]);

        Result.Item [i]:= VariableGenerator.CreateNewVariable;
        CNFGenerator.SubmitOrGate (Result.Item [i]);

      end;

      {
      ~T^i_0 & ~T^i_1 &...T^i_{M-1}\Rightarrow ~Left [i];
      ~T^0_j & ~T^1_j &...T^{M-1}_j\Rightarrow ~Right [j];
      }
      for i:= 0 to Modulo- 1 do{Medium mode}
      begin
        CNFGenerator.BeginConstraint;

        for j:= 0 to Modulo- 1 do
          CNFGenerator.AddLiteral (T.Item [i].Item [j]);
        CNFGenerator.AddLiteral (NegateLiteral (Left.Item [i]));
        CNFGenerator.SubmitClause;

      end;
      for j:= 0 to Modulo- 1 do
      begin
        CNFGenerator.BeginConstraint;

        for i:= 0 to Modulo- 1 do
          CNFGenerator.AddLiteral (T.Item [i].Item [j]);
        CNFGenerator.AddLiteral (NegateLiteral (Right.Item [j]));
        CNFGenerator.SubmitClause;

      end;

      for i:= 0 to Modulo- 1 do
        LiteralCollectionFactory.ReleaseMemeber (T.Item [i]);

{
      Left.Free;
      Right.Free;
}
      Memory [Len].AddItem (Result);

    end;

  end;

  function EncodeDirectly (Index: Integer; Len: Integer): TLiteralCollection;
  {
    Create an answer for InputLiterals [Index, ..., Index+ Len- 1]
  }
  var
    Left, Right: TLiteralCollection;
    T: TClauseCollection;//a Collection of LiteralCollection
    i, j, k, k1{, k2}: Integer;

  begin

    if Len= 1 then
    begin
      Result:= TLiteralCollection.Create (Modulo, VariableGenerator.FalseLiteral);

      if Coefs.Item [Index]<> 0 then
      begin
        Result.Item [Coefs.Item [Index]]:= CopyLiteral (OrigSum.Item [Index].Literal);
        Result.Item [0]:= CopyLiteral (NegateLiteral (OrigSum.Item [Index].Literal));

      end
      else
        Result.Item [0]:= VariableGenerator.TrueLiteral;

      Memory [Len].AddItem (Result);

    end
    else
    begin
      Left:= EncodeDirectly (Index, Len div 2);
      Right:= EncodeDirectly (Index+ Len div 2, Len- Len div 2);

      {
      Intrduce T^i_j<=>L [i] & R [j].

      Result [c]= \bigvee_{i=0}^M T^i_{c-i}
      }
      Result:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);
      for i:= 0 to Modulo- 1 do
        Result.Item [i]:= CreateLiteral (GetVariableManager.CreateNewVariable, False);

      T:= TClauseCollection.Create;
      T.Count:= Modulo;

      for i:= 0 to Modulo- 1 do
      begin
        T.Item [i]:= LiteralCollectionFactory.GetNewMemeber;
        T.Item [i].Count:= Modulo;

        for j:= 0 to Modulo- 1 do
        begin
          CNFGenerator.BeginConstraint;{Left [i] \land Right [k] => Result [i+ k]}
          T.Item [i].Item [j]:= CreateLiteral (GetVariableManager.CreateNewVariable, False);
          CNFGenerator.AddLiteral (Left.Item [i]);
          CNFGenerator.AddLiteral (Left.Item [j]);
          CNFGenerator.SubmitAndGate (T.Item [i].Item [j]);

        end

      end;

      {
      ~T^i_0 & ~T^i_1 &...T^i_{M-1}\Rightarrow ~Left [i];
      ~T^0_j & ~T^1_j &...T^{M-1}_j\Rightarrow ~Right [j];
      }
      for i:= 0 to Modulo- 1 do{Medium mode}
      begin
        CNFGenerator.BeginConstraint;

        for j:= 0 to Modulo- 1 do
          CNFGenerator.AddLiteral (T.Item [i].Item [j]);
        CNFGenerator.AddLiteral (NegateLiteral (Left.Item [i]));
        CNFGenerator.SubmitClause;

      end;
      for j:= 0 to Modulo- 1 do
      begin
        CNFGenerator.BeginConstraint;

        for i:= 0 to Modulo- 1 do
          CNFGenerator.AddLiteral (T.Item [i].Item [j]);
        CNFGenerator.AddLiteral (NegateLiteral (Right.Item [j]));
        CNFGenerator.SubmitClause;

      end;

      for k:= 0 to Modulo- 1 do
      begin
        CNFGenerator.BeginConstraint;{Left [i] \land Right [k] => Result [i+ k]}

        for i:= 0 to Modulo- 1 do
          CNFGenerator.AddLiteral (T.Item [i].Item [(k+ Modulo- i) mod Modulo]);
        CNFGenerator.SubmitOrGate (Result.Item [k]);

      end;

      for i:= 0 to Modulo- 1 do
        for j:= i+ 1 to Modulo- 1 do
        begin
          {Result [i]\Rightarrow \lnot Result [j]}
          CNFGenerator.BeginConstraint;
          CNFGenerator.AddLiteral (NegateLiteral (Result.Item [i]));
          CNFGenerator.AddLiteral (NegateLiteral (Result.Item [j]));
          CNFGenerator.SubmitClause;

        end;

{
      Left.Free;
      Right.Free;
      T.Free;
}
      Memory [Len].AddItem (Result);
      TMemory [Len].AddItem (T);

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
//    Temp:= EncodeDirectly (0, OrigSum.Count);
    Result:= Temp.Item [b];

  end;
  LiteralCollectionFactory.Free;

end;

destructor TPBModEncoderDC.Destroy;
var
  i: Integer;

begin

  for i:= 0 to High (Memory) do
    Memory [i].Free;
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
    Memory [i]:= TListOfLiteralCollection.Create;

end;

end.

