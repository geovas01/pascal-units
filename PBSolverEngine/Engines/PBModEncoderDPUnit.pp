unit PBModEncoderDPUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractPBModEncoderUnit, PBConstraintUnit,
  ClauseUnit;

type
  { TPBModEncoderDP }

  TPBModEncoderDP= class (TAbstractPBModEncoder)
  private
  protected
    DP: TListOfLiteralCollection;

    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function EncodePBMod: TLiteral; override;

    destructor Destroy; override;

  end;

implementation
uses
  TSeitinVariableUnit, ParameterManagerUnit;
{ TPBModEncoderDP }

procedure TPBModEncoderDP.AddExtraClauses_Medium;
var
  n1, b1, b2: Integer;

begin

  for n1:= 0 to OrigSum.Count- 1 do
  begin
    VariableGenerator.SatSolver.BeginConstraint;

    for b1:= 0 to Modulo- 1 do
      if GetVar (DP.Item [n1].Item [b1])<> 0 then
        VariableGenerator.SatSolver.AddLiteral (DP.Item [n1].Item [b1]);

    VariableGenerator.SatSolver.SubmitClause; {DP [i][0] or DP [i][0] or ... DP [i][Modulo- 1]}

  end;
{
  for n1:= 0 to OrigSum.Count- 1 do
    for b1:= 0 to Modulo- 1 do
      if GetVar (DP.Item [n1].Item [b1])<> 0 then
        for b2:= b1+ 1 to Modulo- 1 do
          if GetVar (DP.Item [n1].Item [b2])<> 0 then
          begin
            VariableGenerator.SatSolver.BeginConstraint;

            VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Item [b1]));
            VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Item [b2]));

            VariableGenerator.SatSolver.SubmitClause; {DP [n1][b1]=> \lnot DP [n1][b2]}

          end;
}
end;

procedure TPBModEncoderDP.AddExtraClauses_High;
{
var
  Values: TBoolArray;

var
  n1, n2, b1, b2: Integer;
  Complete: Boolean;
}

begin
//  DoNothing
{
  SetLength (Values, Modulo);
  for n1:= 0 to OrigSum.Count- 1 do
    for n2:= n1+ 1 to OrigSum.Count- 1 do
    begin
       Complete:= GenerateAllPossible (n1+ 1, n2, Values);
       if Complete then
         Break;

       for b1:= 0 to Modulo- 1 do
         if GetVar (DP.Item [n1].Item [b1])<> 0 then
           for b2:= 0 to Modulo- 1 do
             if GetVar (DP.Item [n2].Item [(b1+ b2) mod Modulo])<> 0 then
               if not Values [b2] then// so, DP [n2][b1+ b2] cannot be true if DP [n1][b1] is true.
               begin
                 VariableGenerator.SatSolver.BeginConstraint;

                 VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Item [b1]));
                 VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n2].Item [(b1+ b2) mod Modulo]));

                 VariableGenerator.SatSolver.SubmitClause; {DP [n1][b1]=> \lnot DP [n1][b2]}

               end;

    end;

  SetLength (Values, 0);
}

end;

function TPBModEncoderDP.EncodePBMod: TLiteral;

  function RecEncode (Index: Integer; b: Integer): TLiteral;
  var
//    D__b_c_i, D__b: TLiteral;
    l1, l2: TLiteral;
    LitValue: TGroundBool;

  begin

    if Index= -1 then
      if b= 0 then
        Exit (VariableGenerator.TrueLiteral)
      else
        Exit (VariableGenerator.FalseLiteral);

    if GetVar (Dp.Item [Index].Item [b])<> 0 then
      Exit (CopyLiteral (Dp.Item [Index].Item [b]));

    if Coefs.Item [Index]= 0 then
    begin
      Result:= RecEncode (Index- 1, b);
      Dp.Item [Index].Item [b]:= Result;
      Exit;

    end;

    LitValue:= CNFGenerator.GetLiteralValue (OrigSum.Literal [Permutation.Item [Index]]);

    if LitValue= gbTrue then
      Result:= RecEncode (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo)
    else if LitValue= gbFalse then
      Result:= RecEncode (Index- 1, b)
    else// LitValue= gbUnknown
    begin
//      D__b_c_i:= Encode (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo);
      l1:= VariableGenerator.CreateVariableDescribingAND (
                  OrigSum.Item [Permutation.Item [Index]].Literal,
                  RecEncode (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo));

{      D__b:= Encode (Index- 1, b mod Modulo);
      LiteralCollection.Item [0]:= NegateLiteral (OrigSum.Item [Permutation.Item [Index]].Literal);
      LiteralCollection.Item [1]:= D__b;
}
      l2:= VariableGenerator.CreateVariableDescribingAND (
                NegateLiteral (OrigSum.Item [Permutation.Item [Index]].Literal),
                RecEncode (Index- 1, b mod Modulo));

{      LiteralCollection.Item [0]:= l1;
      LiteralCollection.Item [1]:= l2;
      }
      Result:= VariableGenerator.CreateVariableDescribingOR (l1, l2);

//      LiteralCollection.Free;

    end;

    Dp.Item [Index].Item [b]:= Result;

  end;

  procedure IterEncode (n: Integer; b: Integer);
  var
    i, j: Integer;
    v: Integer;
    Lit: TLiteral;
    LitValue: TGroundBool;
    l1, l2: TLiteral;

  begin
    {
    ToDO: Check if Permutation is already applied to Coefs or not!
    }
    i:= 0;
    Lit:= OrigSum.Literal [i];
    DP.Item [0].Item [0]:= NegateLiteral (Lit);
    v:= Coefs.Item [0];
    DP.Item [0].Item [v]:= Lit;

    for i:= 1 to n- 1 do
    begin
      Lit:= OrigSum.Literal [i];
      v:= Coefs.Item [i];
      for b:= 0 to Modulo- 1 do
      begin
        //      D__b_c_i:= Encode (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo);
        l1:= VariableGenerator.CreateVariableDescribingAND (
                    Lit,
                    DP.Item [i- 1].Item [(b- v+ Modulo) mod Modulo]
                     );

        {      D__b:= Encode (Index- 1, b mod Modulo);
              LiteralCollection.Item [0]:= NegateLiteral (OrigSum.Item [Permutation.Item [Index]].Literal);
              LiteralCollection.Item [1]:= D__b;
        }
        l2:= VariableGenerator.CreateVariableDescribingAND (
                        NegateLiteral (Lit),
                        DP.Item [i- 1].Item [b mod Modulo]);

        {      LiteralCollection.Item [0]:= l1;
              LiteralCollection.Item [1]:= l2;
              }
        Result:= VariableGenerator.CreateVariableDescribingOR (l1, l2);

      end;

    end;

  end;

var
  i, j: Integer;

begin

  Dp:= TListOfLiteralCollection.Create (OrigSum.Count);
  for i:= 0 to OrigSum.Count- 1 do
  begin
    Dp.Item [i].Count:= Modulo;
    for j:= 0 to Dp.Item [i].Count- 1 do
      Dp.Item [i].Item [j]:= 0;

  end;

  {TODO: This procedure can be rewritten using two DP array instead of Sum.Count ones}

  Result:= RecEncode (OrigSum.Count- 1, b);

  { Full DP table is needed for ExtraClauses ... }
  if UpperCase (GetRunTimeParameterManager.ValueByName ['--ExtraClausesLevel'])<> UpperCase ('Off') then
    for i:= 0 to Modulo- 1 do
      RecEncode (OrigSum.Count- 1, i);

//  IterEncode (OrigSum.Count, Modulo);
//  Result:= DP.Item [OrigSum.Count].Item [b]

end;

destructor TPBModEncoderDP.Destroy;
begin
  DP.Free;

  inherited Destroy;
end;

end.

