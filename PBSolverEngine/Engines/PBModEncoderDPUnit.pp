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
  ParameterManagerUnit;
{ TPBModEncoderDP }

procedure TPBModEncoderDP.AddExtraClauses_Medium;
var
  n1, b1: Integer;

begin

  for n1:= 0 to OrigSum.Count- 1 do
  begin
    VariableGenerator.SatSolver.BeginConstraint;

    for b1:= 0 to Modulo- 1 do
      if GetVar (DP.Item [n1].Items[b1])<> 0 then
        VariableGenerator.SatSolver.AddLiteral (DP.Item [n1].Items[b1]);

    Write ('( ');
    for b1:= 0 to Modulo- 1 do
      if GetVar (DP.Item [n1].Items[b1])<> 0 then
        Write (LiteralToString (DP.Item [n1].Items[b1]), ' ');
    WriteLn (')');

    VariableGenerator.SatSolver.SubmitClause; {DP [i][0] or DP [i][0] or ... DP [i][Modulo- 1]}

  end;

  for n1:= 1 to OrigSum.Count- 1 do
  begin

    for b1:= 0 to Modulo- 1 do
      if (GetVar (DP.Item [n1].Items[b1])<> 0) and
         (GetVar (DP.Item [n1- 1].Items[(b1+ Modulo- Coefs.Item [n1]) mod Modulo])<> 0) and
         (GetVar (DP.Item [n1- 1].Items[b1])<> 0) then
      begin
        VariableGenerator.SatSolver.BeginConstraint;

        VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Items[b1]));
        VariableGenerator.SatSolver.AddLiteral (DP.Item [n1- 1].Items[b1]);
        VariableGenerator.SatSolver.AddLiteral (DP.Item [n1- 1].Items[(b1+ Modulo- Coefs.Item [n1]) mod Modulo]); (*  D^i_b=> D^{i-1}_b \lor D^{i-1}_{b-ci}  *)

        VariableGenerator.SatSolver.SubmitClause;

      end;


  end;

    for n1:= 0 to OrigSum.Count- 1 do
      for b1:= 0 to Modulo- 2 do
        if GetVar (DP.Item [n1].Items [b1])<> 0 then
  //        for b2:= b1+ 1 to Modulo- 1 do
            if GetVar (DP.Item [n1].Items [b1+ 1])<> 0 then
            begin
              VariableGenerator.SatSolver.BeginConstraint;

              VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Items [b1]));
              VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Items [b1+ 1]));

              VariableGenerator.SatSolver.SubmitClause; {DP [n1][b1]=> \lnot DP [n1][b1+ 1]}

            end;

      for n1:= 0 to OrigSum.Count- 1 do
        for b1:= 1 to Modulo- 1 do
          if GetVar (DP.Item [n1].Items [b1])<> 0 then
    //        for b2:= b1+ 1 to Modulo- 1 do
              if GetVar (DP.Item [n1].Items [b1- 1])<> 0 then
              begin
                VariableGenerator.SatSolver.BeginConstraint;

                VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Items [b1]));
                VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Items [b1- 1]));

                VariableGenerator.SatSolver.SubmitClause; {DP [n1][b1]=> \lnot DP [n1][b1- 1]}

              end;

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
(*    D^i_b \land D^{i-1}_b \land \lnot D^{i-1}_{b-ci}=> \lnot xi
  D^i_b \land \lnot D^{i-1}_b \land D^{i-1}_{b-ci}=> xi
 *)

  //  DoNothing
(*
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
*)

end;

function TPBModEncoderDP.EncodePBMod: TLiteral;

  function RecEncodeUsingTseitin (Index: Integer; b: Integer): TLiteral;
  var
    D__b_c_i, D__b: TLiteral;
    xi: TLiteral;
    l1, l2: TLiteral;
    LitValue: TGroundBool;

  begin

    if Index= -1 then
      if b= 0 then
        Exit (VariableGenerator.TrueLiteral)
      else
        Exit (VariableGenerator.FalseLiteral);

    if GetVar (Dp.Item [Index].Items [b])<> 0 then
      Exit (CopyLiteral (Dp.Item [Index].Items [b]));

    if Coefs.Item [Index]= 0 then
    begin
      Result:= RecEncodeUsingTseitin (Index- 1, b);
      Dp.Item [Index].Items [b]:= Result;
      Exit;

    end;

    xi:= OrigSum.Item [Index].Literal;
    LitValue:= CNFGenerator.GetLiteralValue (xi);

    if LitValue= gbTrue then
      Result:= RecEncodeUsingTseitin (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo)
    else if LitValue= gbFalse then
      Result:= RecEncodeUsingTseitin (Index- 1, b)
    else// LitValue= gbUnknown
    begin
//        Result= (D^{i-1}_b \land \lnot x_i) \lor (D^{i-1}_{b-ci} \land x_i)

      D__b_c_i:= RecEncodeUsingTseitin (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo);
      l1:= VariableGenerator.CreateVariableDescribingAND (
                  xi,
                  D__b_c_i);//RecEncodeUsingTseitin (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo));

      D__b:= RecEncodeUsingTseitin (Index- 1, b mod Modulo);
      l2:= VariableGenerator.CreateVariableDescribingAND (
                NegateLiteral (xi),
                D__b);

      Result:= VariableGenerator.CreateVariableDescribingOR (l1, l2);
    end;

    Dp.Item [Index].Items [b]:= Result;

  end;

  function RecEncodeDirectly (Index: Integer; b: Integer): TLiteral;
  var
    D__b_c_i, D__b: TLiteral;
    xi: TLiteral;
    LitValue: TGroundBool;

  begin

    if Index= -1 then
      if b= 0 then
        Exit (VariableGenerator.TrueLiteral)
      else
        Exit (VariableGenerator.FalseLiteral);

    xi:= OrigSum.Item [Index].Literal;

    if GetVar (Dp.Item [Index].Items [b])<> 0 then
      Exit (CopyLiteral (Dp.Item [Index].Items [b]));

    if Coefs.Item [Index]= 0 then
    begin
      Result:= RecEncodeDirectly (Index- 1, b);
      Dp.Item [Index].Items [b]:= Result;
      Exit;

    end;

    xi:= OrigSum.Item [Index].Literal;
    LitValue:= CNFGenerator.GetLiteralValue (xi);

    if LitValue= gbTrue then
      Result:= RecEncodeDirectly (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo)
    else if LitValue= gbFalse then
      Result:= RecEncodeDirectly (Index- 1, b)
    else// LitValue= gbUnknown
    begin
      (*
        Result= (D^{i-1}_b \land \lnot x_i) \lor (D^{i-1}_{b-ci} \land x_i)
        The following clauses describe result:
          1- D^{i-1}_b \land \lnot x_i => Result i.e.,  \lnot D^{i-1}_b \lor x_i \lor Result,
          2- D^{i-1}_{b-ci} \land x_i => Result i.e.,  \lnot D^{i-1}_{b-ci} \lor \lnot x_i \lor Result,
          3- Result \land xi =>D^{i-1}_{b-ci} i.e., \lnot Result \lor \lnot xi \lor D^{i-1}_{b-ci}
          4- Result \land \lnot xi =>D^{i-1}_{b} i.e., \lnot Result \lor xi \lor D^{i-1}_{b}
      *)
      D__b_c_i:= RecEncodeDirectly (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo);
      D__b:= RecEncodeDirectly (Index- 1, b mod Modulo);

      Result:= CreateLiteral (VariableGenerator.CreateNewVariable (), False);

      CNFGenerator.BeginConstraint;
      CNFGenerator.AddLiteral (NegateLiteral (D__b));
      CNFGenerator.AddLiteral (xi);
      CNFGenerator.AddLiteral (Result);
      CNFGenerator.SubmitClause;

      CNFGenerator.BeginConstraint;
      CNFGenerator.AddLiteral (NegateLiteral (D__b_c_i));
      CNFGenerator.AddLiteral (NegateLiteral (xi));
      CNFGenerator.AddLiteral (Result);
      CNFGenerator.SubmitClause;

      CNFGenerator.BeginConstraint;
      CNFGenerator.AddLiteral (D__b_c_i);
      CNFGenerator.AddLiteral (NegateLiteral (xi));
      CNFGenerator.AddLiteral (NegateLiteral (Result));
      CNFGenerator.SubmitClause;

      CNFGenerator.BeginConstraint;
      CNFGenerator.AddLiteral (D__b);
      CNFGenerator.AddLiteral (xi);
      CNFGenerator.AddLiteral (NegateLiteral (Result));
      CNFGenerator.SubmitClause;

      (*
      This commented implementation introduce two new literals l1, l2. I changed the implementation
      to obtain smaller CNF.

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
       *)
    end;

    Dp.Item [Index].Items [b]:= Result;

  end;

  procedure IterEncode (n: Integer; b: Integer);
  var
    i: Integer;
    v: Integer;
    Lit: TLiteral;
    l1, l2: TLiteral;

  begin
    {
    ToDO: Check if Permutation is already applied to Coefs or not!
    }
    i:= 0;
    Lit:= OrigSum.Literal [i];
    DP.Item [0].Items [0]:= NegateLiteral (Lit);
    v:= Coefs.Item [0];
    DP.Item [0].Items [v]:= Lit;

    for i:= 1 to n- 1 do
    begin
      Lit:= OrigSum.Literal [i];
      v:= Coefs.Item [i];
      for b:= 0 to Modulo- 1 do
      begin
        //      D__b_c_i:= Encode (Index- 1, (b- Coefs.Item [Index]+ Modulo) mod Modulo);
        l1:= VariableGenerator.CreateVariableDescribingAND (
                    Lit,
                    DP.Item [i- 1].Items [(b- v+ Modulo) mod Modulo]
                     );

        {      D__b:= Encode (Index- 1, b mod Modulo);
              LiteralCollection.Item [0]:= NegateLiteral (OrigSum.Item [Permutation.Item [Index]].Literal);
              LiteralCollection.Item [1]:= D__b;
        }
        l2:= VariableGenerator.CreateVariableDescribingAND (
                        NegateLiteral (Lit),
                        DP.Item [i- 1].Items [b mod Modulo]);

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
      Dp.Item [i].Items [j]:= 0;

  end;

  {TODO: This procedure can be rewritten using two DP array instead of Sum.Count ones}

   Result:= RecEncodeDirectly (OrigSum.Count- 1, b);

  { Full DP table is needed for this constraint is simplication of another constraint}
  if OriginalConstraint.SimplificationOf<> nil then
    for i:= 0 to Modulo- 1 do
      RecEncodeDirectly (OrigSum.Count- 1, i);

//  IterEncode (OrigSum.Count, Modulo);
//  Result:= DP.Item [OrigSum.Count].Item [b]

  WriteLn ('PBModEncoderDPUnit');
  WriteLn ('OrigSum= ', OrigSum.ToString, ' = ', b, ' (', Modulo, ')');
  for i:= 0 to OrigSum.Count- 1 do
  begin
    for j:= 0 to Dp.Item [i].Count- 1 do
      Write (LiteralToString (Dp.Item [i].Items [j]), ' ');
    WriteLn;

  end;
  WriteLn;

end;

destructor TPBModEncoderDP.Destroy;
begin
  DP.Free;

  inherited Destroy;
end;

end.

