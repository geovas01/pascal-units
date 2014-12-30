unit DPBasedSorterEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractSorterEncoderUnit,
    ClauseUnit;

type
  { TDPBasedSorterEncoder }

  TDPBasedSorterEncoder= class(TAbstractSorterEncoder)
  private
    DP: TListOfLiteralCollection;

  protected
    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function Encode: TLiteralCollection; override;

    destructor Destroy; override;

  end;


implementation
uses
  TSeitinVariableUnit, Math;

{ TDPBasedSorterEncoder }

procedure TDPBasedSorterEncoder.AddExtraClauses_Medium;
var
 n1, b1: Integer;

begin

  for n1:= 1 to InputLiterals.Count- 1 do
    for b1:= 0 to Modulo- 1 do
      if GetVar(Dp.Item[n1].Items[b1])<> 0 then
      begin
        VariableManager.SatSolver.BeginConstraint;

      (*
      (\lnot D{n-1}_c \land \lnot D^{n-1}_{c- 1}) => \lnot D^n_c
       *)
        VariableManager.SatSolver.AddLiteral(Dp.Item[n1- 1].Items[b1]);
        VariableManager.SatSolver.AddLiteral(Dp.Item[n1- 1].Items[(b1+ Modulo- 1) mod Modulo]);
        VariableManager.SatSolver.AddLiteral(NegateLiteral(Dp.Item[n1].Items[b1]));

        VariableManager.SatSolver.SubmitClause;// Dp[n1][b1]=> \lnot Dp[n1][b2]

      end;

end;


procedure TDPBasedSorterEncoder.AddExtraClauses_High;
begin
 //Do nothing

end;

function TDPBasedSorterEncoder.Encode: TLiteralCollection;

  function _CreateDPSorterModModulo(Index: Integer; Count: Integer): TLiteral;
  var
    D_c, D_c__1: TLiteral;
    l1, l2: TLiteral;
    LitValue: TGroundBool;

  begin

    if Count< 0 then
      Exit(VariableManager.FalseLiteral);
    if Index+ 1< Count then
      Exit(VariableManager.FalseLiteral);

    if Index= -1 then
      if Count= 0 then
        Exit(VariableManager.TrueLiteral)
      else
        Exit(VariableManager.FalseLiteral);

    if GetVar(Dp.Item[Index].Items[Count])<> 0 then
      Exit(Dp.Item[Index].Items[Count]);

    LitValue:= VariableManager.SatSolver.GetLiteralValue(InputLiterals.Items[Index]);

    if LitValue= gbTrue then
    begin
      D_c__1:= _CreateDPSorterModModulo(Index- 1,(Count+ Modulo- 1) mod Modulo);
      Result:= D_c__1;

    end
    else if LitValue= gbFalse then
    begin
      D_c:= _CreateDPSorterModModulo(Index- 1, Count);
      Result:= D_c;

    end
    else// LitValue= gbUnknown
    begin
      D_c__1:= _CreateDPSorterModModulo(Index- 1,(Count+ Modulo- 1) mod Modulo);
      l1:= VariableManager.CreateVariableDescribingAND(
                                    InputLiterals.Items[Index],
                                    D_c__1
                                     );

      D_c:= _CreateDPSorterModModulo(Index- 1, Count);
      l2:= VariableManager.CreateVariableDescribingAND(
                                     NegateLiteral(InputLiterals.Items[Index]),
                                     D_c
                                      );

      Result:= VariableManager.CreateVariableDescribingOR(l1, l2);

    end;

    Dp.Item[Index].Items[Count]:= Result;

  end;

  function EncodeUsingTseitin: TLiteralCollection;
  {//Translation using Tseitin transformation}
  var
    i, j: Integer;

  begin
    Result:= TLiteralCollection.Create(Modulo, GetVariableManager.FalseLiteral);
    Dp:= TListOfLiteralCollection.Create(InputLiterals.Count);

    for i:= 0 to InputLiterals.Count- 1 do
    begin
      Dp.Item[i].Count:= Modulo;
      for j:= 0 to Modulo- 1 do
        Dp.Item[i].Items[j]:= 0;

    end;

    for i:= 0 to Modulo- 1 do
      Result.Items[i]:= _CreateDPSorterModModulo(InputLiterals.Count- 1, i);
  end;

  function EncodeDirectly: TLiteralCollection;
  var
    i, c: Integer;
    xi, Di_1__c_1, Di_1__c, Di_c: TLiteral;

  begin
    Result:= TLiteralCollection.Create(Modulo, GetVariableManager.FalseLiteral);
    Dp:= TListOfLiteralCollection.Create(InputLiterals.Count);

  {Direct translation}
    for i:= 0 to InputLiterals.Count- 1 do
    begin
      Dp.Item[i].Count:= Modulo;
      for c:= 0 to Modulo- 1 do
        Dp.Item[i].Items[c]:= VariableManager.FalseLiteral;

    end;

    DP.Item[0].Items[0]:= NegateLiteral(InputLiterals.Items[0]);
    DP.Item[0].Items[1]:= InputLiterals.Items[0];

    for i:= 1 to InputLiterals.Count- 1 do
      for c:= 0 to Min(i+ 1, Modulo- 1) do
        DP.Item[i].Items[c]:= CreateLiteral(VariableManager.CreateNewVariable, False);

    for i:= 1 to InputLiterals.Count- 1 do
    begin
      xi:= InputLiterals.Items[i];

      for c:= 0 to Min(i+ 1, Modulo- 1) do
      begin
        Di_1__c_1:= DP.Item[i- 1].Items[(c+ Modulo- 1) mod Modulo];
        Di_1__c:= DP.Item[i- 1].Items[c];
        Di_c:= DP.Item[i].Items[c];

       (*
        D^n_c=(D^{n-1}_c \land \lnot x_n) \lor(D^{n-1}_{c-1} \land x_n)
        *)

       (*(D^{n-1}_c \land \lnot x_n) => D^n_c*)
        VariableManager.SatSolver.BeginConstraint;
        VariableManager.SatSolver.AddLiteral(NegateLiteral(Di_1__c));
        VariableManager.SatSolver.AddLiteral(xi);
        VariableManager.SatSolver.AddLiteral(Di_c);
        VariableManager.SatSolver.SubmitClause;

       (*(D^{n-1}_{c-1} \land x_n) => D^n_c*)
        VariableManager.SatSolver.BeginConstraint;
        VariableManager.SatSolver.AddLiteral(NegateLiteral(Di_1__c_1));
        VariableManager.SatSolver.AddLiteral(NegateLiteral(xi));
        VariableManager.SatSolver.AddLiteral(Di_c);
        VariableManager.SatSolver.SubmitClause;

       (*(\lnot D^{n-1}_{c} \land \lnot x_n) => \lnot D^n_c*)
        VariableManager.SatSolver.BeginConstraint;
        VariableManager.SatSolver.AddLiteral(Di_1__c);
        VariableManager.SatSolver.AddLiteral(xi);
        VariableManager.SatSolver.AddLiteral(NegateLiteral(Di_c));
        VariableManager.SatSolver.SubmitClause;

       (*(\lnot D^{n-1}_{c-1} \land x_n) => \lnot D^n_c*)
        VariableManager.SatSolver.BeginConstraint;
        VariableManager.SatSolver.AddLiteral(Di_1__c_1);
        VariableManager.SatSolver.AddLiteral(NegateLiteral(xi));
        VariableManager.SatSolver.AddLiteral(NegateLiteral(Di_c));
        VariableManager.SatSolver.SubmitClause;

      end;

    end;

    i:= InputLiterals.Count- 1;
    for c:= 0 to Modulo- 1 do
      Result.Items[c]:= DP.Item[i].Items[c];

  end;

begin
//  Result:= EncodeUsingTseitin;
  Result:= EncodeDirectly;

end;

destructor TDPBasedSorterEncoder.Destroy;
begin
  DP.Free;

  inherited Destroy;
end;

end.

