unit DPBasedSorterEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractSorterEncoderUnit,
    ClauseUnit;

type
  { TDPBasedSorterEncoder }

  TDPBasedSorterEncoder= class (TAbstractSorterEncoder)
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
  TSeitinVariableUnit;

{ TDPBasedSorterEncoder }

procedure TDPBasedSorterEncoder.AddExtraClauses_Medium;
var
 n1, b1, b2: Integer;

begin

  for n1:= 0 to InputLiterals.Count- 1 do
    for b1:= 0 to Modulo- 1 do
      if GetVar (Dp.Item [n1].Item [b1])<> 0 then
        for b2:= b1+ 1 to Modulo- 1 do
          if GetVar (Dp.Item [n1].Item [b2])<> 0 then
          begin
            VariableManager.SatSolver.BeginConstraint;

            VariableManager.SatSolver.AddLiteral (NegateLiteral (Dp.Item [n1].Item [b1]));
            VariableManager.SatSolver.AddLiteral (NegateLiteral (Dp.Item [n1].Item [b2]));

            VariableManager.SatSolver.SubmitClause;// Dp[n1][b1]=> \lnot Dp[n1][b2]

          end;

 for n1:= 0 to InputLiterals.Count- 1 do
 begin
   VariableManager.SatSolver.BeginConstraint;

   for b1:= 0 to Modulo- 1 do
     if GetVar (Dp.Item [n1].Item [b1])<> 0 then
       VariableManager.SatSolver.AddLiteral (Dp.Item [n1].Item [b1]);

   VariableManager.SatSolver.SubmitClause;// DP [n1][0] or  DP [n1][1] or ... or DP [n1][modulo- 1]

 end;

end;


procedure TDPBasedSorterEncoder.AddExtraClauses_High;
begin
 //Do nothing

end;

function TDPBasedSorterEncoder.Encode: TLiteralCollection;

  function _CreateDPSorterModModulo (Index: Integer; Count: Integer): TLiteral;
  var
    D_c, D_c__1: TLiteral;
    l1, l2: TLiteral;
    LitValue: TGroundBool;

  begin

    if Count< 0 then
      Exit (VariableManager.FalseLiteral);
    if Index+ 1< Count then
      Exit (VariableManager.FalseLiteral);

    if Index= -1 then
      if Count= 0 then
        Exit (VariableManager.TrueLiteral)
      else
        Exit (VariableManager.FalseLiteral);

    if GetVar (Dp.Item [Index].Item [Count])<> 0 then
      Exit (Dp.Item [Index].Item [Count]);

    LitValue:= VariableManager.SatSolver.GetLiteralValue (InputLiterals.Item [Index]);

    if LitValue= gbTrue then
    begin
      D_c__1:= _CreateDPSorterModModulo (Index- 1, (Count+ Modulo- 1) mod Modulo);
      Result:= D_c__1;

    end
    else if LitValue= gbFalse then
    begin
      D_c:= _CreateDPSorterModModulo (Index- 1, Count);
      Result:= D_c;

    end
    else// LitValue= gbUnknown
    begin
      D_c__1:= _CreateDPSorterModModulo (Index- 1, (Count+ Modulo- 1) mod Modulo);
      l1:= VariableManager.CreateVariableDescribingAND (
                                    InputLiterals.Item [Index],
                                    D_c__1
                                     );

      D_c:= _CreateDPSorterModModulo (Index- 1, Count);
      l2:= VariableManager.CreateVariableDescribingAND (
                                     NegateLiteral (InputLiterals.Item [Index]),
                                     D_c
                                      );

      Result:= VariableManager.CreateVariableDescribingOR (l1, l2);

    end;

    Dp.Item [Index].Item [Count]:= Result;

  end;

var
  i, j: Integer;

begin
  Result:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);
  Dp:= TListOfLiteralCollection.Create (InputLiterals.Count);

  for i:= 0 to InputLiterals.Count- 1 do
  begin
    Dp.Item [i].Count:= Modulo;
    for j:= 0 to Modulo- 1 do
      Dp.Item [i].Item [j]:= 0;

  end;

  for i:= 0 to Modulo- 1 do
    Result.Item [i]:= _CreateDPSorterModModulo (InputLiterals.Count- 1, i);

end;

destructor TDPBasedSorterEncoder.Destroy;
begin
  DP.Free;

  inherited Destroy;
end;

end.

