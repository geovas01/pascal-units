unit PBModEncoderUsingCardUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractSorterEncoderUnit, AbstractPBModEncoderUnit,
  ClauseUnit, TSeitinVariableUnit, CollectionUnit, PBConstraintUnit;

type

  { TPBModEncoderUsingCard }

  TPBModEncoderUsingCard= class (TAbstractPBModEncoder)
  private
  protected
    DP: TListOfLiteralCollection;
    Sorters: array of TAbstractSorterEncoder;
    TrueLiteralCountModModuloPerSorter: TListOfLiteralCollection;

    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function EncodePBMod: TLiteral; override;

    constructor Create(_OrigConsrraint: TPBConstraint; _VariableManager:  TVariableManager;
      _Coefs: TInt64Collection; _b: int64;
      _OrigSum: TPBSum; _Modulo: integer);
    destructor Destroy; override;

    procedure AddExtraClauses; override;

  end;

implementation
uses
  ParameterManagerUnit, SortingNetworkSorterEncoderUnit, DPBasedSorterEncoderUnit,
  DCBasedSorterEncoderUnit, Math;

{ TPBModEncoderUsingCard }

procedure TPBModEncoderUsingCard.AddExtraClauses_Medium;
var
  n1, b1, b2: Integer;

begin
  if VariableGenerator.SimulationMode then
    Exit;


  for n1:= 1 to Modulo- 1 do
  begin
    VariableGenerator.SatSolver.BeginConstraint;

    for b1:= 0 to Modulo- 1 do
      if GetVar (DP.Item [n1].Items [b1])<> 0 then
        VariableGenerator.SatSolver.AddLiteral (DP.Item [n1].Items [b1]);

    VariableGenerator.SatSolver.SubmitClause; {DP [i][0] or DP [i][0] or ... DP [i][Modulo- 1]}

  end;

  for n1:= 1 to Modulo- 1 do
    for b1:= 0 to Modulo- 1 do
      if GetVar (DP.Item [n1].Items [b1])<> 0 then
        for b2:= b1+ 1 to Modulo- 1 do
          if (GetVar (DP.Item [n1].Items [b2])<> 0) then
          begin
            VariableGenerator.SatSolver.BeginConstraint;

            VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Items [b1]));
            VariableGenerator.SatSolver.AddLiteral (NegateLiteral (DP.Item [n1].Items [b2]));

            VariableGenerator.SatSolver.SubmitClause; {DP [n1][b1]=> \lnot DP [n1][b2]}

          end;

end;

procedure TPBModEncoderUsingCard.AddExtraClauses_High;
begin
  if VariableGenerator.SimulationMode then
    Exit;

end;

function TPBModEncoderUsingCard.EncodePBMod: TLiteral;

  function Encode (Index: integer; b: integer;
    TrueLiteralCountModModuloPerSorter: TListOfLiteralCollection): TLiteral;
{
 Used by EncodeModularityConstraint_CardDP, EncodeModularityConstraint_CardSN,
   and EncodeModularityConstraint_CardDC
}
  var
    TempLiterals: TLiteralCollection;
    k: integer;

  begin
    if GetVar (Dp.Item [Index].Items [b])<> 0 then
      Exit (Dp.Item [Index].Items [b]);

    if Index= 0 then
      if b= 0 then
        Exit (VariableGenerator.TrueLiteral)
      else
        Exit (VariableGenerator.FalseLiteral);

    TempLiterals:= TLiteralCollection.Create;

    for k:= 0 to Modulo- 1 do
      if TrueLiteralCountModModuloPerSorter.Item [Index].Items [k]<>
          VariableGenerator.FalseLiteral then
        TempLiterals.PushBack (
          VariableGenerator.CreateVariableDescribingAND  (
              Encode (
                        Index- 1, (b+ k* (Modulo- Index)) mod Modulo,
                        TrueLiteralCountModModuloPerSorter
                      ),
              TrueLiteralCountModModuloPerSorter.Item [Index].Items [k]
                                                         )
                     );

    Result:= VariableGenerator.CreateVariableDescribingOR (TempLiterals,
          TempLiterals.Count);

    Dp.Item [Index].Items [b]:= Result;

    TempLiterals.Free;

  end;

  function CreateSorterModModulo (InputLiterals: TLiteralCollection): TAbstractSorterEncoder;
  var
    DPVarCount, DCVarCount, SNVarCount: Integer;
    Winner: AnsiString;

  begin
     if InputLiterals.Count= 0 then
       Result:= TAbstractSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals)
     else if UpperCase (GetRunTimeParameterManager.GetValueByName ('--ModularEncoder'))= UpperCase ('Card.SN') then
       Result:= TSortingNetworkSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals)
     else if UpperCase (GetRunTimeParameterManager.GetValueByName ('--ModularEncoder'))= UpperCase ('Card.DP') then
        Result:= TDPBasedSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals)
     else if UpperCase (GetRunTimeParameterManager.GetValueByName ('--ModularEncoder'))= UpperCase ('Card.DC') then
        Result:= TDCBasedSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals)
     else if UpperCase (GetRunTimeParameterManager.GetValueByName ('--ModularEncoder'))= UpperCase ('Less.Variable') then
     begin
       VariableGenerator.SetSimulationMode;

       SNVarCount:= VariableGenerator.LastUsedCNFIndex;
       Result:= nil;
       Result:= TSortingNetworkSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals);
       SNVarCount:= VariableGenerator.LastUsedCNFIndex- SNVarCount;
       Result.Free;
       VariableGenerator.ResetSimulationMode;

       VariableGenerator.SetSimulationMode;

       DPVarCount:= VariableGenerator.LastUsedCNFIndex;
       Result:= TDPBasedSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals);
       DPVarCount:= VariableGenerator.LastUsedCNFIndex- DPVarCount;
       Result.Free;
       VariableGenerator.ResetSimulationMode;

       VariableGenerator.SetSimulationMode;

       DCVarCount:= VariableGenerator.LastUsedCNFIndex;
       Result:= TDCBasedSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals);
       DCVarCount:= VariableGenerator.LastUsedCNFIndex- DCVarCount;
       Result.Free;
       VariableGenerator.ResetSimulationMode;

       Winner:= '';
       if SNVarCount= Min (Min (SNVarCount, DPVarCount), DCVarCount) then
       begin
         Result:= TSortingNetworkSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals);
         Winner:= 'SN';

       end
       else if DPVarCount= Min (Min (SNVarCount, DPVarCount), DCVarCount) then
       begin
         Result:= TDPBasedSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals);
         Winner:= 'DP';

       end
       else if DCVarCount= Min (Min (SNVarCount, DPVarCount), DCVarCount) then
       begin
         Result:= TDCBasedSorterEncoder.Create (VariableGenerator, Modulo, InputLiterals);
         Winner:= 'DC';

       end;

       if (GetRunTimeParameterManager.Verbosity and Ord (vbFull))<> 0 then
         WriteLn ('The winner for Comparsion network is: ', Winner);

    end;

 end;

var
  i, j: Integer;
  LiteralsBasedOnRemainder: TListOfLiteralCollection;

begin
  LiteralsBasedOnRemainder:= TListOfLiteralCollection.Create (Modulo);

  for i:= 0 to OrigSum.Count- 1 do
    if Coefs.Item [i] mod Modulo<> 0 then// We do not need a sorter for literals whose remainders are equal to 0.
      LiteralsBasedOnRemainder.Item [Coefs.Item [i] mod Modulo].PushBack (OrigSum.Item [i].Literal);

  SetLength (Sorters, Modulo);
  for i:= 0 to Modulo- 1 do
    Sorters [i]:= CreateSorterModModulo (LiteralsBasedOnRemainder.Item [i]);

  LiteralsBasedOnRemainder.Free;

  TrueLiteralCountModModuloPerSorter:= TListOfLiteralCollection.Create;
  for i:= 0 to Modulo- 1 do
    TrueLiteralCountModModuloPerSorter.AddItem (Sorters [i].Encode);

  Dp:= TListOfLiteralCollection.Create (Modulo);
//Dp[i][j]= T <=> there is an assignment to x_k where a_k mod Modulo <= i s.t. the left-hand side can be equal to j

  for i:= 0 to Modulo- 1 do
  begin
    Dp.Item [i].Count:= Modulo;
    for j:= 0 to Modulo- 1 do
      Dp.Item [i].Items [j]:= 0;

  end;

  Dp.Item [0].Items [0]:= VariableGenerator.TrueLiteral;
  for i:= 1 to Modulo- 1 do
    Dp.Item [0].Items [i]:= VariableGenerator.FalseLiteral;

  for i:= 0 to Modulo- 1 do
    Encode (Modulo- 1, i, TrueLiteralCountModModuloPerSorter);

  Result:= Encode (Modulo- 1, b, TrueLiteralCountModModuloPerSorter);

end;

destructor TPBModEncoderUsingCard.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Modulo- 1 do
    Sorters [i].Free;
  SetLength (Sorters, 0);
  Dp.Free;
  TrueLiteralCountModModuloPerSorter.Free;

  inherited Destroy;

end;

procedure TPBModEncoderUsingCard.AddExtraClauses;
var
  i: Integer;

begin
  inherited AddExtraClauses;

  for i:= 0 to Modulo- 1 do
    Sorters [i].AddExtraClauses;

end;

constructor TPBModEncoderUsingCard.Create (_OrigConsrraint: TPBConstraint; _VariableManager: TVariableManager;
  _Coefs: TInt64Collection; _b: int64; _OrigSum: TPBSum;
  _Modulo: integer);
var
  i: Integer;

begin
  inherited;

  DP:= nil;
  SetLength (Sorters, Modulo);
  for i:= 0 to Modulo- 1 do
    Sorters [i]:= nil;
  TrueLiteralCountModModuloPerSorter:= nil;

end;

end.

