unit PBModEncoderUsingSingleSorterUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractPBModEncoderUnit, ClauseUnit, TSeitinVariableUnit,
  CollectionUnit, PBConstraintUnit, AbstractSorterEncoderUnit;

type

  { TSNBasedEncoder }

  TPBModEncoderUsingSingleSorter= class(TAbstractPBModEncoder)
  private
    Sorter: TAbstractSorterEncoder;

    function CreateSorter(InputLiterals: TLiteralCollection): TAbstractSorterEncoder;

  protected
    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function EncodePBMod: TLiteral; override;
    procedure AddExtraClauses; override;

    constructor Create(_OrigConsrraint: TPBConstraint; _VariableManager: TVariableManager;
                        _Coefs: TInt64Collection; _b: Int64;
                        _OrigSum: TPBSum; _Modulo: Integer);
    destructor Destroy; override;

  end;

implementation
uses
  BigInt, ParameterManagerUnit, SortingNetworkSorterEncoderUnit, DPBasedSorterEncoderUnit,
   DCBasedSorterEncoderUnit, Math;

{ TSNBasedEncoder }

function TPBModEncoderUsingSingleSorter.CreateSorter(InputLiterals: TLiteralCollection): TAbstractSorterEncoder;
var
  DPVarCount, DCVarCount, SNVarCount: Integer;
  Winner: AnsiString;

begin
   if InputLiterals.Count= 0 then
     Result:= TAbstractSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals)
   else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'))= UpperCase('SingleSorter.SN') then
     Result:= TSortingNetworkSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals)
   else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'))= UpperCase('SingleSorter.DP') then
      Result:= TDPBasedSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals)
   else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'))= UpperCase('SingleSorter.DC') then
   begin
     if Modulo< 4* ln(InputLiterals.Count)/ ln(2) then
       Result:= TDCBasedSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals)
     else
       Result:= TSortingNetworkSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals);

   end
   else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'))= UpperCase('SingleSorter.Less.Variable') then
   begin
     VariableGenerator.SetSimulationMode;

     SNVarCount:= VariableGenerator.LastUsedCNFIndex;
     Result:= nil;
     Result:= TSortingNetworkSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals);
     SNVarCount:= VariableGenerator.LastUsedCNFIndex- SNVarCount;
     Result.Free;
     VariableGenerator.ResetSimulationMode;

     VariableGenerator.SetSimulationMode;

     DPVarCount:= VariableGenerator.LastUsedCNFIndex;
     Result:= TDPBasedSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals);
     DPVarCount:= VariableGenerator.LastUsedCNFIndex- DPVarCount;
     Result.Free;
     VariableGenerator.ResetSimulationMode;

     VariableGenerator.SetSimulationMode;

     DCVarCount:= VariableGenerator.LastUsedCNFIndex;
     Result:= TDCBasedSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals);
     DCVarCount:= VariableGenerator.LastUsedCNFIndex- DCVarCount;
     Result.Free;
     VariableGenerator.ResetSimulationMode;

     Winner:= '';
     if SNVarCount= Min(Min(SNVarCount, DPVarCount), DCVarCount) then
     begin
       Result:= TSortingNetworkSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals);
       Winner:= 'SN';

     end
     else if DPVarCount= Min(Min(SNVarCount, DPVarCount), DCVarCount) then
     begin
       Result:= TDPBasedSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals);
       Winner:= 'DP';

     end
     else if DCVarCount= Min(Min(SNVarCount, DPVarCount), DCVarCount) then
     begin
       Result:= TDCBasedSorterEncoder.Create(VariableGenerator, Modulo, InputLiterals);
       Winner:= 'DC';

     end;

     if(GetRunTimeParameterManager.Verbosity and Ord(vbFull))<> 0 then
       WriteLn('The winner for Comparsion network is: ', Winner);

  end
  else
    Assert(False);

end;

procedure TPBModEncoderUsingSingleSorter.AddExtraClauses_Medium;
begin
end;

procedure TPBModEncoderUsingSingleSorter.AddExtraClauses_High;
begin

end;

function TPBModEncoderUsingSingleSorter.EncodePBMod: TLiteral;
var
  InputLiterals, SorterOutput: TLiteralCollection;
  i, j: Integer;
  m, Res: TBigInt;

begin
  InputLiterals:= TLiteralCollection.Create;
  m:= BigIntFactory.GetNewMemeber.SetValue(Modulo);

  for i:= 0 to OrigSum.Count- 1 do
  begin
    Res:= OrigSum.Coef [i].Modulo(m);

    for j:= 1 to Res.GetValue do
      InputLiterals.PushBack(OrigSum.Literal [i]);

    Res.Free;

  end;

//  WriteLn(InputLiterals.ToString);
  Sorter:= CreateSorter(InputLiterals);

  InputLiterals.Free;

  SorterOutput:= Sorter.Encode;
  Result:= SorterOutput.Items[b];
  SorterOutput.Free;

  m.Free;

end;

procedure TPBModEncoderUsingSingleSorter.AddExtraClauses;
begin
  Sorter.AddExtraClauses;

end;

constructor TPBModEncoderUsingSingleSorter.Create(_OrigConsrraint: TPBConstraint; _VariableManager: TVariableManager;
  _Coefs: TInt64Collection; _b: Int64; _OrigSum: TPBSum;
  _Modulo: Integer);
begin
  inherited;

  Sorter:= nil;

end;


destructor TPBModEncoderUsingSingleSorter.Destroy;
begin
  Sorter.Free;

  inherited;

end;

end.

