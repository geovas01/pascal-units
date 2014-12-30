unit AbstractMyPBSolverEngineUnit;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, AbstractPBSolverUnit, ClauseUnit, PBConstraintUnit, BigInt,
    CollectionUnit;

type
  { TAbstractMyPBSolverEngine }

  TAbstractMyPBSolverEngine= class(TAbstractPBSolverEngine)
  protected
    function GenerateModulos(m: TBigInt): TIntegerCollection; virtual; abstract;

  protected
//    function ForceLessThanForEquality(AConstraint: TPBConstraint): TLiteral;
    function EncodeModularityConstraint(AConstraint: TPBConstraint; OrigSum: TPBSum; Modulo: Integer; b: Integer): TLiteral; overload;
    function EncodeHardConstraint(AConstraint: TPBConstraint): TLiteral; override;
    function EncodeEqualityConstraint(AConstraint: TPBConstraint): TLiteral; override;

    function EncodeGreaterThanOrEqualConstraint(AConstraint: TPBConstraint): TLiteral; override;
    function EncodeLessThanOrEqualConstraint(AConstraint: TPBConstraint): TLiteral; override;


  public

    constructor Create;
    destructor Destroy; override;

  end;

procedure DebugLn(s: AnsiString);

implementation
uses
  MyPBSolverEngineUsingPrimesUnit,
  TSeitinVariableUnit, ParameterManagerUnit, SatSolverInterfaceUnit, Math,
    AbstractPBModEncoderUnit, PBModEncoderDPUnit, PBModEncoderDCUnit,
  PBModEncoderUsingCardUnit, PBModEncoderUsingAdderUnit,
  PBModEncoderUsingSingleSorterUnit;

procedure DebugLn(S: AnsiString);
begin
  WriteLn(S);

end;

{ TAbstractMyPBSolverEngine }

(*
function TAbstractMyPBSolverEngine.ForceLessThanForEquality(AConstraint: TPBConstraint;
    Modulos: TIntegerCollection): TLiteral;
var
  i, j: Integer;
  NewConstraint: TPBConstraint;
  Literals: TLiteralCollection;
  m, Res: TBigInt;
  LHS: TPBSum;
  Lit: TLiteral;

begin
  Literals:= TLiteralCollection.Create;

//  for i:= Modulos.Count- 1 downto 0 do
  begin
    LHS:= TPBSum.Create;
    //    m:= TBigInt.Create.SetValue(Modulos.Item[i]);
    m:= AConstraint.RHS.Div2.Incr;

    for j:= 0 to AConstraint.LHS.Count- 1 do
    begin
      Res:= AConstraint.LHS.Item[j].Coef.Modulo(m);

      if Res.IsZero then
        LHS.AddNewTerm(
             TTerm.Create(AConstraint.LHS.Item[j].Literal,
                           AConstraint.LHS.Item[j].Coef.Divide(m))
                          )
      else
        LHS.AddNewTerm(
             TTerm.Create(AConstraint.LHS.Item[j].Literal,
                           AConstraint.LHS.Item[j].Coef.Divide(m).Incr)
                          );

      Res.Free;

    end;

    Res:= AConstraint.RHS.Modulo(m);
    if Res.IsZero then
      NewConstraint:= TPBConstraint.Create(LHS,
              '<=', True, AConstraint.RHS.Divide(m))
    else
       NewConstraint:= TPBConstraint.Create(LHS,
             '<=', True, AConstraint.RHS.Divide(m).Incr);

    WriteLn(NewConstraint.ToString);
    Res.Free;

    VariableGenerator.SetSimulationMode;
    Lit:= EncodeHardConstraint(NewConstraint);
    VariableGenerator.ResetSimulationMode;

    if Lit<> GetVariableManager.TrueLiteral then
    begin
      if GetRunTimeParameterManager.Verbosity= ord(vbFull) then
      begin
        WriteLn('c m=', m.ToString);
        WriteLn('c Extra Constraint: ', NewConstraint.ToString);

      end;

      Lit:= EncodeHardConstraint(NewConstraint);
      Literals.AddItem(Lit);

//      Break;

    end;
    m.Free;
    NewConstraint.Free;

  end;

  Result:= VariableGenerator.CreateVariableDescribingAND(Literals);
  Literals.Free;

end;

{function TAbstractMyPBSolverEngine.ForceGreaterThanForEquality(
  AConstraint: TPBConstraint; Modulos: TIntegerCollection): TLiteral;
var
  i, j: Integer;
  NewConstraint: TPBConstraint;
  Literals: TLiteralCollection;
  m, Res: TBigInt;
  LHS: TPBSum;
  Lit: TLiteral;

begin
  Literals:= TLiteralCollection.Create;

  for i:= Modulos.Count- 1 downto 0 do
  begin
    VariableGenerator.SetSimulationMode;

    LHS:= TPBSum.Create;
    m:= TBigInt.Create.SetValue(Modulos.Item[i]);

    for j:= 0 to AConstraint.LHS.Count- 1 do
    begin
      Res:= AConstraint.LHS.Item[j].Coef.Modulo(m);

      if Res.IsZero then
        LHS.AddNewTerm(
             TTerm.Create(AConstraint.LHS.Item[j].Literal,
                           AConstraint.LHS.Item[j].Coef.Divide(m))
                          )
      else
        LHS.AddNewTerm(
             TTerm.Create(AConstraint.LHS.Item[j].Literal,
                           AConstraint.LHS.Item[j].Coef.Divide(m).Incr)
                          );

      Res.Free;

    end;

    Res:= AConstraint.RHS.Modulo(m);
    if Res.IsZero then
      NewConstraint:= TPBConstraint.Create(LHS,
              '<=', True, AConstraint.RHS.Divide(m))
    else
       NewConstraint:= TPBConstraint.Create(LHS,
             '<=', True, AConstraint.RHS.Divide(m).Incr);

    Res.Free;

    Lit:= EncodeHardConstraint(NewConstraint);
    Literals.AddItem(Lit);

    if Lit<> GetVariableManager.TrueLiteral then
    begin
      if GetRunTimeParameterManager.Verbosity= ord(vbFull) then
      begin
        WriteLn('c m=', m.ToString);
        WriteLn('c Extra Constraint: ', NewConstraint.ToString);

      end;

      VariableGenerator.ResetSimulationMode;
      Lit:= EncodeHardConstraint(NewConstraint);
      Literals.AddItem(Lit);

      Break;

    end;

    m.Free;
    NewConstraint.Free;

  end;

  Result:= VariableGenerator.CreateVariableDescribingAND(Literals);
  Literals.Free;

end;
}
*)

function TAbstractMyPBSolverEngine.EncodeModularityConstraint(AConstraint: TPBConstraint; OrigSum: TPBSum; Modulo: Integer; b: Integer): TLiteral;
var
  Coefs: TInt64Collection;
  Permutation: TIntegerCollection;

  function  EncodeModularityConstraintUsingEquality: TLiteral;
  var
    i: Integer;
    NewConstraint: TPBConstraint;
    SumOfCoefs: Integer;
    Literals: TLiteralCollection;
 
  begin
    SumOfCoefs:= 0;

    for i:= 0 to OrigSum.Count- 1 do
      SumOfCoefs+= Coefs.Item[i];

    if 10* Modulo< SumOfCoefs then
    begin
      WriteLn('c Using DP instead ...!');
      Halt(1);
//      Exit(EncodeModularityConstraint_DP);

    end;

    Literals:= TLiteralCollection.Create;

    while b<= SumOfCoefs do
    begin
      NewConstraint:= TPBConstraint.Create(OrigSum.Copy, coEquality, True, BigIntFactory.GetNewMemeber.SetValue(b));
      
      Literals.PushBack(EncodeEqualityConstraint(NewConstraint));

      NewConstraint.Free;

      b+= Modulo;

    end;

    Result:= VariableGenerator.CreateVariableDescribingOR(Literals);
    Literals.Free;

  end;

  function GenerateRandomPermutation(n: Integer): TIntegerCollection;
  var
    i: Integer;
    a, b: Integer;

  begin
    Randomize;
    Result:= TIntegerCollection.Create(n, GetVariableManager.FalseLiteral);
 
    for i:= 0 to n- 1 do
      Result.Item[i]:= i;

    if GetRunTimeParameterManager.ValueByName['--UseRandomPermutation']= UpperCase('1') then
      for i:= 0 to 2* n do
      begin
        repeat
          a:= Random(n);
          b:= Random(n);

        until a<> b;

        Result.Item[a]:= Result.Item[a] xor Result.Item[b];
        Result.Item[b]:= Result.Item[a] xor Result.Item[b];
        Result.Item[a]:= Result.Item[a] xor Result.Item[b];

      end;

  end;

var
  i: Integer;
  BA_i, Temp, BModulo: TBigInt;
  DPCost, DCCost, CardCost: Integer;
  Winner: AnsiString;
  PBModEncoder: TAbstractPBModEncoder;
  
begin
  Permutation:= GenerateRandomPermutation(OrigSum.Count);
  Coefs:= TInt64Collection.Create(OrigSum.Count, GetVariableManager.FalseLiteral);

  BModulo:= BigIntFactory.GetNewMemeber.SetValue(Modulo);
  for i:= 0 to OrigSum.Count- 1 do
  begin
    BA_i:= OrigSum.Item[Permutation.Item[i]].Coef;
//    BA_i:= Sum.Item[i].Coef;
    Temp:= BA_i.Modulo(BModulo);
    Coefs.Item[i]:= Temp.GetValue;
    BigIntFactory.ReleaseMemeber(Temp);
    Assert(Coefs.Item[i]< Modulo);
  
  end;

{  if 7< Modulo then
    Result:= EncodeModularityConstraintUsingEquality
  else}

  if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'))= UpperCase('DP') then
    PBModEncoder:= TPBModEncoderDP.Create(AConstraint, VariableGenerator,
                                      Coefs, b, OrigSum, Modulo)
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'))= UpperCase('DCDirect') then
      PBModEncoder:= TPBModEncoderDCUsingDirect.Create(AConstraint, VariableGenerator,
                                      Coefs, b, OrigSum, Modulo)
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'))= UpperCase('DCTseitin') then
      PBModEncoder:= TPBModEncoderDCUsingTseitin.Create(AConstraint, VariableGenerator,
                                      Coefs, b, OrigSum, Modulo)
  else if UpperCase(Copy(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'), 1, Length('Adder')))= UpperCase('Adder') then
  begin
     PBModEncoder:= TPBModEncoderUsingAdders.Create(AConstraint, VariableGenerator,
                                      Coefs, b, OrigSum, Modulo);
     Halt(1);
  end
  else if UpperCase(Copy(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'), 1, Length('SingleSorter')))= UpperCase('SingleSorter') then
     PBModEncoder:= TPBModEncoderUsingSingleSorter.Create(AConstraint, VariableGenerator,
                                      Coefs, b, OrigSum, Modulo)
  else if UpperCase(Copy(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'), 1, 4))= UpperCase('Card') then
     PBModEncoder:= TPBModEncoderUsingCard.Create(AConstraint, VariableGenerator,
                                      Coefs, b, OrigSum, Modulo)
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModularEncoder'))= UpperCase('Less.Variable') then
  begin
    VariableGenerator.SetSimulationMode;
    DPCost:= VariableGenerator.LastUsedCNFIndex;

    PBModEncoder:= TPBModEncoderDP.Create(AConstraint, VariableGenerator, Coefs, b, OrigSum,
                        Modulo);
    PBModEncoder.EncodePBMod;
    PBModEncoder.Free;

    DPCost:= VariableGenerator.LastUsedCNFIndex- DPCost;
    VariableGenerator.ResetSimulationMode;

    VariableGenerator.SetSimulationMode;
    DCCost:= VariableGenerator.LastUsedCNFIndex;

    PBModEncoder:= TPBModEncoderDCUsingDirect.Create(AConstraint, VariableGenerator, Coefs, b, OrigSum,
                        Modulo);
    PBModEncoder.EncodePBMod;
    PBModEncoder.Free;

    DCCost:= VariableGenerator.LastUsedCNFIndex- DCCost;
    VariableGenerator.ResetSimulationMode;

    VariableGenerator.SetSimulationMode;
    CardCost:= VariableGenerator.LastUsedCNFIndex;
    PBModEncoder:= TPBModEncoderUsingCard.Create(AConstraint, VariableGenerator, Coefs, b, OrigSum,
                        Modulo);
    CardCost:= VariableGenerator.LastUsedCNFIndex- CardCost;
    VariableGenerator.ResetSimulationMode;

    Winner:= '';
    if Min(DPCost, Min(CardCost, DCCost))= DCCost then
    begin
      PBModEncoder:= TPBModEncoderDCUsingDirect.Create(AConstraint, VariableGenerator,
                                Coefs, b, OrigSum, Modulo);
      Winner:= 'DC';

    end
    else if Min(DPCost, Min(CardCost, DCCost))= DPCost then
    begin
      PBModEncoder:= TPBModEncoderDP.Create(AConstraint, VariableGenerator,
                                Coefs, b, OrigSum, Modulo);
      Winner:= 'DP';

    end
    else if Min(DPCost, Min(CardCost, DCCost))= CardCost then
    begin
      PBModEncoder:= TPBModEncoderUsingCard.Create(AConstraint, VariableGenerator,
                                Coefs, b, OrigSum, Modulo);
      Winner:= 'UsingCard';

    end;

    if GetRunTimeParameterManager.Verbosity= ord(vbFull) then
      WriteLn('c Winner for Modulo= ', Modulo, ' is ', Winner);

  end
  else
  begin
    WriteLn('c Invalid --ModularEncoder: "', GetRunTimeParameterManager.ValueByName['--ModularEncoder'], '"');
    WriteLn('c ModularEncoder can be "DP", "DCDirect", "DCTseitin", "SN", "Card.DP", "Card.DC", "Card.SN"');
    Halt(1);

  end;


  Result:= PBModEncoder.EncodePBMod;
  if UpperCase(GetRunTimeParameterManager.GetValueByName('--ExtraClausesLevel'))
           <> UpperCase('Off') then
    PBModEncoder.AddExtraClauses;
  PBModEncoder.Free;

  BigIntFactory.ReleaseMemeber(BModulo);
  Coefs.Free;
  Permutation.Free;

end; 

function TAbstractMyPBSolverEngine.EncodeHardConstraint(AConstraint: TPBConstraint): TLiteral;

  procedure FindSums(LHS: TPBSum; TrueInteger, UnknowInteger: TBigInt);
  var
    i: Integer;

  begin
    for i:= 0 to LHS.Count- 1 do
      case CNFGenerator.GetLiteralValue(LHS.Item[i].Literal) of
        gbTrue:
          TrueInteger.Add(LHS.Item[i].Coef);
        gbUnknown:
          UnknowInteger.Add(LHS.Item[i].Coef);
      end;

  end;

{
const
  TooMany: Int64= 1<< 60;
}
{
  function CountPossibleSolutions(LHS: TPBSum; RHS: Int64; Modulo: Int64): Int64;
}
  {
  var
    Dp: array[False..True] of array of Int64;
    i, j: Integer;
    Active: Boolean;
   }
{
  begin
    WriteLn('c CountPossibleSolutions is not implemented for BigInt');
    Halt(1);
    Result:= TooMany;
}
    {
    Result:= 0;
    SetLength(Dp[False], Modulo);
    SetLength(Dp[True], Modulo);

    Active:= True;
    FillChar(Dp[Active][0], SizeOf(Dp[Active]), 0);
    Dp[Active][0]:= 1; 

    for i:= 0 to LHS.Count- 1 do
    begin
      Active:= not Active;
//      WriteLn('Coef[', i+ 1, ']=', LHS.Item[i].Coef mod Modulo);

      for j:= 0 to Modulo- 1 do
      begin
        Dp[Active][j]:= Dp[not Active][(Modulo+ j-(LHS.Item[i].Coef mod Modulo)) mod Modulo]+ 
                         Dp[not Active][j];
        if TooMany< Dp[Active][j] then
          DP[Active][j]:= TooMany;

      end;

    end;

    Result:= Dp[Active][RHS];
    SetLength(Dp[False], 0);
    SetLength(Dp[True], 0);
}
{
  end;
}

begin
  AConstraint.Finalize;

  if GetRunTimeParameterManager.Verbosity and Ord(vbFull)<> 0 then
    WriteLn('c ',AConstraint.ToString);

  if not AConstraint.RHSSign then// RHS is negative
    if AConstraint.CompareOperator= coEquality then
      Exit(GetVariableManager.FalseLiteral)
    else if AConstraint.CompareOperator= coGreaterThanOrEqual then
      Exit(GetVariableManager.TrueLiteral)
    else if AConstraint.CompareOperator= coLessThanOrEqual then
      Exit(GetVariableManager.FalseLiteral);

  if AConstraint.CompareOperator= coEquality then
    Result:= EncodeEqualityConstraint(AConstraint)
  else if AConstraint.CompareOperator= coGreaterThanOrEqual then
    Exit(EncodeGreaterThanOrEqualConstraint(AConstraint))
  else if AConstraint.CompareOperator= coLessThanOrEqual then
    Exit(EncodeLessThanOrEqualConstraint(AConstraint))

end;

constructor TAbstractMyPBSolverEngine.Create;
begin
  inherited  Create;

end;

{
function MinimizeTseitinVars(AConstraint: TPBConstraint; MinTargetSum: Extended; Primes: TIntegerCollection; SatSolver: TSatSolverInterface): TIntegerCollection;

var
  Costs: TIntegerCollection;
  TrueLiteral, FalseLiteral: TLiteral;

  function FindTheBestMinimalSet: TIntegerCollection;
  var
    IsThere: array of Boolean;
    MinCost: Integer;

    procedure Find(Index: Integer; CurrentSum: Extended; CurrentCost: Integer);
    var
      i: Integer;

    begin
      if Index= -1 then
      begin
        if(CurrentCost<= MinCost) and(MinTargetSum< CurrentSum) then
        begin
          Result.Free;
          Result:= TIntegerCollection.Create;
          MinCost:= CurrentCost;

          for i:= 0 to High(IsThere) do
            if IsThere[i] then
               Result.AddItem(i);

        end;

        Exit;

      end;

      IsThere[Index]:= False;
      Find(Index- 1, CurrentSum, CurrentCost);
      IsThere[Index]:= True;
      Find(Index- 1, CurrentSum+ ln(Primes.Item[Index]), CurrentCost+ Costs.Item[Index]);

    end;


  begin
    Result:= nil;
    SetLength(IsThere, Primes.Count);
    FillChar(IsThere[0], SizeOf(IsThere), 0);

    MinCost:= 1<< 20;
    Find (Primes.Count- 1, 0, 0);

    SetLength(IsThere, 0);

   end;


  function FindMinimalSetFast: TIntegerCollection;
  var
    i: Integer;
    CurrentSum: Extended;
    Changed: Boolean;
    MaxIndex: Integer;
    Fixed: array of Boolean;

  begin

    CurrentSum:= 0;
    SetLength(Fixed, Primes.Count);

    Result:= TIntegerCollection.Create;
    for i:= 0 to Primes.Count- 1 do
    begin
      Fixed[i]:= False;
      CurrentSum+= ln(Primes.Item[i]);
      Result.AddItem(i);
 
    end;

    Changed:= True;

    while Changed do
    begin
      Changed:= False;
      MaxIndex:= 0;

      for i:= 0 to Result.Count- 1 do
        if not Fixed[Result.Item[i]] then
          if Costs.Item[Result.Item[MaxIndex]]< Costs.Item[Result.Item[i]] then
            MaxIndex:= i;


      if MinTargetSum< CurrentSum- ln(Primes.Item[Result.Item[MaxIndex]]) then
      begin
        CurrentSum:= CurrentSum- ln(Primes.Item[Result.Item[MaxIndex]]);
        Changed:= True;
        Result.Delete(MaxIndex);

      end
      else
        Fixed[Result.Item[MaxIndex]]:= True;

    end;

   end;

   function CountNumberOfVars(AConstraint: TPBConstraint; Modulo: Integer): Integer;
   var
     Dp: TListOfLiteralCollection;
     Coefs: array of Integer;
     NVars: Integer;

     function Encode(Index: Integer; b: Integer): TLiteral;
     var
       D__b_c_i, D__b: TLiteral;
       l1, l2: TLiteral;
       LitValue: TGroundBool;
   
     begin
       if Index= -1 then
         if b= 0 then
           Exit(TrueLiteral)
         else
           Exit(FalseLiteral);
   
       if Dp.Item[Index].Item[b]<> 0 then
       begin
         Exit(Dp.Item[Index].Item[b]);

       end;
   
       if Coefs[Index]= 0 then
       begin
         Result:= Encode(Index- 1, b);
         Dp.Item[Index].Item[b]:= Result;
         Exit(Result);

       end;
   
       LitValue:= SatSolver.GetLiteralValue(AConstraint.LHS.Literal[Index]);
   
       if LitValue= gbTrue then
       begin
         D__b_c_i:= Encode(Index- 1,(b- Coefs[Index]+ Modulo) mod Modulo);
         Result:= D__b_c_i;
   
       end
       else if LitValue= gbFalse then
       begin
         D__b:= Encode(Index- 1, b);
         Result:= D__b;
   
       end
       else// LitValue= gbUnknown
       begin
         D__b_c_i:= Encode(Index- 1,(b- Coefs[Index]+ Modulo) mod Modulo);
     
         if D__b_c_i= FalseLiteral then
           l1:= FalseLiteral
         else if D__b_c_i= TrueLiteral then
           l1:= AConstraint.LHS.Literal[Index]
         else 
         begin
           Inc(NVars);
           l1:= CreateLiteral(NVars, False);

         end;
         
         D__b:= Encode(Index- 1, b mod Modulo);
         if D__b= FalseLiteral then
           l2:= FalseLiteral
         else if D__b= TrueLiteral then
           l2:= AConstraint.LHS.Literal[Index] xor 1
         else 
         begin
           Inc(NVars);
           l2:= CreateLiteral(NVars, False);

         end;

         if l1= TrueLiteral then
           Result:= TrueLiteral
         else if l2= TrueLiteral then
           Result:= TrueLiteral
         else if l1= FalseLiteral then
           Result:= l2
         else if l2= FalseLiteral then
           Result:= l1
         else
         begin
           Inc(NVars);
           Result:= CreateLiteral(NVars, False);

         end

       end;
   
       Dp.Item[Index].Item[b]:= Result;
   
     end;
   
   var
     i, j: Integer;
     BA_i, BModulo: Int64;
     Temp: Int64;
   
   begin
     TrueLiteral:= CreateLiteral(0, False);
     FalseLiteral:= CreateLiteral(0, True);
   
     Dp:= TListOfLiteralCollection.Create(AConstraint.LHS.Count);
     SetLength(Coefs, AConstraint.LHS.Count);
   
     BModulo:= Modulo;
    (*TODO: This procedure can be rewritten using two DP array insteard of Sum.Count ones*)
   
     for i:= 0 to AConstraint.LHS.Count- 1 do
     begin
       Dp.Item[i]:= TLiteralCollection.Create(Modulo);
       for j:= 0 to Dp.Item[i].Count- 1 do
         Dp.Item[i].Item[j]:= 0;
   
       BA_i:= AConstraint.LHS.Item[i].Coef;
       Temp:= BA_i mod BModulo;
       Coefs[i]:= Temp;
       Assert(Coefs[i]< Modulo);
   
     end;
   
     NVars:= 1;
     Encode(AConstraint.LHS.Count- 1, AConstraint.RHS mod Modulo);
     Result:= NVars- 1;
   
     SetLength(Coefs, 0);
     Dp.Free;
   
    end;

var
  i: Integer;
}
{
begin
  Result:= nil;
  }
{
  Costs:= TIntegerCollection.Create;


  for i:= 0 to Primes.Count- 1 do
  begin
    Costs.AddItem(CountNumberOfVars(AConstraint, Primes.Item[i]));
    WriteLn('c ', Primes.Item[i], ':', Costs.Item[i]);

  end;

  if GetRunTimeParameterManager.MinimizeTseitinVarsMethod= 'G' then
    Result:= FindMinimalSetFast
 else
    Result:= FindTheBestMinimalSet;

  Costs.Free;
}
{
end;
}

function CompareBigInts(Item1, Item2: Pointer): Integer;
begin
  Result:= -TTerm(Item1).Coef.CompareWith(TTerm(Item2).Coef);

end;

function TAbstractMyPBSolverEngine.EncodeEqualityConstraint(AConstraint: TPBConstraint): TLiteral;

  procedure ForceLessThanForEqualityByClauses(ActiveConstraint: TPBConstraint);
  type
    TBoolArray= array of Boolean;

    var
      GeneratedClausesCount: Integer;
      MaxClauseLimit: Integer;
      TargetClauseLength: Integer;

    function GenerateClauses(LHS: TPBSum; Target: TBigInt; var IsSelected: TBoolArray;
          CurrentSum: TBigInt; CurrentIndex: Integer; SelectedItemCount: Integer= 0): Boolean;
    var
      i: Integer;

    begin
      Result:= False;
      if TargetClauseLength< SelectedItemCount then
        Exit;

      if CurrentSum.CompareWith(Target)> 0 then
      begin

        Result:= True;
        if SelectedItemCount= TargetClauseLength then
        begin
          VariableGenerator.SatSolver.BeginConstraint;

//          Write('c ', GeneratedClausesCount, ':', MaxClauseLimit, ':(');
          for i:= CurrentIndex to LHS.Count- 1 do
            if IsSelected[i] then
            begin
              VariableGenerator.SatSolver.AddLiteral(NegateLiteral(LHS.Item[i].Literal));
    //          Write(LiteralToString(NegateLiteral(LHS.Item[i].Literal)), ' ')
//              Write('(', LiteralToString(lHS.Item[i].Literal), ',', lHS.Item[i].Coef.ToString, ')');

            end;
//          WriteLn(')', CurrentSum.ToString, ' ', Target.ToString);
          VariableGenerator.SatSolver.SubmitClause;

          Inc(GeneratedClausesCount);

        end
        else
          Exit;

      end
      else
      begin

        if(CurrentIndex= -1) or(SelectedItemCount= TargetClauseLength) then
          Exit(False);

        for i:= CurrentIndex downto(TargetClauseLength- SelectedItemCount)- 1 do
        begin
          IsSelected[i]:= True;
          CurrentSum.Add(LHS.Item[i].Coef);

          if GenerateClauses(LHS, Target, IsSelected, CurrentSum, i- 1, SelectedItemCount+ 1) then
          begin
            Result:= True;
            CurrentSum.Sub(LHS.Item[i].Coef);
            IsSelected[i]:= False;

          end
          else
          begin
            CurrentSum.Sub(LHS.Item[i].Coef);
            IsSelected[i]:= False;
            break;

          end;
          CurrentSum.Sub(LHS.Item[i].Coef);
          IsSelected[i]:= False;

        end;
{        IsSelected[CurrentIndex]:= False;
        CurrentSum.Sub(LHS.Item[CurrentIndex].Coef);
        GenerateClauses(LHS, Target, IsSelected, CurrentSum, CurrentIndex- 1, SelectedItemCount);
}
      end;

    end;

  var
    LHS: TPBSum;
    RHS: TBigInt;
    CurrentSum: TBigInt;
    IsSelected: TBoolArray;

  begin
    LHS:= ActiveConstraint.LHS;
    RHS:= ActiveConstraint.RHS;

    LHS.Sort(@CompareBigInts);
//Naive Method

    if(GetRunTimeParameterManager.Verbosity and Ord(vbMedium))<> 0 then
    begin
      WriteLn('c LHS.Sort =', LHS.ToString);
      WriteLn('c RHS=', RHS.ToString);

    end;

    SetLength(IsSelected, LHS.Count);
    FillChar(IsSelected[0], SizeOf(IsSelected), 0);

    CurrentSum:= BigIntFactory.GetNewMemeber.SetValue(0);

    GeneratedClausesCount:= 0;
    MaxClauseLimit:= Sqr(LHS.Count)* LHS.Count;
    for TargetClauseLength:= 2 to 10 do
    begin
      GenerateClauses(LHS, RHS, IsSelected, CurrentSum, LHS.Count- 1);
      if MaxClauseLimit< GeneratedClausesCount then
        break;

    end;

  end;

  procedure ForceGreaterThanForEqualityByClauses(ActiveConstraint: TPBConstraint);
  type
    TBoolArray= array of Boolean;

    var
      GeneratedClausesCount: Integer;
      MaxClauseLimit: Integer;
      TargetClauseLength: Integer;

    function GenerateClauses(LHS: TPBSum; Target: TBigInt; var IsSelected: TBoolArray;
          CurrentSum: TBigInt; CurrentIndex: Integer; SelectedItemCount: Integer= 0): Boolean;
    var
      i: Integer;

    begin
      if TargetClauseLength< SelectedItemCount then
        Exit(False);

      if CurrentSum.CompareWith(Target)> 0 then
      begin

        Result:= True;

        if SelectedItemCount= TargetClauseLength then
        begin
          VariableGenerator.SatSolver.BeginConstraint;

//          Write('c ', GeneratedClausesCount, ':', MaxClauseLimit, ':(');
          for i:= CurrentIndex to LHS.Count- 1 do
            if IsSelected[i] then
            begin
              VariableGenerator.SatSolver.AddLiteral(LHS.Item[i].Literal);
    //          Write(LiteralToString(LHS.Item[i].Literal), ' ')
//              Write('(', LiteralToString(lHS.Item[i].Literal), ',', lHS.Item[i].Coef.ToString, ')');

            end;
//          WriteLn(')', CurrentSum.ToString, ' ', Target.ToString);
          VariableGenerator.SatSolver.SubmitClause;

          Inc(GeneratedClausesCount);

        end;

      end
      else
      begin

        if(CurrentIndex= -1) or(SelectedItemCount= TargetClauseLength) then
          Exit(False);

        IsSelected[CurrentIndex]:= True;
        CurrentSum.Add(LHS.Item[CurrentIndex].Coef);
        if not GenerateClauses(LHS, Target, IsSelected, CurrentSum, CurrentIndex- 1, SelectedItemCount+ 1) then
          Exit(False);

        IsSelected[CurrentIndex]:= False;
        CurrentSum.Sub(LHS.Item[CurrentIndex].Coef);
        if not GenerateClauses(LHS, Target, IsSelected, CurrentSum, CurrentIndex- 1, SelectedItemCount) then
          Exit(False);
        Result:= True;

      end;

    end;

  var
    LHS: TPBSum;
    Target: TBigInt;
    CurrentSum: TBigInt;
    IsSelected: TBoolArray;

  begin
    LHS:= ActiveConstraint.LHS;
    Target:= LHS.SumOfCoefs.Sub(ActiveConstraint.RHS);

    LHS.Sort(@CompareBigInts);
//Naive Method

    if(GetRunTimeParameterManager.Verbosity and Ord(vbMedium))<> 0 then
      WriteLn('c LHS.Sort =', LHS.ToString);

    SetLength(IsSelected, LHS.Count);
    FillChar(IsSelected[0], SizeOf(IsSelected), 0);

    CurrentSum:= BigIntFactory.GetNewMemeber.SetValue(0);

    GeneratedClausesCount:= 0;
    MaxClauseLimit:= Sqr(LHS.Count)* LHS.Count;
    for TargetClauseLength:= 2 to 10 do
    begin
      GenerateClauses(LHS, Target, IsSelected, CurrentSum, LHS.Count- 1);
      if MaxClauseLimit< GeneratedClausesCount then
        break;

    end;

    Target.Free;

  end;

  procedure ForceLessThanForEqualityByCardinalityConstraints(ActiveConstraint: TPBConstraint);
  begin
    Assert(False);

  end;

  procedure ForceGreaterThanForEqualityByCardinalityConstraints(ActiveConstraint: TPBConstraint);
  begin
    Assert(False);

  end;

var
  ActiveConstraint: TPBConstraint;
  i, j: Integer;
  EncodingResult: TLiteral;
  Literals: TLiteralCollection;
  Residue: Integer;
  bp, bResidue: Int64;
  BigIntbp, Temp,
  SumOfCoefs: TBigInt;
//  CandidatePrimeIndices: TIntegerCollection;
  Modulos: TIntegerCollection;

begin
//  ActiveConstraint:= SimplifyEqualityConstraint(AConstraint);

  SumOfCoefs:= AConstraint.LHS.SumOfCoefs;

  if SumOfCoefs.CompareWith(AConstraint.RHS)< 0 then
  begin
    Result:= VariableGenerator.FalseLiteral;
    BigIntFactory.ReleaseMemeber(SumOfCoefs);
    Exit;

  end
  else if SumOfCoefs.CompareWith(AConstraint.RHS)= 0 then
  begin

    Result:= CreateLiteral(VariableGenerator.CreateNewVariable(vpFalse, True), False);

    CNFGenerator.BeginConstraint;
    for i:= 0 to AConstraint.LHS.Count- 1 do
      CNFGenerator.AddLiteral(AConstraint.LHS.Literal[i]);
    CNFGenerator.SubmitAndGate(Result);

    BigIntFactory.ReleaseMemeber(SumOfCoefs);
    Exit;

  end;

  ActiveConstraint:= AConstraint.Copy;

  if ActiveConstraint= nil then
  begin
    if(GetRunTimeParameterManager.Verbosity and Ord(vbMedium))<> 0 then
      WriteLn('c ActiveConstraint= nil');
    Exit(VariableGenerator.FalseLiteral);

  end;

  if(GetRunTimeParameterManager.Verbosity and Ord(vbMedium))<> 0 then
    WriteLn('c', ActiveConstraint.ToString);

//  SumOfCoefs:= ActiveConstraint.LHS.SumOfCoefs;

  Modulos:= GenerateModulos(SumOfCoefs);
  BigIntFactory.ReleaseMemeber(SumOfCoefs);

  if(GetRunTimeParameterManager.Verbosity and Ord(vbFull))<> 0 then
  begin
    WriteLn('c Modulos are:');
    Write('c ');
    for j:= 0 to Modulos.Count- 1 do
      Write(Modulos.Item[j], ' ');
   WriteLn;

  end;

  if(GetRunTimeParameterManager.Verbosity and Ord(vbFull))<> 0 then
    CNFGenerator.ReportForcedVariables;

  Literals:= TLiteralCollection.Create;
  {The main loop}

  for i:= 0 to Modulos.Count- 1 do
  begin
    bp:= Modulos.Item[i];

    if GetRunTimeParameterManager.Verbosity<> 0 then
      WriteLn('c ', bp);

    BigIntbp:= BigIntFactory.GetNewMemeber.SetValue(bp);
    Temp:= ActiveConstraint.RHS.Modulo(BigIntbp);
    bResidue:= Temp.GetValue;
    BigIntFactory.ReleaseMemeber(Temp);
    BigIntFactory.ReleaseMemeber(BigIntbp);

    Residue:= bResidue;

    if 0< i then
      VariableGenerator.DecisionForNewVariable:= GetRunTimeParameterManager.GetValueByName('--DecisionVarMinization')= '1';

    EncodingResult:= EncodeModularityConstraint(AConstraint, ActiveConstraint.LHS, Modulos.Item[i], Residue);

    SatSolverInterfaceUnit.GetSatSolver.AddComment('Literal for '+ IntToStr(bp)+ ':'+ LiteralToString(EncodingResult));
    Literals.PushBack(EncodingResult);

    if EncodingResult= GetVariableManager.FalseLiteral then
    begin
      WriteLn('c Unsatifiable modulo ', bp);
      Break;

    end;

  end;

  Result:= VariableGenerator.CreateVariableDescribingAND(Literals);
  SatSolverInterfaceUnit.GetSatSolver.AddComment('Literal for constraint:'+ LiteralToString(Result));

  {
  Having an equation \sum a_ix_i= b, forcing \sum a_ix_i\le b, using my encoding, does not help.

  The other option is to disallow certain combinations of xi(the combinations whose summation is greater than b.
  I used a greedy approach to avoid these combinations...
  There is a better way to do so, which I may develop later, depending on the performance of this implementation.
}

  if UpperCase(GetRunTimeParameterManager.GetValueByName('--ForceLessThanForEquality'))= UpperCase('UsingClauses') then
  begin
    ForceLessThanForEqualityByClauses(ActiveConstraint);
//    ForceGreaterThanForEqualityByClauses(ActiveConstraint);

  end
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ForceLessThanForEquality'))= UpperCase('UsingCardinality') then
  begin
    Assert(False);
{    ForceLessThanForEqualityByCardinalityConstraints(ActiveConstraint);
    ForceGreaterThanForEqualityByCardinalityConstraints(ActiveConstraint);
 }
  end;

  Literals.Free;
  Modulos.Free;

  ActiveConstraint.Free;

end;

var
  PrimeModuloGenerator: TMyPBSolverEngineUsingPrimeModulos;

function TAbstractMyPBSolverEngine.EncodeGreaterThanOrEqualConstraint(AConstraint: TPBConstraint): TLiteral;
  procedure DescribeUsingPowerOfTwo(Diff: TBigInt; LHS: TPBSum; RHS: TBigInt);
  var
    Current: TBigInt;

  begin
    Current:= BigIntFactory.GetNewMemeber.SetValue(1);

    while Current.CompareWith(Diff)<= 0 do
    begin
      LHS.AddNewTerm(TTerm.Create(CreateLiteral(VariableGenerator.CreateNewVariable(vpNone, True), True), Current.Copy));
      RHS.Add(Current);

      Current.Mul2;

    end;

    BigIntFactory.ReleaseMemeber(Current);

  end;

  procedure DescribeUsingPrimes(Diff: TBigInt; LHS: TPBSum; RHS: TBigInt);
  var
    Primes: TIntegerCollection;
    Current, Temp: TBigInt;
    i, j, k: Integer;
    Literals: TLiteralCollection;

  begin
    Primes:= PrimeModuloGenerator.GenerateModulos(Diff);
    Current:= BigIntFactory.GetNewMemeber.SetValue(1);

    Literals:= TLiteralCollection.Create;

    for i:= 0 to Primes.Count- 1 do
    begin
      Literals.Clear;

      for j:= 1 to Primes.Item[i]- 1 do
      begin
        Literals.PushBack(CreateLiteral(VariableGenerator.CreateNewVariable(vpNone, True), True));
        LHS.AddNewTerm(TTerm.Create(Literals.Items[j- 1], Current.Copy));
        RHS.Add(Current);

      end;

      for j:= 0 to Literals.Count- 1 do
        for k:= j+ 1 to Literals.Count- 1 do
        begin
          CNFGenerator.BeginConstraint;

          //L[k]=> L[j] <==> ~L[k] or L[j]
          CNFGenerator.AddLiteral(NegateLiteral(Literals.Items[k]));
          CNFGenerator.AddLiteral(Literals.Items[j]);

          CNFGenerator.SubmitClause;

        end;

      Temp:= Current.Mul(BigIntFactory.GetNewMemeber.SetValue(Primes.Item[i]));
      BigIntFactory.ReleaseMemeber(Current);
      Current:= Temp;

    end;

    Literals.Free;
    BigIntFactory.ReleaseMemeber(Current);

  end;

var
  Dif: TBigInt;
  NewLHS: TPBSum;
  NewRHS: TBigInt;
  NewConstraint: TPBConstraint;
  ForcedLiterals: TLiteralCollection;
  i: Integer;
  TrueIntegers, UnknownIntegers: TBigInt;

begin
  if GetRunTimeParameterManager.Verbosity and Ord(vbFull)<> 0 then
    WriteLn('c AConstraint=', AConstraint.ToString);

  NewLHS:= TPBSum.Create;
  ForcedLiterals:= TLiteralCollection.Create;

  TrueIntegers:= BigIntFactory.GetNewMemeber.SetValue(0);
  UnknownIntegers:= BigIntFactory.GetNewMemeber.SetValue(0);

  for i:= 0 to AConstraint.LHS.Count- 1 do
    if AConstraint.RHS.CompareWith(AConstraint.LHS.Item[i].Coef)<= 0 then
    begin
      case CNFGenerator.GetLiteralValue(AConstraint.LHS.Item[i].Literal) of
        gbUnknown:
        begin
          ForcedLiterals.PushBack(AConstraint.LHS.Item[i].Literal);

        end;

        gbFalse:;
        gbTrue:
        begin
          ForcedLiterals.Free;
          NewLHS.Free;
          BigIntFactory.ReleaseMemeber(TrueIntegers);
          BigIntFactory.ReleaseMemeber(UnknownIntegers);
          Exit(VariableGenerator.TrueLiteral);

        end;

      end;

    end
    else
    begin
      case CNFGenerator.GetLiteralValue(AConstraint.LHS.Item[i].Literal) of
        gbUnknown:
        begin

          NewLHS.AddNewTerm(TTerm.Create(AConstraint.LHS.Item[i].Literal, AConstraint.LHS.Item[i].Coef.Copy));
          UnknownIntegers.Add(AConstraint.LHS.Item[i].Coef);

        end;
        gbTrue:
          TrueIntegers.Add(AConstraint.LHS.Item[i].Coef);

      end;

    end;

  if AConstraint.RHS.CompareWith(TrueIntegers)<= 0 then
  begin
    NewLHS.Free;
    ForcedLiterals.Free;

    BigIntFactory.ReleaseMemeber(TrueIntegers);
    BigIntFactory.ReleaseMemeber(UnknownIntegers);
    Exit(VariableGenerator.TrueLiteral);

  end;

// Can be improved

  NewRHS:= AConstraint.RHS.Copy.Sub(TrueIntegers);

  if UnknownIntegers.CompareWith(NewRHS)< 0 then
  begin
    if GetRunTimeParameterManager.Verbosity and Ord(vbFull)<> 0 then
      WriteLn('UnknownIntegers< NewRHS', ' ', UnknownIntegers.ToString, ' ', NewRHS.ToString);

    if 0< ForcedLiterals.Count then
      Result:= VariableGenerator.CreateVariableDescribingOR(ForcedLiterals)
    else
      Result:= VariableGenerator.FalseLiteral;

    ForcedLiterals.Free;
    BigIntFactory.ReleaseMemeber(TrueIntegers);
    BigIntFactory.ReleaseMemeber(UnknownIntegers);
    NewLHS.Free;
    BigIntFactory.ReleaseMemeber(NewRHS);
    Exit;

  end;

  Dif:= NewLHS.SumOfCoefs.Sub(NewRHS);

  if not Dif.IsZero then
  begin
//    DescribeUsingPowerOfTwo(Dif, NewLHS, NewRHS);
    DescribeUsingPrimes(Dif, NewLHS, NewRHS);

  end;

  NewConstraint:= TPBConstraint.Create(NewLHS, '=', True, NewRHS);
  NewConstraint.SimplificationOf:= AConstraint;

  if GetRunTimeParameterManager.Verbosity and Ord(vbFull)<> 0 then
    WriteLn('c NewConstraint='+ NewConstraint.ToString);

  Result:= EncodeHardConstraint(NewConstraint);

  if ForcedLiterals.Count<> 0 then
  begin
    ForcedLiterals.PushBack(Result);
    Result:= VariableGenerator.CreateVariableDescribingOR(ForcedLiterals);

  end;

  BigIntFactory.ReleaseMemeber(TrueIntegers);
  BigIntFactory.ReleaseMemeber(UnknownIntegers);
  ForcedLiterals.Free;
  NewConstraint.Free;

  BigIntFactory.ReleaseMemeber(Dif);

end;

function TAbstractMyPBSolverEngine.EncodeLessThanOrEqualConstraint(AConstraint: TPBConstraint): TLiteral;

  function IsSpecialCase(AConstraint: TPBConstraint): Boolean;
  var
    i, j: Integer;
    a: TBigInt;

  begin
    Result:= False;

    for i:= 0 to AConstraint.LHS.Count- 1 do
      for j:= i+ 1 to AConstraint.LHS.Count- 1 do
      begin
        a:= AConstraint.LHS.Coef[i].Copy.Add(AConstraint.LHS.Coef[j]);
        if a.CompareWith(AConstraint.RHS)<= 0 then// a<= RHS
        begin
          BigIntFactory.ReleaseMemeber(a);
          Exit;

        end;

      end;

     Result:= True;

  end;

  function EncodeSpecialCase(AConstraint: TPBConstraint): TLiteral;
  var
    i, j: Integer;
    Clause: TClause;

  begin
    for i:= 0 to AConstraint.LHS.Count- 1 do
      for j:= i+ 1 to AConstraint.LHS.Count- 1 do
      begin
        Clause:= VariableGenerator.SatSolver.BeginConstraint;
        Clause.PushBack(NegateLiteral(AConstraint.LHS.Literal[i]));
        Clause.PushBack(NegateLiteral(AConstraint.LHS.Literal[i]));
        VariableGenerator.SatSolver.SubmitClause;

      end;


    Result:= VariableGenerator.TrueLiteral;

  end;

  procedure DescribeUsingPowerOfTwo(Diff: TBigInt; LHS: TPBSum; RHS: TBigInt);
  var
    Current: TBigInt;

  begin
    Current:= BigIntFactory.GetNewMemeber.SetValue(1);

    while Current.CompareWith(Diff)<= 0 do
    begin
      LHS.AddNewTerm(TTerm.Create(CreateLiteral(VariableGenerator.CreateNewVariable(vpNone, True), True), Current.Copy));

      Current.Mul2;

    end;

    BigIntFactory.ReleaseMemeber(Current);

  end;

  procedure DescribeUsingPrimes(Diff: TBigInt; LHS: TPBSum; RHS: TBigInt);
  var
    Primes: TIntegerCollection;
    Current, Temp: TBigInt;
    i, j, k: Integer;
    Literals: TLiteralCollection;

  begin
    Primes:= PrimeModuloGenerator.GenerateModulos(Diff);
    Current:= BigIntFactory.GetNewMemeber.SetValue(1);

    Literals:= TLiteralCollection.Create;//(Primes.Item[Primes.Count- 1]);

    for i:= 0 to Primes.Count- 1 do
    begin
      Literals.Clear;

      for j:= 1 to Primes.Item[i]- 1 do
      begin
        Literals.PushBack(CreateLiteral(VariableGenerator.CreateNewVariable(vpNone, True), True));
        LHS.AddNewTerm(TTerm.Create(Literals.Items[j- 1], Current.Copy));

      end;

      for j:= 0 to Literals.Count- 1 do
        for k:= j+ 1 to Literals.Count- 1 do
        begin
          CNFGenerator.BeginConstraint;

          //L[k]=> L[j] <==> ~L[k] or L[j]
          CNFGenerator.AddLiteral(NegateLiteral(Literals.Items[k]));
          CNFGenerator.AddLiteral(Literals.Items[j]);

          CNFGenerator.SubmitClause;

        end;

      Temp:= Current.Mul(BigIntFactory.GetNewMemeber.SetValue(Primes.Item[i]));
      BigIntFactory.ReleaseMemeber(Current);
      Current:= Temp;

    end;

    Literals.Free;
    BigIntFactory.ReleaseMemeber(Current);

  end;

var
  Temp: TBigInt;
  NewLHS: TPBSum;
  NewRHS: TBigInt;
  NewConstraint: TPBConstraint;
  i: Integer;

begin
  if GetRunTimeParameterManager.Verbosity and Ord(vbFull)<> 0 then
    WriteLn('c AConstraint=', AConstraint.ToString);

  if not AConstraint.RHSSign then
    Exit(VariableGenerator.TrueLiteral);

  if IsSpecialCase(AConstraint) then
  begin
    if(GetRunTimeParameterManager.Verbosity and Ord(vbFull))<> 0 then
    begin
      WriteLn('c Special case for <=:');
      WriteLn('c ', AConstraint.ToString);

    end;

    Exit(EncodeSpecialCase(AConstraint));

  end;

  {
    Let S be the sum of ai's.
    If S- b> b, it is beneficial to convert the PBconstraint to an equivalent constraint
    with >= operator.
  }

  Temp:= AConstraint.LHS.SumOfCoefs.Sub(AConstraint.RHS);
  if Temp.CompareWith(AConstraint.RHS)< 0 then
  begin
    NewLHS:= AConstraint.LHS.Copy;
    NewRHS:= BigIntFactory.GetNewMemeber;
    NewRHS.SetValue(0);

    for i:= 0 to AConstraint.LHS.Count- 1 do
    begin
      NewLHS.Item[i].First:= NegateLiteral(AConstraint.LHS.Item[i].Literal);
      NewRHS.Add(AConstraint.LHS.Item[i].Coef);

    end;

    if AConstraint.RHSSign then
      NewRHS.Sub(AConstraint.RHS)
    else
    begin
      NewRHS.Add(AConstraint.RHS);
      NewConstraint:= TPBConstraint.Create(NewLHS, '>=', True, NewRHS);

      NewConstraint.Free;

    end;

    NewConstraint:= TPBConstraint.Create(NewLHS, '>=', True, NewRHS);
    if GetRunTimeParameterManager.Verbosity and Ord(vbFull)<> 0 then
      WriteLn('c NewConstraint=', NewConstraint.ToString);

    Result:= EncodeGreaterThanOrEqualConstraint(NewConstraint);

    NewConstraint.Free;

  end
  else
  begin
    {
      Convert AConstraint to an equivalent constraint whose comparision operator is =.
    }

    NewLHS:= AConstraint.LHS.Copy;
    NewRHS:= AConstraint.RHS.Copy;
    DescribeUsingPrimes(AConstraint.RHS, NewLHS, NewRHS);
    NewConstraint:= TPBConstraint.Create(NewLHS, '=', True, NewRHS);

    if GetRunTimeParameterManager.Verbosity and Ord(vbFull)<> 0 then
      WriteLn('c '+ NewConstraint.ToString);

    Result:= EncodeHardConstraint(NewConstraint);

    NewConstraint.Free;

  end;

  BigIntFactory.ReleaseMemeber(Temp);
end;


destructor TAbstractMyPBSolverEngine.Destroy;
begin

  inherited Destroy;

end;

initialization
  PrimeModuloGenerator:= TMyPBSolverEngineUsingPrimeModulos.Create;

finalization
  PrimeModuloGenerator.Free;

end.
