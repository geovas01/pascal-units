unit ModuloBasedFactoringUnit;

{$mode objfpc}{$H+}
{$Assertions on}
interface

uses
  Classes, SysUtils, FactoringUsingSATUnit, BigInt,
  BitVectorUnit, BinaryArithmeticCircuitUnit,
  ClauseUnit, GenericCollectionUnit;

type
  TIntegerCollection = specialize TGenericCollectionForBuiltInData<Integer>;


  { TModuloBasedBinaryArithmeticCircuit }

  TModuloBasedBinaryArithmeticCircuit = class(TBinaryArithmeticCircuit)
  private
    function GenerateAllPrimesLessThanEq(Last: Integer): TIntegerCollection;

  protected
    function GenerateModulosUsingDP(n: Integer): TIntegerCollection;
    function GenerateModulos(n: Integer; MaxModLogVal: Integer = -1): TIntegerCollection;
    function EncodeMul(const a, b, c: TBitVector): TLiteral; override;
    {
    Encode c = a mod (2^m-1).
    }
    function EncodeMod(const a: TBitVector; const m: Int64; const c : TBitVector): TLiteral;
    {
    Encode a > (2^m-1).
    }
    function EncodeGreaterThan(const a: TBitVector; const m: Int64): TLiteral;
    {
      Encode c = (a + b) mod  (2^m -1)
    }
    function EncodeAddModuloMod(const a, b, c: TBitVector; m: Integer): TLiteral;
    {
      Result = (a + b) mod  (2^m -1)
    }
    function AddModuloMod(const a, b: TBitVector; m: Integer): TBitVector;

  public
    constructor Create;
    destructor Destroy; override;



  end;

  { TBinaryModuloFactorizer }

  TBinaryModuloFactorizer= class(TBaseFactorizerUsingSAT)
  public
    constructor Create;
    destructor Destroy; override;

  end;

 const
    VerbBaseFactoringUsingModulo: Integer = 16;

implementation
uses
  TSeitinVariableUnit, ParameterManagerUnit, gvector, math,
  SatSolverInterfaceUnit, PBConstraintUnit;

{ TModuloBasedBinaryArithmeticCircuit }

function TModuloBasedBinaryArithmeticCircuit.GenerateModulos(n: Integer;
  MaxModLogVal: Integer): TIntegerCollection;

  function SelectSmallestSubset(Modulos: TIntegerCollection;
    TargetLog: Integer): TIntegerCollection;
  var
    ModulosBigInts: TBigIntCollection;
    CurrentSet: TBigIntCollection;
    MinSetMask: Integer;
    MinProd, Temp: TBigInt;
    i, j: Integer;
    mCount: Integer;

  begin
    ModulosBigInts := TBigIntCollection.Create;
    for i := 0 to Modulos.Count - 1 do
      ModulosBigInts.Add(
          BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(Modulos[i]).Decr);

    mCount := Modulos.Count;

    MinProd := BigIntFactory.ComputeProduct(ModulosBigInts);
    Assert(TargetLog <= MinProd.Log, 'TargetLog = ' + IntToStr(TargetLog) +
      ' all modulos prod = ' + IntToStr(MinProd.Log));
    MinSetMask := (1 shl mCount) - 1;

    CurrentSet := TBigIntCollection.Create;
    for i := 0 to ((1 shl mCount) - 1) do
    begin
      CurrentSet.Clear;

      for j := 0 to mCount - 1 do
        if (i and (1 shl j)) <> 0 then
          CurrentSet.Add(ModulosBigInts[j]);

      Temp := BigIntFactory.ComputeProduct(CurrentSet);
  //    WriteLn('i = ', i, ' Temp = ', Temp.Log, ' TargetLog', TargetLog, ' MinProd', MinProd.ToString);
      if Temp.Log < TargetLog then
      else if MinProd.CompareWith(Temp) > 0 then
      begin
        BigIntFactory.ReleaseMemeber(MinProd);
        MinProd := Temp.Copy;
        MinSetMask := i;
      end;

      BigIntFactory.ReleaseMemeber(Temp);
    end;

    for i := 0 to ModulosBigInts.Count - 1 do
      BigIntFactory.ReleaseMemeber(ModulosBigInts[i]);
    ModulosBigInts.Clear;
    ModulosBigInts.Free;

    Result := TIntegerCollection.Create;
    for j := 0 to mCount - 1 do
      if (MinSetMask and (1 shl j)) <> 0 then
        Result.Add(Modulos[j]);
  end;

var
  AllPrimes, Modulos: TIntegerCollection;
  b: Integer;
  p2b: Int64;
  logn: Integer;
  i, j: Integer;

  Top, Bot, Mid: Integer;
  StartingIndex: Integer;

  Prod, t2p, Temp: TBigInt;
begin
  if MaxModLogVal = -1 then
    MaxModLogVal := MaxInt;

  AllPrimes := GenerateAllPrimesLessThanEq(2 * n);

  logn := Round(log2(n)) + 1;
  if (GetRunTimeParameterManager.Verbosity and VerbBaseFactoringUsingModulo) <> 0 then
    WriteLn('logn = ', logn);

  b := Round(log2((n + 2) / logn));
  p2b := 1 shl b;

  Top := AllPrimes.Count - 1; Bot := 0;

  StartingIndex := -1;
  while Bot <= Top do
  begin
    Mid := (Top + Bot) div 2;
    if AllPrimes[Mid] < p2b then
      Bot := Mid + 1
    else // if p2b <= AllPrimes[Mid]
    begin
      StartingIndex := Mid;
      Top := Mid - 1;
    end;
  end;

  Prod := BigIntFactory.GetNewMember.SetValue(1);

  Modulos := TIntegerCollection.Create;
  for i := StartingIndex to Min(StartingIndex +  logn, AllPrimes.Count - 1)  do
  begin
    if MaxModLogVal <= AllPrimes[i]  then
      break;
    t2p := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(AllPrimes[i]).Decr;
    assert(not t2p.IsZero);

    Temp := Prod.Mul(t2p);
    BigIntFactory.ReleaseMemeber(t2p);
    BigIntFactory.ReleaseMemeber(Prod);
    Prod := Temp;

    Modulos.Add(AllPrimes[i]);
    if n < Prod.Log then
      Break;

  end;

  if Prod.Log < n then
  begin
    for i := StartingIndex - 1 downto Max(0, Modulos.Count - Logn)  do
    begin
      t2p := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(AllPrimes[i]).Decr;
      Temp := Prod.Mul(t2p);
      BigIntFactory.ReleaseMemeber(t2p);
      BigIntFactory.ReleaseMemeber(Prod);
      Prod := Temp;

      Modulos.Add(AllPrimes[i]);
      if n < Prod.Log then
        Break;
    end;
  end;

  BigIntFactory.ReleaseMemeber(Prod);
  AllPrimes.Free;

  Result := SelectSmallestSubset(Modulos, n);
  Modulos.Free;

end;

function TModuloBasedBinaryArithmeticCircuit.GenerateAllPrimesLessThanEq(
  Last: Integer): TIntegerCollection;
var
  IsPrime: array of Boolean;
  i, j: Integer;

begin
  SetLength(IsPrime, Last + 1);

  for i := 0 to Last do
    IsPrime[i] := True;

  IsPrime[0] := False;
  IsPrime[1] := False;
  for i := 2 to (Last + 1) div 2 do
    if IsPrime[i] then
      for j := 2 to Last  div i do
        IsPrime[j * i] := False;

  Result := TIntegerCollection.Create;
  for i := 2 to Last do
    if IsPrime[i] then
      Result.Add(i);

end;

function TModuloBasedBinaryArithmeticCircuit.GenerateModulosUsingDP(n: Integer
  ): TIntegerCollection;
type
  TIntegerCollection = specialize TGenericCollectionForBuiltInData<Integer>;

type
  TPair = record
    Sum: TIntegerCollection;
    Count: Integer;
  end;

var
  AllPrimes: TIntegerCollection;
  dp: array of TIntegerCollection;
  logn: Integer;
  i, j: Integer;
  TargetSum: Integer;
  ActivePrime: Integer;

begin
  AllPrimes := GenerateAllPrimesLessThanEq(2 * n);

  logn := Round(log2(n)) + 1;
  SetLength(dp, logn + 1);
  for i := 0 to logn  do
    dp[i] := TIntegerCollection.Create(3 * n, 0);


  dp[0][0] := 1;
  dp[0][AllPrimes[0]] := 1;
  for i := 1 to AllPrimes.Count - 1 do
  begin
    ActivePrime := AllPrimes[i];

    for j := 0 to dp[i - 1].Count - 1 do
      if dp[i - 1][j] <> 0 then
      begin
        dp[i][j] := 1;
        if j + ActivePrime < dp[i].Count then
          dp[i][j + ActivePrime] := 1;
      end;
  end;

  TargetSum := -1;
  for i := 2 * n + 3 to dp[AllPrimes.Count - 1].Count do
    if dp[AllPrimes.Count - 1][i] <> 0 then
    begin
      TargetSum := i;
      break;
    end;
  assert(TargetSum <> -1, 'TargetSum = -1');

  Result := TIntegerCollection.Create;
  i := AllPrimes.Count - 1;
  while 1 <= i do
  begin
    Assert(dp[i][TargetSum] <> -1);
    ActivePrime := AllPrimes[i];

    if dp[i - 1][TargetSum] <> 0 then
    else if dp[i - 1][TargetSum - ActivePrime] <> 0  then
    begin
      Result.Add(ActivePrime);
      TargetSum -= ActivePrime;
    end
    else
      Assert(False,
        'both dp[i-1][TargetSum] and dp[i-1][TargetSum-ActivePrime] are False!');

    Dec(i);
  end;

  assert((TargetSum = 2) or (TargetSum = 0));
  if TargetSum = 2 then
    Result.Add(2);

  for i := 0 to logn  do
    dp[i].Free;
  SetLength(dp, 0);
  AllPrimes.Free;
end;

function TModuloBasedBinaryArithmeticCircuit.EncodeMul(const a, b, c: TBitVector
  ): TLiteral;
{
  This method selects Log(n) prime integers, p1,...,p_m, such that their summation is
  greater than 2n + 2, and returns 2^p_i - 1, for i = 1, ..., m = Log(n).
}


var
  i: Integer;
  Temp, t2p: TBigInt;
  m: Int64;
  Modulos: TIntegerCollection;
  aModm, bModm, cModm, TempVector: TBitVector;
  aModbMod, aModbModModm: TBitVector;
  lit: TLiteral;
  Prod: TBigInt;
  LastVar: Integer;

begin
  assert(a.Count + b.Count <= c.Count);

  if a.Count + b.Count <= 26 then
  begin
    TempVector := inherited Mul(a, b);
    Result := EncodeIsEqual(TempVector, c);
    Exit;
  end;

//  WriteLn('Mul a.Count = ', a.Count, ' b.Count = ', b.Count);
  Modulos := nil;
  if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModuloMode')) = UpperCase('dp') then
    Modulos := GenerateModulosUsingDP(a.Count + b.Count)
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModuloMode')) = UpperCase('normal') then
    Modulos := GenerateModulos(a.Count + b.Count, Min(a.Count, b.Count));
  assert(Modulos <> nil);

  prod := BigIntFactory.GetNewMember.SetValue(1);
  for i := 0 to Modulos.Count - 1 do
  begin
    t2p := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(Modulos[i]).Decr;

    Temp := Prod.Mul(t2p);
    BigIntFactory.ReleaseMemeber(Prod);
    Prod := Temp;
    if (GetRunTimeParameterManager.Verbosity and VerbBaseFactoringUsingModulo) <> 0 then
       WriteLn('Modulo[ ', i, '] = ', t2p.ToString);
  end;

  Assert(a.Count + b.Count <= Prod.Log);
  BigIntFactory.ReleaseMemeber(Prod);

  //Result := TBitVector.Create(a.Count + b.Count);

  Write(a.Count + b.Count, ':');
  for i:= 0 to Modulos.Size- 1 do
    Write(' ', Modulos[i]);
  WriteLn;

  SatSolver.BeginConstraint;

  for i:= 0 to Modulos.Size- 1 do
  begin
    LastVar := GetVariableManager.LastUsedCNFIndex;
    m := Modulos[i];
    WriteLn('Modulo = ', m);
    assert(m < Max(a.Count, b.Count), 'm  = '+ IntToStr(m) + ' a.Count = ' + IntToStr(a.Count) +  ' b.Count = ' + IntToStr(b.Count));

    if GetRunTimeParameterManager.Verbosity and
       (1 shl VerbBaseFactoringUsingModulo) <> 0 then
    begin
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: M [', i, '] = 2^', m, ' - 1 mBin= ', IntToStr(m));
    end;

    aModm := TBitVector.Create(Min(m, a.Count));
    bModm := TBitVector.Create(Min(m, b.Count));
    cModm := TBitVector.Create(Min(m, c.Count));
    WriteLn('Modulos = ', m, ' vars = ', GetVariableManager.LastUsedCNFIndex - LastVar);


//    WriteLn('*', cModm[cModm.Count - 1]);
    SatSolver.BeginConstraint;
//    SatSolver.AddComment('a , amodM (M=2^' + IntToStr(m) + '-1)' + a.ToString + ' ' + aModm.ToString);
//    WriteLn('aMod ', m, aModm.ToString);

    LastVar:= GetVariableManager.LastUsedCNFIndex;
    SatSolver.AddLiteral(EncodeMod(a, m, aModm));
    WriteLn('aMod[', m, '] len=', a.Count,' vars = ', GetVariableManager.LastUsedCNFIndex - LastVar);

    SatSolver.AddComment('b and bmodM = ' + b.ToString + ' ' + bModm.ToString);
//    WriteLn('b and bmodM = ' + b.ToString + ' ' + bModm.ToString);
    LastVar:= GetVariableManager.LastUsedCNFIndex;
    SatSolver.AddLiteral(EncodeMod(b, m, bModm));
    WriteLn('bMod[', m, '] len=', b.Count,' vars = ', GetVariableManager.LastUsedCNFIndex - LastVar);

    SatSolver.AddComment('bmod done');
    SatSolver.AddComment('c and cmodM = ' + c.ToString + ' ' + cModm.ToString);
//    WriteLn('c and cmodM = ' + c.ToString + ' ' + cModm.ToString);

    LastVar:= GetVariableManager.LastUsedCNFIndex;
    SatSolver.AddLiteral(EncodeMod(c, m, cModm));
    WriteLn('cMod[', m, '] len=', c.Count,' vars = ', GetVariableManager.LastUsedCNFIndex - LastVar);

    if GetRunTimeParameterManager.Verbosity and
       (1 shl VerbBaseFactoringUsingModulo) <> 0 then
    begin
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: aModm =', aModm.ToString);
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: bModm =', bModm.ToString);
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: cModm =', cModm.ToString);
    end;

    LastVar:= GetVariableManager.LastUsedCNFIndex;
    aModbMod:= TBitVector.Create(aModm.Count + bModm.Count);
    SatSolver.AddLiteral(EncodeMul(aModm, bModm, aModbMod));
//    WriteLn('aModbMod = ' + aModbMod.ToString);
    WriteLn('aModbMod[', m, '] vars = ', GetVariableManager.LastUsedCNFIndex - LastVar);

    aModbModModm := TBitVector.Create(m);
    LastVar:= GetVariableManager.LastUsedCNFIndex;
    SatSolver.AddLiteral(EncodeMod(aModbMod, m, aModbModModm));
//    WriteLn('aModbModModm = ' + aModbModModm.ToString);
    WriteLn('aModbModModm[', m, '] len=', aModbMod.Count,' vars = ', GetVariableManager.LastUsedCNFIndex - LastVar);

    SatSolver.AddLiteral(Self.EncodeIsEqual(aModbModModm, cModm));
    if GetRunTimeParameterManager.Verbosity and
       (1 shl VerbBaseFactoringUsingModulo) <> 0 then
    begin
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: c =', c.ToString);
      WriteLn('[BaseFactoringUsingModulo.GenerateCNF]: cPrimeMode =',
                                    cModm.ToString);
    end;

    LastVar:= GetVariableManager.LastUsedCNFIndex;
    SatSolver.AddLiteral(Self.EncodeIsEqual(cModm, aModbModModm));
    WriteLn('isEqual[', m, '] vars = ', GetVariableManager.LastUsedCNFIndex - LastVar);

    lit := CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SatSolver.SubmitAndGate(lit);

    SatSolver.AddLiteral(lit);

    aModm.Free;
    bModm.Free;
    cModm.Free;
    aModbMod.Free;
    aModbModModm.Free;

    WriteLn('Modulos = ', m, ' vars = ', GetVariableManager.LastUsedCNFIndex - LastVar);
  end;

  Modulos.Free;

  Result := GetVariableManager.TrueLiteral;
  SatSolver.SubmitAndGate(Result);

end;

function TModuloBasedBinaryArithmeticCircuit.EncodeMod(const a: TBitVector;
  const m: Int64; const c: TBitVector): TLiteral;
type
    TBitVectorList= specialize TGenericCollection<TBitVector>;

  function FindConjunction(const vecList: TBitVector): TLiteral;
  var
    i: Integer;
  begin
    SatSolver.BeginConstraint;

    for i := 0 to vecList.Count - 1 do
      SatSolver.AddLiteral(vecList[i]);
    Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SatSolver.SubmitAndGate(Result);
  end;

  function ParallelAdder(const Mat: TBitVectorList; Start, Finish: Integer): TBitVector;
  var
    FirstHalf, SecondHalf: TBitVector;
    i: Integer;

  begin
    if Start = Finish then
    begin
      Result:= Mat[Start].Copy;

      if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
       (1 shl VerbBinArithmCircuit)<> 0 then
        WriteLn('[Mul.ParallelAdder] Mat[', Start, '] = ', Result.ToString);

      Exit;
    end;

    FirstHalf:= ParallelAdder(Mat, Start, (Start + Finish) div 2);
    SecondHalf:= ParallelAdder(Mat, (Start + Finish) div 2 + 1, Finish);
    if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
     (1 shl VerbBinArithmCircuit)<> 0 then
    begin
      WriteLn('[Mul.ParallelAdder] FirstHalf = ', FirstHalf.ToString);
      WriteLn('[Mul.ParallelAdder] SecondHalf = ', SecondHalf.ToString);
    end;

    Result := TBitVector.Create(m);
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(EncodeAddModuloMod(FirstHalf, SecondHalf, Result, m));
    SatSolver.SubmitClause;

    if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
     (1 shl VerbBinArithmCircuit)<> 0 then
    begin
      WriteLn('[Mul.ParallelAdder] Mat[', Start, '] + ...+  Mat[', Finish, '] = ', Result.ToString);
    end;

    FirstHalf.Free;
    SecondHalf.Free;

  end;

var
  Mat: TBitVectorList;
  Current: TBitVector;
  i: Integer;
  cPrime: TBitVector;

begin
  Assert(c.Count <= m, 'c.Count = ' + IntToStr(c.Count) +
     ' while m = ' + IntToStr(m));

  for i := Min(m, c.Count) to c.Count - 1 do
  begin
    // Should not reach here
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(NegateLiteral(c[i]));
    SatSolver.SubmitClause;
  end;

  if a.Count <= m then
  begin// special case
    for i := 0 to a.Count - 1 do
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(a[i]);
      SatSolver.AddLiteral(c[i]);
      SatSolver.SubmitEquivGate(GetVariableManager.TrueLiteral);
    end;
    for i := a.Count to c.Count - 1 do
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(NegateLiteral(c[i]));
      SatSolver.SubmitClause;
    end;

    Result := GetVariableManager.TrueLiteral;
    Exit;
  end;

  Mat := TBitVectorList.Create;

  for i := 0 to a.Count - 1 do
  begin
    if i mod m = 0 then
    begin
      Current := TBitVector.Create;//(m, GetVariableManager.FalseLiteral);
      Mat.PushBack(Current);
    end;
    Current.Add(a[i]);
  end;

  for i := Current.Count + 1 to m do
    Current.PushBack(GetVariableManager.FalseLiteral);

  cPrime := ParallelAdder(Mat, 0, Mat.Count - 1);
  Result := Self.EncodeIsEqual(cPrime, c);

  cPrime.Free;
  Mat.Free;

end;

function TModuloBasedBinaryArithmeticCircuit.EncodeGreaterThan(
  const a: TBitVector; const m: Int64): TLiteral;
var
  i: Integer;
begin
  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);
  SatSolver.AddComment('GreaterThan ' + a.ToString + ' ' + IntToStr(m) + ' <=> ' + LiteralToString(Result));

  SatSolver.BeginConstraint;

  for i := m to a.Count - 1 do
    SatSolver.AddLiteral(a[i]);
  SatSolver.SubmitOrGate(Result);

  SatSolver.AddComment('GreaterThan ' + a.ToString + ' ' + IntToStr(m) + ' <=> ' + LiteralToString(Result));

end;

function TModuloBasedBinaryArithmeticCircuit.EncodeAddModuloMod(const a, b,
  c: TBitVector; m: Integer): TLiteral;
var
  Sum, TwoToMMinusOne, CPlusTwoTpMMinusOne: TBitVector;
  i: Integer;
  l, l1, l2: TLiteral;

begin
  Assert(a.Count = m);
  Assert(b.Count = m);
  Assert(m <= c.Count);

  for i := m to c.Count - 1 do
  begin
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(NegateLiteral(c[i]));
    SatSolver.SubmitClause;
  end;

  SatSolver.BeginConstraint;
  Sum := Add(a, b);
  l1 := EncodeIsEqual(c, Sum);

  if (Sum.Count = m) or (SatSolver.GetLiteralValue(Sum[m]) = gbFalse) then // 0..m-1  2^m-1
    SatSolver.AddLiteral(l1)
  else
  begin
    TwoToMMinusOne := TBitVector.Create(m, GetVariableManager.TrueLiteral);
    CPlusTwoTpMMinusOne := Add(c, TwoToMMinusOne);
  //    WriteLn('EncodeAddModuloMod: CPlusTwoTpMMinusOne = ' +
  //             CPlusTwoTpMMinusOne.ToString);
    l2 := EncodeIsEqual(CPlusTwoTpMMinusOne, Sum);
   //   WriteLn('EncodeAddModuloMod: l1 = ' + LiteralToString(l1));
   //   WriteLn('EncodeAddModuloMod: l2 = ' + LiteralToString(l2));
{    end;
 }
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(l1);
    SatSolver.AddLiteral(l2);
    l := CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SatSolver.SubmitOrGate(l);

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(l1);
    SatSolver.AddLiteral(l2);
    SatSolver.SubmitXOrGate(GetVariableManager.TrueLiteral);// ! (l1 -> ~l2) and (l2 -> ~l1)

    SatSolver.AddLiteral(l);

  end;
  Result := CreateLiteral(GetVariableManager.CreateNewVariable, True);
  SatSolver.SubmitAndGate(Result);
//  WriteLn('EncodeAddModuloMod: Result = ', LiteralToString(Result));
  SatSolver.AddComment(a.ToString + ' + ' + b.ToString + ' = ' + c.ToString + ' modulo 2^'+ IntToStr(m)+ '- 1');

end;

function TModuloBasedBinaryArithmeticCircuit.AddModuloMod(const a,
  b: TBitVector; m: Integer): TBitVector;
var
  Sum, TwoToMMinusOne, CPlusTwoTpMMinusOne: TBitVector;
  i: Integer;
  l, l1, l2: TLiteral;

begin
  Assert(a.Count = m);
  Assert(b.Count = m);
  Assert(m <= c.Count);

  SatSolver.BeginConstraint;
  Sum := Add(a, b);
  l1 := EncodeIsEqual(c, Sum);

  if (Sum.Count = m) or (SatSolver.GetLiteralValue(Sum[m]) = gbFalse) then // 0..m-1  2^m-1
    Result := Sum
  else
  begin
    TwoToMMinusOne := TBitVector.Create(m, GetVariableManager.TrueLiteral);
    CPlusTwoTpMMinusOne := Add(c, TwoToMMinusOne);
  //    WriteLn('EncodeAddModuloMod: CPlusTwoTpMMinusOne = ' +
  //             CPlusTwoTpMMinusOne.ToString);
    l2 := EncodeIsEqual(CPlusTwoTpMMinusOne, Sum);
   //   WriteLn('EncodeAddModuloMod: l1 = ' + LiteralToString(l1));
   //   WriteLn('EncodeAddModuloMod: l2 = ' + LiteralToString(l2));
{    end;
 }
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(l1);
    SatSolver.AddLiteral(l2);
    l := CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SatSolver.SubmitOrGate(l);

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(l1);
    SatSolver.AddLiteral(l2);
    SatSolver.SubmitXOrGate(GetVariableManager.TrueLiteral);// ! (l1 -> ~l2) and (l2 -> ~l1)

    SatSolver.AddLiteral(l);

  end;
  Result := CreateLiteral(GetVariableManager.CreateNewVariable, True);
  SatSolver.SubmitAndGate(Result);
//  WriteLn('EncodeAddModuloMod: Result = ', LiteralToString(Result));
  SatSolver.AddComment(a.ToString + ' + ' + b.ToString + ' = ' + c.ToString + ' modulo 2^'+ IntToStr(m)+ '- 1');

end;

constructor TModuloBasedBinaryArithmeticCircuit.Create;
begin
  inherited Create;

end;

destructor TModuloBasedBinaryArithmeticCircuit.Destroy;
begin

  inherited Destroy;
end;

constructor TBinaryModuloFactorizer.Create;
begin
  inherited;

  ArithmeticCircuit.Free;
  ArithmeticCircuit := TModuloBasedBinaryArithmeticCircuit.Create;
end;

destructor TBinaryModuloFactorizer.Destroy;
begin
  inherited Destroy;
end;

end.

