unit BinaryArithmeticCircuitUnit;
{$Assertions on}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseArithmeticCircuitUnit, BitVectorUnit, ClauseUnit,
  BigInt;

type

  {
  It considers the BitVectors represent the binary representation of numbers.
  This class assumes all numbers to be non-negative.

    BitVector V, with size n, encodes integer:
      \sum V[i]* 2^i
    In other word, the variable in V is True iff the integer represented by V is odd.
    The second variable in V is True iff the integer represented by V has one in its second bit,
    ....
  }
  { TBinaryArithmeticCircuit }

  TBinaryArithmeticCircuit = class(TBaseArithmeticCircuit)
  private
  protected
    { Result = a + 1}
    function Incr(const a: TBitVector): TBitVector; override;
    { Result = a + b}
    function Add(const a, b: TBitVector): TBitVector; override;
    { Result = a * b}
    function Mul(const a, b: TBitVector): TBitVector; override;

    function GenerateCarryForAdd(a, b, c: TLiteral): TLiteral;
  public
    { Result is True iff a< b}
    function EncodeIsLessThan(const a, b: TBitVector): TLiteral; override;
    { Result is True iff a= b}
    function EncodeIsEqual(const a, b: TBitVector): TLiteral; override;

    function EncodeBinaryRep(const n: TBigInt; a: TBitVector; nbits: Integer = -1): TLiteral; override;

  end;

const
  VerbBinArithmCircuit: Integer = 2;

implementation

uses
  Math, TSeitinVariableUnit, SatSolverInterfaceUnit,
  ParameterManagerUnit;

function GetValueCount(SatSolver: TSATSolverInterface; v: TGroundBool;
  Args: array of const): Integer;
var
  i: Integer;
  lit : TLiteral;

begin
  Result := 0;
  for i := 0 to High(Args) do
  begin
    lit := TLiteral(Args[i].VInteger);
    if SatSolver.GetValue(lit) = v then
      Inc(Result);
  end;
end;

{ TBinaryArithmeticCircuit }

function TBinaryArithmeticCircuit.Incr(const a: TBitVector): TBitVector;
var
  Carry, One: TBitVector;
  i: Integer;
  ai: TLiteral;

begin
  assert(False);
  assert(1 <= a.Count, 'a.Count must be greater than 1.');
  Exit(nil);

  Carry := TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);
  Result := TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);

  Carry[0] :=  a[0];

  Result[0] := NegateLiteral(a[0]);

  if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
   (1 shl VerbBinArithmCircuit)<> 0 then
  begin
    WriteLn('[Add]: Carry = ', Carry.ToString);
    WriteLn('[Add]: Result = ', Result.ToString);

  end;

  for i := 1 to a.Count - 1 do
  begin
    ai := a[i];

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(ai);
    SatSolver.AddLiteral(Carry[i- 1]);
    SatSolver.SubmitXOrGate(Result[i]);

    { Carray }
    // a[i] and Carry[i- 1] -> Carry[i]
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(NegateLiteral(ai));
    SatSolver.AddLiteral(NegateLiteral(Carry[i- 1]));
    SatSolver.AddLiteral(Carry[i]);
    SatSolver.SubmitClause;

  end;

  if Carry[a.Count - 1] <> TSeitinVariableUnit.GetVariableManager.FalseLiteral
     then
    Result.Add(Carry[a.Count - 1]);

end;

function TBinaryArithmeticCircuit.Add(const a, b: TBitVector): TBitVector;
var
  Carry: TBitVector;
  MaxLen: Integer;
  i: Integer;
  ai, bi: TLiteral;
begin
  assert(1 <= a.Count, 'a.Count (' + IntToStr(a.Count) + ') must be greater than 1.');
  assert(1 <= b.Count, 'b.Count (' + IntToStr(b.Count) + ') must be greater than 1.');

  MaxLen := Max(a.Count, b.Count);

  Carry := TBitVector.Create(MaxLen, GetVariableManager.FalseLiteral);
  Result := TBitVector.Create(MaxLen, GetVariableManager.FalseLiteral);


  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(a[0]);
  SatSolver.AddLiteral(b[0]);
  Carry[0] := SatSolver.GenerateAndGate;

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(a[0]);
  SatSolver.AddLiteral(b[0]);
  Result[0] := SatSolver.GenerateXOrGate;

  for i := 1 to MaxLen - 1 do
  begin
    if i< a.Count then
      ai := a[i]
    else
      ai := GetVariableManager.FalseLiteral;

    if i< b.Count then
      bi := b[i]
    else
      bi := GetVariableManager.FalseLiteral;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(ai);
    SatSolver.AddLiteral(bi);
    SatSolver.AddLiteral(Carry[i- 1]);
    Result[i] := SatSolver.GenerateXOrGate;

    Carry[i] := GenerateCarryForAdd(ai, bi, Carry[i - 1]);
  end;

  if Carry[MaxLen- 1] <> TSeitinVariableUnit.GetVariableManager.FalseLiteral
     then
    Result.Add(Carry[MaxLen- 1]);

  if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
   (1 shl VerbBinArithmCircuit)<> 0 then
  begin
    WriteLn('[Add]: Carry = ', Carry.ToString);
    WriteLn('[Add]: Result = ', Result.ToString);
  end;
  SatSolver.AddComment(a.ToString + ' + ' + b.ToString + ' = ' + Result.ToString);

end;

function TBinaryArithmeticCircuit.Mul(const a, b: TBitVector): TBitVector;
  function ParallelAdder(Mat: TBitVectorList; Start, Finish: Integer): TBitVector;
  var
    Mid: Integer;
    FirstHalf, SecondHalf: TBitVector;
  begin
    if Start = Finish then
    begin
      Result := Mat[Start].Copy;

      if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
       (1 shl VerbBinArithmCircuit)<> 0 then
        WriteLn('[Mul.ParallelAdder] Mat[', Start, '] = ', Result.ToString);
      Exit;
    end;


    Mid := (Start + Finish) div 2;
    FirstHalf := ParallelAdder(Mat, Start, Mid);
    if Mid < Finish then
      SecondHalf := ParallelAdder(Mat, Mid + 1, Finish)
    else
      SecondHalf := TBitVector.Create(Mat[0].Count, GetVariableManager.FalseLiteral);

    if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
     (1 shl VerbBinArithmCircuit)<> 0 then
    begin
      WriteLn('[Mul.ParallelAdder] FirstHalf= ', FirstHalf.ToString);
      WriteLn('[Mul.ParallelAdder] SecondHalf= ', SecondHalf.ToString);
    end;

    Result := Self.Add(FirstHalf, SecondHalf);

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
  i, j: Integer;

begin
  Assert((1<= a.Count) and(1<= b.Count));

  Mat := TBitVectorList.Create;

  for i := 0 to a.Count - 1 do
    Mat.PushBack(TBitVector.Create(a.Count + b.Count, GetVariableManager.FalseLiteral));

  for i := 0 to a.Count- 1 do
  begin
   // Mat[i][k], k< i, False

   // Mat[i][i + j] <=> a[i] and b[j]
    for j := 0 to b.Count- 1 do
    begin
      //Mat[i][i+ j] := CreateLiteral(GetVariableManager.CreateNewVariable, False);
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(a[i]);
      SatSolver.AddLiteral(b[j]);
      Mat[i][i+j] := SatSolver.GenerateAndGate;
    end;
  end;

  if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
   (1 shl VerbBinArithmCircuit)<> 0 then
  begin
    for i := 0 to Mat.Count- 1 do
      WriteLn('[MUL]: Mat[', i, ']= ', Mat[i].ToString);
    WriteLn('[MUL]');
  end;

  Result := ParallelAdder(Mat, 0, Mat.Count- 1);

  Mat.Free;
  SatSolver.AddComment('TBinaryArithmeticCircuit: ' + a.ToString + ' * ' + b.ToString + ' = ' + Result.ToString);
end;

function TBinaryArithmeticCircuit.GenerateCarryForAdd(a, b, c: TLiteral): TLiteral;
var
  TrueCount, FalseCount: Integer;
begin
  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(a);
  SatSolver.AddLiteral(b);
  SatSolver.AddLiteral(c);

  TrueCount := SatSolver.NoOfLiteralInTopConstraint[gbTrue];
  FalseCount := SatSolver.NoOfLiteralInTopConstraint[gbFalse];

  SatSolver.AbortConstraint;

  if 2 <= TrueCount then
    Exit(GetVariableManager.TrueLiteral);
  if 2 <= FalseCount then
    Exit(GetVariableManager.FalseLiteral);


  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);

  { Carray }

  // a and b -> r    (~a, ~b, r)
  // a and c -> r    (~a, ~c, r)
  // b and c -> r    (~b, ~c, r)
  // r and ~a -> b   (~r, a, b)
  // r and ~a -> c   (~r, a, c)
  // r and ~b -> c   (~r. b, c)

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(a), NegateLiteral(b), Result]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(a), NegateLiteral(c), Result]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(b), NegateLiteral(c), Result]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(Result), a, b]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(Result), b, c]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(Result), a, c]);
  SatSolver.SubmitClause;

end;

function TBinaryArithmeticCircuit.EncodeIsLessThan(const a, b: TBitVector): TLiteral;
var
  i: Integer;
  li: TLiteral;
  ailebi, aixorbi: TLiteral;
  aIsLessThanb,
  aiIsEqbi, aiIsLbi,
  aIsEqbTillNow: TBitVector;

begin//a< b
  WriteLn('[TBinaryArithmeticCircuit.EncodeIsLessThan] This function can be improved!');
  Assert(a.Count = b.Count);

  aIsLessThanB := TBitVector.Create(a.Count);

  aIsEqbTillNow := TBitVector.Create(a.Count);

  aiIsEqbi := TBitVector.Create(a.Count);
  aiIsLbi := TBitVector.Create(a.Count);

  if(GetRunTimeParameterManager.Verbosity and(1 shl VerbBinArithmCircuit))<> 0 then
  begin
    WriteLn('[IsLessThan] a: ', a.ToString);
    WriteLn('[IsLessThan] b: ', b.ToString);
    WriteLn('[IsLessThan] aiIsEqbi : ', aiIsEqbi.ToString);
    WriteLn('[IsLessThan] aiIsLbi : ', aiIsLbi.ToString);
    WriteLn('[IsLessThan] aiIsEqbTillNow : ', aIsEqbTillNow.ToString);

  end;

  aIsEqbTillNow.Add(GetVariableManager.TrueLiteral);

  for i := a.Count- 1 downto 0 do
  begin
    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral(a[i]);
    GetSatSolver.AddLiteral(b[i]);

    GetSatSolver.SubmitXOrGate(NegateLiteral(aiIsEqbi[i]));//(not aixorbi) <=> ai= bi;

    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral(aiIsEqbi[i]);
    GetSatSolver.AddLiteral(aIsEqbTillNow[i+ 1]);
    GetSatSolver.SubmitAndGate(aIsEqbTillNow[i]);

  end;

  for i := a.Count- 1 downto 0 do
  begin
    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral(NegateLiteral(a[i]));
    GetSatSolver.AddLiteral(b[i]);
    GetSatSolver.SubmitAndGate(aiIsLbi[i]);

    GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral(aiIsLbi[i]);
    GetSatSolver.AddLiteral(aIsEqbTillNow[i+ 1]);
    GetSatSolver.SubmitAndGate(aIsLessThanb[i]);

  end;

  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);
  GetSatSolver.BeginConstraint;

  for i := a.Count- 1 downto 0 do
    GetSatSolver.AddLiteral(aIsLessThanb[i]);
  GetSatSolver.SubmitOrGate(Result);

end;

function TBinaryArithmeticCircuit.EncodeIsEqual(const a, b: TBitVector): TLiteral;
var
  i: Integer;
  SameithBit: TLiteral;

begin
  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);

  GetSatSolver.BeginConstraint;

//  WriteLn('[EncodeIsEqual] a = ', a.ToString);
//  WriteLn('[EncodeIsEqual] b = ', b.ToString);

  for i := Min(a.Count- 1, b.Count- 1)+ 1 to
                    Max(a.Count- 1, b.Count- 1) do
  begin
    if i < a.Count then
      GetSatSolver.AddLiteral(NegateLiteral(a[i]))
    else
      GetSatSolver.AddLiteral(NegateLiteral(b[i]));
  end;

  for i := 0 to Min(a.Count- 1, b.Count- 1) do
  begin
    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral(a[i]);
    GetSatSolver.AddLiteral(b[i]);

    SameithBit := CreateLiteral(GetVariableManager.CreateNewVariable, False);
    GetSatSolver.SubmitEquivGate(SameithBit);//aiAndbi <=> ai <-> bi;

//    WriteLn(GetSatSolver.TopConstraint.ToString);
    GetSatSolver.AddLiteral(SameithBit);

  end;

  GetSatSolver.SubmitAndGate(Result);//Result<=> \bigwedge_i SameithBit;

  if ParameterManagerUnit.GetRunTimeParameterManager.
    ValueByName['--AddExtraClausesForEq'] = UpperCase('True') then
  begin
    for i := 0 to Min(a.Count- 1, b.Count- 1) do
    begin
// ~ai, bi, ~r : ai &~bi => ~r
      GetSatSolver.BeginConstraint;
      GetSatSolver.AddLiteral(NegateLiteral(a[i]));
      GetSatSolver.AddLiteral(b[i]);
      GetSatSolver.AddLiteral(NegateLiteral(Result));
      GetSatSolver.SubmitClause;
// ai, ~bi, ~r  : ~ai & bi => ~r
      GetSatSolver.BeginConstraint;
      GetSatSolver.AddLiteral(a[i]);
      GetSatSolver.AddLiteral(NegateLiteral(b[i]));
      GetSatSolver.AddLiteral(NegateLiteral(Result));
      GetSatSolver.SubmitClause;
    end;
  end;
end;

function TBinaryArithmeticCircuit.EncodeBinaryRep(const n: TBigInt; a: TBitVector;
  nbits: Integer): TLiteral;
var
  P2: TBigInt;
  BitCount: Integer;
  i: Integer;
  AndResult: TBigInt;

begin
  P2 := BigIntFactory.GetNewMember.SetValue(2);
  BitCount := 1;

  while P2.CompareWith(n)<= 0 do
  begin
    Inc(BitCount);
    P2.Add(P2);
  end;
  BigIntFactory.ReleaseMemeber(P2);
  if BitCount < nbits then
    BitCount := nbits;

  if (GetRunTimeParameterManager.Verbosity and
    (1 shl VerbBinArithmCircuit)) <> 0 then
    WriteLn(BitCount, ' ', n.ToString, ' ', P2.ToString);

  a.Count := BitCount;
  for i := 0 to BitCount- 1 do
    if n.CheckBit(i) then
      a[i] := GetVariableManager.TrueLiteral
    else
      a[i] := GetVariableManager.FalseLiteral;

  Result := GetVariableManager.TrueLiteral;
end;

end.

