unit BinaryArithmeticCircuitUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseArithmeticCircuitUnit, BitVectorUnit, ClauseUnit;

type

  {
  It considers the BitVectors represent the binary representation of numbers.
  This class assumes all numbers to be non-negative.

    BitVector V, with size n, encodes integer:
      \sum V [i]* 2^i
    In other word, the variable in V is True iff the integer represented by V is odd.
    The second variable in V is True iff the integer represented by V has one in its second bit,
    ....
  }
  { TBinaryArithmeticCircuit }

  TBinaryArithmeticCircuit= class(TBaseArithmeticCircuit)
  private
  public
    { Result = a + b}
    function Add (a, b: TBitVector): TBitVector; override;
    { Result = a - b}
    function Sub (a, b: TBitVector): TBitVector; override;
    { Result = a * b}
    function Mul (a, b: TBitVector): TBitVector; override;
    { Result = a div b}
    function Divide (a, b: TBitVector): TBitVector; override;

    { Result is True iff a< b}
    function IsLessThan (a, b: TBitVector): TLiteral; override;
    { Result is True iff a= b}
    function IsEqual (a, b: TBitVector): TLiteral; override;


  end;

const
  BinArithmCircuitADD: Integer = 2;
  BinArithmCircuitMUL: Integer = 3;
  BinArithmCircuitLessThan: Integer = 4;

implementation

uses
  Math, TSeitinVariableUnit, SatSolverInterfaceUnit,
  ParameterManagerUnit;
{ TBinaryArithmeticCircuit }

function TBinaryArithmeticCircuit.Add (a, b: TBitVector): TBitVector;
var
  Carry: TBitVector;
  MaxLen: Integer;
  i: Integer;
  ai, bi: TLiteral;
  aibi, aici_1, bici_1: TLiteral;

begin
  assert (1<= a.Count);
  assert (1<= b.Count);

  MaxLen:= Max (a.Count, b.Count);

  Carry:= TBitVector.Create(MaxLen);
  Result:= TBitVector.Create(MaxLen);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral (a [0]);
  SatSolver.AddLiteral (b [0]);
  SatSolver.SubmitAndGate(Carry [0]);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral (a [0]);
  SatSolver.AddLiteral (b [0]);
  SatSolver.SubmitXOrGate (Result [0]);

  if StrToInt (GetRunTimeParameterManager.ValueByName ['--Verbosity']) and
     (1 shl BinArithmCircuitADD)<> 0 then
  begin
    WriteLn ('[Add]: Carry = ', Carry.ToString);
    WriteLn ('[Add]: Result = ', Result.ToString);

  end;

  for i:= 1 to MaxLen- 1 do
  begin
    if i< a.Count then
      ai:= a [i]
    else
      ai:= GetVariableManager.FalseLiteral;

    if i< b.Count then
      bi:= b [i]
    else
      bi:= GetVariableManager.FalseLiteral;


    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (ai);
    SatSolver.AddLiteral (bi);
    SatSolver.AddLiteral (Carry [i- 1]);

    SatSolver.SubmitXOrGate (Result [i]);

    // a [i] and b [i] -> Carry [i]
    // a [i] and Carry [i- 1] -> Carry [i]
    // b [i] and Carry [i- ] -> Carry [i]
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (NegateLiteral (ai));
    SatSolver.AddLiteral (NegateLiteral (bi));
    SatSolver.AddLiteral (Carry [i]);
    SatSolver.SubmitClause;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (NegateLiteral (ai));
    SatSolver.AddLiteral (NegateLiteral (Carry [i- 1]));
    SatSolver.AddLiteral (Carry [i]);
    SatSolver.SubmitClause;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (NegateLiteral (bi));
    SatSolver.AddLiteral (NegateLiteral (Carry [i- 1]));
    SatSolver.AddLiteral (Carry [i]);
    SatSolver.SubmitClause;


    {
    Carry [i] and ~ai -> bi \land Carry [i- 1].
    Carry [i] and ~bi -> ai \land Carry [i- 1].
    Carry [i] and ~Carry[i-1] -> ai \land b [i].
    }
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (NegateLiteral (Carry [i]));
    SatSolver.AddLiteral (ai);
    SatSolver.AddLiteral (bi);
    SatSolver.SubmitClause;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (NegateLiteral (Carry [i]));
    SatSolver.AddLiteral (ai);
    SatSolver.AddLiteral (Carry [i- 1]);
    SatSolver.SubmitClause;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (NegateLiteral (Carry [i]));
    SatSolver.AddLiteral (bi);
    SatSolver.AddLiteral (ai);
    SatSolver.SubmitClause;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (NegateLiteral (Carry [i]));
    SatSolver.AddLiteral (bi);
    SatSolver.AddLiteral (Carry [i- 1]);
    SatSolver.SubmitClause;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (NegateLiteral (Carry [i]));
    SatSolver.AddLiteral (Carry [i- 1]);
    SatSolver.AddLiteral (ai);
    SatSolver.SubmitClause;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral (NegateLiteral (Carry [i]));
    SatSolver.AddLiteral (Carry [i- 1]);
    SatSolver.AddLiteral (bi);
    SatSolver.SubmitClause;


  end;

  if Carry [MaxLen- 1]<> TSeitinVariableUnit.GetVariableManager.FalseLiteral
     then
    Result.Add(Carry[MaxLen- 1]);


end;

{
  Instead of encoding c=a- b, the sub function encodes
    (a'= b+ c) and (a= a')
}
function TBinaryArithmeticCircuit.Sub (a, b: TBitVector): TBitVector;
var
  aPrime: TBitVector;
  aPrimeEqa: TLiteral;

begin
  Result:= TBitVector.Create (a.Count);

  aPrime:= Self.Add (b, Result);

  aPrimeEqa:= Self.IsEqual (aPrime, a);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral (aPrimeEqa);
  SatSolver.SubmitClause;

end;

function TBinaryArithmeticCircuit.Mul (a, b: TBitVector): TBitVector;
  function ParallelAdder (Mat: TBitVectorList; Start, Finish: Integer): TBitVector;
  var
    FirstHalf, SecondHalf: TBitVector;

  begin
    if Start= Finish then
    begin
      Result:= Mat [Start].Copy;

      if StrToInt (GetRunTimeParameterManager.ValueByName ['--Verbosity']) and
         (1 shl BinArithmCircuitMUL)<> 0 then
      begin
        WriteLn ('[Mul.ParallelAdder] Mat [', Start, '] = ', Result.ToString);

      end;

      Exit;
    end;

    FirstHalf:= ParallelAdder (Mat, Start, (Start+ Finish) div 2);
    SecondHalf:= ParallelAdder (Mat, (Start+ Finish) div 2+ 1, Finish);
    if StrToInt (GetRunTimeParameterManager.ValueByName ['--Verbosity']) and
       (1 shl BinArithmCircuitMUL)<> 0 then
    begin
      WriteLn ('[Mul.ParallelAdder] FirstHalf= ', FirstHalf.ToString);
      WriteLn ('[Mul.ParallelAdder] SecondHalf= ', SecondHalf.ToString);

    end;

    Result:= Self.Add (FirstHalf, SecondHalf);

    if StrToInt (GetRunTimeParameterManager.ValueByName ['--Verbosity']) and
       (1 shl BinArithmCircuitMUL)<> 0 then
    begin
      WriteLn ('[Mul.ParallelAdder] Mat [', Start, '] + ...+  Mat [', Finish, '] = ', Result.ToString);
    end;

    FirstHalf.Free;
    SecondHalf.Free;

  end;

var
  Mat: TBitVectorList;
  i, j: Integer;
  Temp: TBitVector;

begin
  Assert ((1<= a.Count) and (1<= b.Count));

  Mat:= TBitVectorList.Create;
  Mat.Capacity:= a.Count;

  for i:= 0 to a.Count- 1 do
    Mat.Add (TBitVector.Create (a.Count+ b.Count, GetVariableManager.FalseLiteral));

  for i:= 0 to a.Count- 1 do
  begin
   // Mat [i][k], k< i, False

   // Mat [i][i + j] <=> a [i] and b[j]
    for j:= 0 to b.Count- 1 do
    begin
      Mat [i][i+ j]:= CreateLiteral (GetVariableManager.CreateNewVariable, False);
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral (a [i]);
      SatSolver.AddLiteral (b [j]);
      SatSolver.SubmitAndGate (Mat [i][i+ j]);

    end;

  end;

  if StrToInt (GetRunTimeParameterManager.ValueByName ['--Verbosity']) and
     (1 shl BinArithmCircuitMUL)<> 0 then
  begin
    for i:= 0 to Mat.Count- 1 do
      WriteLn ('[MUL]: Mat [', i, ']= ', Mat [i].ToString);
    WriteLn ('[MUL]');

  end;

  Result:= ParallelAdder (Mat, 0, Mat.Count- 1);

  Mat.Free;

end;

function TBinaryArithmeticCircuit.Divide(a, b: TBitVector): TBitVector;
var
  aPrime: TBitVector;
  aPrimeEqa: TLiteral;

begin
  Result:= TBitVector.Create (a.Count);

  aPrime:= Self.Mul (b, Result);

  aPrimeEqa:= Self.IsEqual (aPrime, a);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral (aPrimeEqa);
  SatSolver.SubmitClause;

end;

function TBinaryArithmeticCircuit.IsLessThan (a, b: TBitVector): TLiteral;
var
  i: Integer;
  li: TLiteral;
  ailebi, aixorbi: TLiteral;
  aIsLessThanb,
  aiIsEqbi, aiIsLbi,
  aIsEqbTillNow: TBitVector;

begin//a< b
  Assert (a.Count= b.Count);

  aIsLessThanB:= TBitVector.Create (a.Count);

  aIsEqbTillNow:= TBitVector.Create (a.Count);

  aiIsEqbi:= TBitVector.Create (a.Count);
  aiIsLbi:= TBitVector.Create (a.Count);

  if GetRunTimeParameterManager.Verbosity and BinArithmCircuitLessThan<> 0 then
  begin
    WriteLn ('[IsLessThan] a: ', a.ToString);
    WriteLn ('[IsLessThan] b: ', b.ToString);
    WriteLn ('[IsLessThan] aiIsEqbi : ', aiIsEqbi.ToString);
    WriteLn ('[IsLessThan] aiIsLbi : ', aiIsLbi.ToString);
    WriteLn ('[IsLessThan] aiIsEqbTillNow : ', aIsEqbTillNow.ToString);

  end;

  aIsEqbTillNow.Add (GetVariableManager.TrueLiteral);

  for i:= a.Count- 1 downto 0 do
  begin
    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral (a [i]);
    GetSatSolver.AddLiteral (b [i]);

    GetSatSolver.SubmitXOrGate (NegateLiteral (aiIsEqbi [i]));//(not aixorbi) <=> ai= bi;

    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral (aiIsEqbi [i]);
    GetSatSolver.AddLiteral (aIsEqbTillNow [i+ 1]);
    GetSatSolver.SubmitAndGate (aIsEqbTillNow [i]);

  end;

  for i:= a.Count- 1 downto 0 do
  begin
    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral (NegateLiteral (a [i]));
    GetSatSolver.AddLiteral (b [i]);
    GetSatSolver.SubmitAndGate (aiIsLbi [i]);

    GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral (aiIsLbi [i]);
    GetSatSolver.AddLiteral (aIsEqbTillNow [i+ 1]);
    GetSatSolver.SubmitAndGate (aIsLessThanb [i]);

  end;

  Result:= CreateLiteral (GetVariableManager.CreateNewVariable, False);
  GetSatSolver.BeginConstraint;

  for i:= a.Count- 1 downto 0 do
    GetSatSolver.AddLiteral (aIsLessThanb [i]);
  GetSatSolver.SubmitOrGate (Result);

end;

function TBinaryArithmeticCircuit.IsEqual(a, b: TBitVector): TLiteral;
var
  i: Integer;
  aiAndbi: TLiteral;
  notaiAndnotbi: TLiteral;
  SameithBit: TLiteral;

begin
  Result:= CreateLiteral (GetVariableManager.CreateNewVariable, False);

  GetSatSolver.BeginConstraint;

  for i:= Min (a.Count- 1, b.Count- 1)+ 1 to
                    Max (a.Count- 1, b.Count- 1) do
  begin
    GetSatSolver.BeginConstraint;

    if i< a.Count then
      GetSatSolver.AddLiteral (NegateLiteral (a [i]))
    else
      GetSatSolver.AddLiteral (NegateLiteral (b [i]));

    GetSatSolver.SubmitClause;
  end;

  for i:= 0 to Min (a.Count- 1, b.Count- 1) do
  begin
    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral (a [i]);
    GetSatSolver.AddLiteral (b [i]);

    aiAndbi:= CreateLiteral (GetVariableManager.CreateNewVariable, False);
    GetSatSolver.SubmitAndGate (aiAndbi);//aiAndbi <=> ai and bi;

    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral (NegateLiteral (a [i]));
    GetSatSolver.AddLiteral (NegateLiteral (b [i]));

    notaiAndnotbi:= CreateLiteral (GetVariableManager.CreateNewVariable, False);
    GetSatSolver.SubmitAndGate (notaiAndnotbi);//notaiAndnotbi <=> not ai and  not bi;

    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral (aiAndbi);
    GetSatSolver.AddLiteral (notaiAndnotbi);

    SameithBit:= CreateLiteral (GetVariableManager.CreateNewVariable, False);
    GetSatSolver.SubmitOrGate (SameithBit);// SameithBit<-> aiAndbi or notaiAndnotbi;

    GetSatSolver.AddLiteral (SameithBit);

  end;

  Result:= CreateLiteral (GetVariableManager.CreateNewVariable, False);
  GetSatSolver.SubmitAndGate (Result);//Result<=> \bigwedge_i SameithBit;


end;

end.

