unit BaseArithmeticCircuitUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseCircuitUnit, BitVectorUnit, ClauseUnit,
  SatSolverInterfaceUnit, fgl;

type
  TBitVectorList= specialize TFPGList<TBitVector>;

  { TBaseArithmeticCircuit }

  {
  This class is the base class for arithmetic circuits. It does not
  have any assumption about the representation of numbers.
  }
  TBaseArithmeticCircuit= class (TBaseCircuit)
  private
  public
    {
    Generates appropriate set clauses (and submits each of them to
      SatSolver such that
     we have the returned BitVector is the result of a+ b
    }
    function Add (a, b: TBitVector): TBitVector; virtual; abstract;
    {
    Generates appropriate set clauses (and submits each of them to
      SatSolver such that
     we have the returned BitVector is the result of a+ b
    }
    function Sub (a, b: TBitVector): TBitVector; virtual; abstract;
    {
    Generates appropriate set clauses (and submits each of them to
       SatSolver such that
     we have the returned BitVector is the result of a+ b
    }
    function Mul (a, b: TBitVector): TBitVector; virtual; abstract;
    {
    Generates appropriate set clauses (and submits each of them to
       SatSolver such that
     the returned BitVector is the result of a div b.
    }

    function Divide (a, b: TBitVector): TBitVector; virtual; abstract;
    {
    Generates appropriate set clauses (and submits each of them to
       SatSolver such that
     the returned BitVector is the result of a mod b.
    }

    function Remainder (a, b: TBitVector): TBitVector; virtual;

    {
    Generates appropriate set clauses (and submits each of them to
       SatSolver such that the returning literal is true iff a< b.
    }
    function IsLessThan (a, b: TBitVector): TLiteral; virtual; abstract;
    {
    Generates appropriate set clauses (and submits each of them to
       SatSolver such that the returning literal is true iff a= b.
    }
    function IsEqual (a, b: TBitVector): TLiteral; virtual; abstract;
    {
    Generates appropriate set clauses (and submits each of them to
       SatSolver such that the returning literal is true iff a< b.
      The implememtation provided by the base class uses the following observation:
      a<= b iff a< b or a= b.
    }
    function IsLessThanOrEq (a, b: TBitVector): TLiteral; virtual;
    {
    Generates appropriate set clauses (and submits each of them to
       SatSolver such that the returning literal is true iff a> b.
      The implememtation provided by the base class uses the following observation:
      a> b iff not (a<= b).
    }
    function IsGreaterThan (a, b: TBitVector): TLiteral; virtual;
    {
    Generates appropriate set clauses (and submits each of them to
       SatSolver such that the returning literal is true iff a< b.
      The implememtation provided by the base class uses the following observation:
      a>= b iff not(a< b).
    }
    function IsGreaterThanOrEq (a, b: TBitVector): TLiteral; virtual;


    {
      Using a divide and conquer approach, this function builds a tower of
      adders whose depth is log of # of vectors in Nums.
    }
    function Add (Nums: TBitVectorList): TBitVector; virtual;


  end;

implementation

uses
  TSeitinVariableUnit;
{ TBaseArithmeticCircuit }

function TBaseArithmeticCircuit.Remainder (a, b: TBitVector): TBitVector;
var
  c, d, bd: TBitVector;
  EqLit, LeLit: TLiteral;

begin
  Result:= TBitVector.Create (b.Count);
  WriteLn ('[Remainder] Result = ', Result.ToString);

  d:= TBitVector.Create (a.Count);
  WriteLn ('[Remainder] d = ', d.ToString);
  bd:= Self.Mul(b, d);
  WriteLn ('[Remainder] b*d = ', bd.ToString);
  c:= Self.Add(bd, Result);
  WriteLn ('[Remainder] b*d + r= ', c.ToString);
  EqLit:= Self.IsEqual (a, c);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral (EqLit);
  SatSolver.SubmitClause;

  LeLit:= Self.IsLessThan (Result, b);
  SatSolver.BeginConstraint;
  SatSolver.AddLiteral (LeLit);
  SatSolver.SubmitClause;


end;

function TBaseArithmeticCircuit.IsLessThanOrEq (a, b: TBitVector): TLiteral;
var
  l1, l2: TLiteral;

begin
  l1:= IsLessThan(a, b);
  l2:= IsEqual(a, b);

  Result:= CreateLiteral (TSeitinVariableUnit.GetVariableManager.
                        CreateNewVariable, False);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(l1);
  SatSolver.AddLiteral(l2);
  SatSolver.SubmitOrGate(Result);

end;

function TBaseArithmeticCircuit.IsGreaterThan (a, b: TBitVector): TLiteral;
begin
  Result:= NegateLiteral (Self.IsLessThanOrEq (a, b));

end;

function TBaseArithmeticCircuit.IsGreaterThanOrEq (a, b: TBitVector): TLiteral;
begin
  Result:= NegateLiteral (IsLessThan (a, b));

end;

function TBaseArithmeticCircuit.Add (Nums: TBitVectorList): TBitVector;

  function RecBuild (Low, High: Integer): TBitVector;
  var
    Left, Right: TBitVector;

  begin
    if Low = High then
      Result:= Nums [Low]
    else
    begin
      Left:= RecBuild (Low, (Low+ High) div 2);
      Right:= RecBuild ((Low+ High) div 2+ 1, High);

      Result:= Self.Add (Left, Right);

    end;

  end;

begin
  if Nums.Count= 2 then
    Result:= Self.Add (Nums [0], Nums [1])
  else
    Result:= RecBuild (0, Nums.Count- 1);

end;


end.

