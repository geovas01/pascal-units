unit BinaryEncodingForFactoringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FactoringUsingSATUnit, BigInt, SatSolverInterfaceUnit,
    BitVector;

type
  { TBinaryRepBasedFactorizer }

  TBinaryRepBasedFactorizer= class (TBaseFactorizerUsingSAT)
  public
    procedure GenerateCNF (n: TBigInt; SatSolver: TSATSolverInterface); override;

  end;


implementation

{ TBinaryRepBasedFactorizer }

procedure TBinaryRepBasedFactorizer.GenerateCNF (n: TBigInt;
  SatSolver: TSATSolverInterface);
var
  BitCount: Integer;
  P2: TBigInt;
  a, b: TBitVector;

begin
  P2:= BigIntFactory.GetNewMemeber.SetValue (2);
  BitCount:= 1;

  while P2.CompareWith (n)<= 0 do
  begin
    Inc (BitCount);
    P2.Add (P2);

  end;
  BigIntFactory.ReleaseMemeber (P2);

  WriteLn ('c Encoding ', n.ToString, ' needs ', BitCount, ' bits');
  WriteLn ('c n = a * b, where a is a ', (BitCount + 1) div 2, '-bit integer and b has a ', BitCount,'-bit integer.');
  a:= TBitVector.Create (BitCount- 1);
  b:= TBitVector.Create (BitCount);

end;

end.

