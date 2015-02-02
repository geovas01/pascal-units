unit BinaryEncodingBasedFactoringUnit;

{$mode objfpc}{$H+}
{$ASSERTIONS on}
interface

uses
  FactoringUsingSATUnit, BigInt, BinaryArithmeticCircuitUnit;

type
  { TBinaryRepBasedFactorizer }

  TBinaryRepBasedFactorizer = class(TBaseFactorizerUsingSAT)
  public
    constructor Create;
    destructor Destroy; override;

 //   function GenerateCNF(const a, b: TBitVector; n: TBigInt): TLiteral; override;

  end;


implementation
{ TBinaryRepBasedFactorizer }

constructor TBinaryRepBasedFactorizer.Create;
begin
  inherited Create;

  ArithmeticCircuit.Free;
  ArithmeticCircuit:= TBinaryArithmeticCircuit.Create;

end;

destructor TBinaryRepBasedFactorizer.Destroy;
begin
  inherited Destroy;

end;

end.

