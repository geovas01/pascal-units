unit BinaryEncodingBasedRSAFactoringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RSAFactoringUsingSATUnit, BigInt;

type

  { TBinaryEncodingForRSAFactoring }

  TBinaryRepBasedRSAFactorizer = class(TBaseRSAFactorizerUsingSAT)
  public
    procedure GenerateCNF(n: TBigInt); override;
  end;

implementation

{ TBinaryEncodingForRSAFactoring }

procedure TBinaryRepBasedRSAFactorizer.GenerateCNF(n: TBigInt);
begin

end;

end.

