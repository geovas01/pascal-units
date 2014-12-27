unit BinaryModuloRSAFactorizerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RSAFactoringUsingSATUnit, BigInt;

type

  { TBinaryModuloFactorizer }

  TBinaryModuloRSAFactorizer = class(TBaseRSAFactorizerUsingSAT)
    public
      procedure GenerateCNF(n: TBigInt); override;
  end;

implementation

{ TBinaryModuloFactorizer }

procedure TBinaryModuloRSAFactorizer.GenerateCNF(n: TBigInt);
begin

end;

end.

