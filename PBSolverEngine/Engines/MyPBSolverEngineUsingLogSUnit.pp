unit MyPBSolverEngineUsingLogSUnit;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, AbstractMyPBSolverEngineUnit, BigInt, CollectionUnit;
{
  This solver uses the prime number P_i,\cdots,P_j as the modulos such that
    2^{\log S} < P_k < 2^{\log S+ 1}
}

type

  { TMyPBSolverEngineUsingLargeModulos }

  { TMyPBSolverEngineUsingLogSModulos }

  TMyPBSolverEngineUsingLogSModulos= class (TAbstractMyPBSolverEngine)
  protected
    function GenerateModulos (m: TBigInt): TIntegerCollection; override;

  public

  end;

implementation

{ TMyPBSolverEngineUsingLogSModulos }

function TMyPBSolverEngineUsingLogSModulos.GenerateModulos(m: TBigInt):
  TIntegerCollection;
var
  P2: TBigInt;

begin
  WriteLn(m.Log.ToString);
  Result := nil;
end;

end.

