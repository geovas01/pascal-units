unit MyPBSolverEngineUsingLargeModuloUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractMyPBSolverEngineUnit, BigInt, CollectionUnit;
{
  This solver uses m+ 1 as the modulo. The encoding obtained using this encoder
   may have exponential size.
}

type

  { TMyPBSolverEngineUsingLargeModulos }

  TMyPBSolverEngineUsingLargeModulos= class (TAbstractMyPBSolverEngine)
  protected

    function GenerateModulos (m: TBigInt): TIntegerCollection; override;

  public

  end;

implementation

{ TMyPBSolverEngineUsingLargeModulos }

function TMyPBSolverEngineUsingLargeModulos.GenerateModulos (m: TBigInt): TIntegerCollection;
begin
  Result:= TIntegerCollection.Create;
  Result.AddItem (m.Copy.Incr.GetValue);

end;

end.

