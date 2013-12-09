unit BaseCircuitUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SatSolverInterfaceUnit;

type

  { TBaseCircuit }

  TBaseCircuit= class(TObject)
  private
    function GetSatSolver: TSATSolverInterface;

  public
    {
    Returns the global SatSolver returned by SatSolverInterfaceUnit.GetSatSolver
    }
    property SatSolver: TSATSolverInterface read GetSatSolver;

    constructor Create;
    destructor Destroy; override;

//    procedure GenerateCNF; virtual; abstract;

  end;

implementation

{ TBaseCircuit }

function TBaseCircuit.GetSatSolver: TSATSolverInterface;
begin
  Result:= SatSolverInterfaceUnit.GetSatSolver;
end;

constructor TBaseCircuit.Create;
begin
  inherited Create;

end;

destructor TBaseCircuit.Destroy;
begin
  inherited Destroy;
end;

end.

