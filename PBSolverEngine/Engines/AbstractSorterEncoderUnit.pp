unit AbstractSorterEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClauseUnit, TSeitinVariableUnit;

type

  { TAbstractSorterEncoder }

  {
    Encode a sorter mod Modulo.
    The i-th output of Encode function will be true if
      the number of true literal among the InputLiterals is equal
      to i modulo Modulo.
  }
  TAbstractSorterEncoder= class (TObject)
  private
    FInputLiterals: TLiteralCollection;
    FVariableManager: TVariableManager;
    FModulo: Integer;

  protected
    property Modulo: Integer read FModulo;
    property VariableManager: TVariableManager read FVariableManager;

    procedure AddExtraClauses_Medium; virtual;
    procedure AddExtraClauses_High; virtual;

  public
    property InputLiterals: TLiteralCollection read FInputLiterals;

    {
      This class creates a copy from Inputs, and keeps the reference
         to VariableManager.
    }
    constructor Create (_VariableManager: TVariableManager;
                _Modulo: Integer;
                Inputs: TLiteralCollection);
    destructor Destroy; override;

    {
      Encode a sorter whose input literals are FInputLiterals and
            returns the output literals
     }
    function Encode: TLiteralCollection; virtual;
    procedure AddExtraClauses; virtual;

  end;

implementation
uses
  ParameterManagerUnit;

{ TAbstractSorterEncoder }

procedure TAbstractSorterEncoder.AddExtraClauses_Medium;
begin
  // Do nothing

end;

procedure TAbstractSorterEncoder.AddExtraClauses_High;
begin
  // Do nothing

end;

constructor TAbstractSorterEncoder.Create (_VariableManager: TVariableManager;
                _Modulo: Integer;
                Inputs: TLiteralCollection);
begin
  inherited Create;

  FVariableManager:= _VariableManager;
  FModulo:= _Modulo;
  FInputLiterals:= Inputs.Copy;

end;

destructor TAbstractSorterEncoder.Destroy;
begin
  InputLiterals.Free;

  inherited Destroy;
end;

function TAbstractSorterEncoder.Encode: TLiteralCollection;
var
  i: Integer;

begin
  if InputLiterals.Count= 0 then
  begin
    Result:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);
    Result.Items[0]:= VariableManager.TrueLiteral;

    for i:= 1 to Modulo- 1 do
      Result.Items[i]:= VariableManager.FalseLiteral;

  end
  else
    raise Exception.Create ('Should not reach here!');


end;

procedure TAbstractSorterEncoder.AddExtraClauses;
begin
  if VariableManager.SimulationMode then
    Exit;

  if (UpperCase (GetRunTimeParameterManager.GetValueByName ('--ExtraClausesLevel'))= UpperCase ('Medium')) or
     (UpperCase (GetRunTimeParameterManager.GetValueByName ('--ExtraClausesLevel'))= UpperCase ('High')) then
     AddExtraClauses_Medium;

  if UpperCase (GetRunTimeParameterManager.GetValueByName ('--ExtraClausesLevel'))= UpperCase ('High') then
     AddExtraClauses_High;

end;

end.

