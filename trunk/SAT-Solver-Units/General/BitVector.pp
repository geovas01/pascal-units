unit BitVector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, TSeitinVariableUnit, ClauseUnit;

type

  { TBitVector }

  TBitVector= class (specialize TFPGList<TLiteral>)
  private
    function GetBit(Index: Integer): TLiteral;
  public
    property Bit [Index: Integer]: TLiteral read GetBit;

    constructor Create (Size: Integer);

  end;

implementation

{ TBitVector }

function TBitVector.GetBit(Index: Integer): TLiteral;
begin
  Result:= Self [Index];

end;

constructor TBitVector.Create(Size: Integer);
begin
  inherited Create;

  Count:= Size;

end;

end.

