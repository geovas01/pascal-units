unit UnaryAdderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractAdderUnit, ClauseUnit;

type

  { TSNUnaryAdder }

  TSNUnaryAdder= class (TAbstractAdder)
  private
//    Carries: TLiteralCollection;

  public
    procedure AddExtraClauses_Medium; override;

    function Encode: TLiteralCollection; override;

    constructor Create (n1, n2: TLiteralCollection);
    destructor Destroy; override;

  end;

implementation
uses
  TSeitinVariableUnit;

{ TSNUnaryAdder }

procedure TSNUnaryAdder.AddExtraClauses_Medium;
begin
  Assert (False);

end;

function TSNUnaryAdder.Encode: TLiteralCollection;
begin
  Result:= nil;
  Assert (False);

end;

constructor TSNUnaryAdder.Create(n1, n2: TLiteralCollection);
begin
  inherited;

end;

destructor TSNUnaryAdder.Destroy;
begin
  inherited Destroy;
end;


end.

