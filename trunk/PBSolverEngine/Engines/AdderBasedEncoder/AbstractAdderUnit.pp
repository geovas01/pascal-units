unit AbstractAdderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClauseUnit;

type

  { TAbstractAdder }

  TAbstractAdder= class (TObject)
  protected
    x, y: TLiteralCollection;

  public
    {
    n1 and n2 are stored in the object and will be freed whenever the
    destructor of adder has been called.
    }
    constructor Create (n1, n2: TLiteralCollection);
    destructor Destroy; override;

    {
    Returns the result of summation of x and y as a vector of literals (Result_1,...,Result_n)
    }
    function Encode: TLiteralCollection; virtual; abstract;

    procedure AddExtraClauses_Medium; virtual; abstract;

  end;

implementation

{ TAbstractAdder }

constructor TAbstractAdder.Create(n1, n2: TLiteralCollection);
begin
  inherited Create;

  x:= n1;
  y:= n2;

end;

destructor TAbstractAdder.Destroy;
begin
  x.Free;
  y.Free;

  inherited;

end;

end.

