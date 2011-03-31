unit GenericStackUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TGenericStack }

  generic TGenericStack<TData>= class (TObject)
  private
    Stack: TStack;

    function GetCount: Integer;
  public
    property Count: Integer read GetCount;

    function Pop: TData;
    procedure Push (Data: Tdata);

    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TGenericStack }

function TGenericStack.GetCount: Integer;
begin
  Result:= Stack.Count;

end;

function TGenericStack.Pop: TData;
begin
  Result:= TData (Stack.Pop);

end;

procedure TGenericStack.Push (Data: Tdata);
begin
  Stack.Push (Data);

end;

constructor TGenericStack.Create;
begin
  inherited Create;

  Stack:= TStack.Create;

end;

destructor TGenericStack.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Stack.Count- 1 do
    Pop.Free;
  Stack.Free;

  inherited Destroy;
end;

end.

