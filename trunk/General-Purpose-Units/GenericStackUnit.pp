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
    function GetIsEmpty: Boolean;
    function GetTop: TData;
  public
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Top: TData read GetTop;

    function Pop: TData;
    procedure Push (Data: Tdata);

    constructor Create;
    {
    TGenericStack does not free the members stored in it.
    }
    destructor Destroy; override;

  end;

implementation

{ TGenericStack }

function TGenericStack.GetCount: Integer;
begin
  Result:= Stack.Count;

end;

function TGenericStack.GetIsEmpty: Boolean;
begin
  Result:= (Count= 0);

end;

function TGenericStack.GetTop: TData;
begin
  Result:= TData (Stack.Peek);

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
  Stack.Free;

  inherited Destroy;
end;

end.

