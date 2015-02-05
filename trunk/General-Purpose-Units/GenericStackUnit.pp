unit GenericStackUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, GenericCollectionUnit;

type

  { EStackIsEmpty }

  EStackIsEmpty= class(Exception)
    constructor Create;
  end;
  { TGenericAbstactStack }

  generic TGenericAbstactStack<TData>= class(TObject)
  end;
  { TGenericStack }

  {This stack is not suitable for primary data-types}
  generic TGenericStack<TData>= class(specialize TGenericAbstactStack<TData>)
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
    procedure Push(Data: Tdata);

    constructor Create;
    {
    TGenericStack does not free the members stored in it.
    }
    destructor Destroy; override;
    procedure Clear;

  end;

    { TGenericStack }

  { TGenericStackForBuildInData }

  generic TGenericStackForBuildInData<TData>= class(specialize TGenericAbstactStack<TData>)
  private type
    TDataCollection= specialize TGenericCollectionForBuiltInData<TData>;

  private
    Elements: TDataCollection;

    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetTop: TData;
  public
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Top: TData read GetTop;

    function Pop: TData;
    procedure Push(Data: Tdata);

    constructor Create;
    {
    TGenericStack does not free the members stored in it.
    }
    destructor Destroy; override;

    procedure Clear;

  end;

implementation

{ EStackIsEmpty }

constructor EStackIsEmpty.Create;
begin
  inherited Create('Stack is Empty');

end;

{ TGenericStackForBuildInData }

function TGenericStackForBuildInData.GetCount: Integer;
begin
  Result:= Top;

end;

function TGenericStackForBuildInData.GetIsEmpty: Boolean;
begin
  Result:=(Count= 0);

end;

function TGenericStackForBuildInData.GetTop: TData;
begin
  Result:= Elements.Item [Count- 1];

end;

function TGenericStackForBuildInData.Pop: TData;
begin
  if IsEmpty then
    raise EStackIsEmpty.Create;

  Result:= GetTop;
  Elements.Delete(Count- 1);

end;

procedure TGenericStackForBuildInData.Push(Data: Tdata);
begin
  Elements.AddItem(Data);

end;

constructor TGenericStackForBuildInData.Create;
begin
  inherited Create;

  Elements:= TDataCollection.Create;

end;

destructor TGenericStackForBuildInData.Destroy;
begin
  Elements.Free;

  inherited Destroy;

end;

procedure TGenericStackForBuildInData.Clear;
begin
  Elements.Clear;

end;

{ TGenericStack }

function TGenericStack.GetCount: Integer;
begin
  Result:= Stack.Count;

end;

function TGenericStack.GetIsEmpty: Boolean;
begin
  Result:=(Count= 0);

end;

function TGenericStack.GetTop: TData;
begin
  Result:= TData(Stack.Peek);

end;

function TGenericStack.Pop: TData;
begin
  Result:= TData(Stack.Pop);

end;

procedure TGenericStack.Push(Data: Tdata);
begin
  Stack.Push(Data);

end;

constructor TGenericStack.Create;
begin
  inherited Create;

  Stack:= TStack.Create;

end;

destructor TGenericStack.Destroy;
begin
  Stack.Free;

  inherited Destroy;
end;

procedure TGenericStack.Clear;
begin
//  Stack.Clear;

end;

end.

