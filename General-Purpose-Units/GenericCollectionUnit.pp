unit GenericCollectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamUnit, gvector;

type
  TSortCompare = function (Item1, Item2: Pointer): Integer;
  { TGenericCollection }

  generic TGenericCollection<TData>= class(specialize TVector<TData>)
  private
    function GetCount: Integer; inline;
    function GetFirstItem: TData; virtual;
    function GetItem(Index: Integer): TData; inline;
    function GetLastItem: TData; inline;
    procedure SetItem(Index: Integer; const AValue: TData); inline;

  public
    property Item[Index: Integer]: TData read GetItem write SetItem;
    property FirstItem: TData read GetFirstItem;
    property LastItem: TData read GetLastItem;
    property Count : Integer read GetCount;

    constructor Create(Size_: Integer);
    constructor Create;
    destructor Destroy; override;

    procedure AddItem(NewItem: TData);
    procedure AddAnotherCollection(AnotherCollection: TList);

    {
      Deletes the Index-th item from the list and return it.
      The item has not been freed, yet.
    }
    function Delete(Index: Integer): TData;

    procedure Load(Stream: TMyTextStream); virtual;

    procedure Sort(Compare: TSortCompare);
  end;

  { TGenericCollectionForBuiltInData }

  generic TGenericCollectionForBuiltInData<TData>= class(TObject)
//  type
//    TGenericCollectionForBuiltInData_TData= specialize TGenericCollectionForBuiltInData<TData>;

  private
    function GetItem(Index: Integer): TData; inline;
    procedure SetCount(AValue: Integer); virtual;
    procedure SetItem(Index: Integer; const AValue: TData); inline;

  protected
    Items: array of TData;
    Capacity: Integer;
    FCount: Integer;

  public
    property Count: Integer read FCount write SetCount;
    property Item[Index: Integer]: TData read GetItem write SetItem;

    constructor Create(InitSize: Integer; InitValue: TData);

    {
    Note that Capacity is different from Count!
    }
    constructor Create(InitCap: Integer);
    constructor Create;
    destructor Destroy; override;

    procedure AddItem(NewItem: TData); virtual;
    procedure AddAnotherCollection(AnotherCollection: TGenericCollectionForBuiltInData);

    {
      Deletes the Index-th item from the list and return it.
    }
    function Delete(Index: Integer): TData;

    {
      Set Count to 0.
    }
    procedure Clear;
  end;

implementation

{ TGenericCollection }

function TGenericCollection.GetCount: Integer;
begin
  Exit(Size);
end;

function TGenericCollection.GetFirstItem: TData;
begin
  Result := Front;

end;

function TGenericCollection.GetItem(Index: Integer): TData;
begin
  Result := TData(Items[Index]);

end;

function TGenericCollection.GetLastItem: TData;
begin
  Result := Back;

end;

procedure TGenericCollection.SetItem(Index: Integer; const AValue: TData);
begin
  Items[Index] := AValue;

end;

constructor TGenericCollection.Create(Size_: Integer);
var
  i: Integer;

begin
  inherited Create;

  Resize(Size_);
  for i := 0 to Size - 1 do
    Item[i] := TData.Create;

end;

constructor TGenericCollection.Create;
begin
  inherited Create;

end;

destructor TGenericCollection.Destroy;
var
  i: Integer;

begin
  for i := 0 to Size - 1 do
    Item[i].Free;

  inherited Destroy;

end;

procedure TGenericCollection.AddItem(NewItem: TData);
begin
  inherited PushBack(NewItem);

end;

procedure TGenericCollection.AddAnotherCollection(AnotherCollection: TList);
var
  i: Integer;

begin
  for i := 0 to AnotherCollection.Count- 1 do
    Self.AddItem(TData(AnotherCollection[i]));

end;

function TGenericCollection.Delete(Index: Integer): TData;
begin
  Result := Item[Index];
  inherited Erase(Index);

end;

procedure TGenericCollection.Load(Stream: TMyTextStream);
begin
  //Do nothing

end;

procedure TGenericCollection.Sort(Compare: TSortCompare);
begin

end;

{ TGenericCollectionForBuiltInData }

function TGenericCollectionForBuiltInData.GetItem(Index: Integer): TData;
begin
  Result := Items[Index];

end;

procedure TGenericCollectionForBuiltInData.SetCount(AValue: Integer);
begin
  FCount := AValue;

  if Capacity< Count then
  begin
    Capacity := Count;
    SetLength(Items, Capacity);

  end;


end;

procedure TGenericCollectionForBuiltInData.SetItem(Index: Integer; const AValue: TData);
begin
  Items[Index] := AValue;

end;

constructor TGenericCollectionForBuiltInData.Create(InitSize: Integer;
  InitValue: TData);
var
  i: Integer;

begin
  inherited Create;

  SetLength(Items, InitSize);
  Capacity := InitSize;
  FCount := Capacity;

  for i := 0 to FCount- 1 do
    Items[i] := InitValue;

end;

constructor TGenericCollectionForBuiltInData.Create(InitCap: Integer);
begin
  inherited Create;

  SetLength(Items, InitCap);
  Capacity := InitCap;
  FCount := 0;

end;

constructor TGenericCollectionForBuiltInData.Create;
begin
  inherited Create;

  FCount := 0;
  Capacity := 0;
  SetLength(Items, Capacity);

end;

destructor TGenericCollectionForBuiltInData.Destroy;
begin
  SetLength(Items, 0);

  inherited Destroy;

end;

procedure TGenericCollectionForBuiltInData.AddItem(NewItem: TData);
begin
  if Count < Capacity then
  begin
    Items[Count] := NewItem;
    Inc(FCount);

  end
  else
  begin
    SetLength(Items, 2* Capacity+ 1);
    Capacity := 2* Capacity+ 1;
    AddItem(NewItem);

  end;

end;

procedure TGenericCollectionForBuiltInData.AddAnotherCollection(
     AnotherCollection: TGenericCollectionForBuiltInData);
var
  i: Integer;

begin
  for i := 0 to AnotherCollection.Count- 1 do
    AddItem(AnotherCollection.Item[i]);

end;

function TGenericCollectionForBuiltInData.Delete(Index: Integer): TData;
var
  i: Integer;

begin
  Result := Item[Index];
  for i := Index+ 1 to Count - 1 do
    Items[i- 1] := Items[i];
  Dec(FCount);

end;

procedure TGenericCollectionForBuiltInData.Clear;
begin
  Self.Count := 0;

end;


end.

