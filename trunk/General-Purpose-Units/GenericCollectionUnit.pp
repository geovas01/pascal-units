unit GenericCollectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamUnit;

type
  { TGenericCollection }

  generic TGenericCollection<TData>= class (TList)
  private
    function GetFirstItem: TData; virtual;
    function GetItem (Index: Integer): TData; virtual;
    function GetLastItem: TData; virtual;
    procedure SetItem (Index: Integer; const AValue: TData); virtual;

  public
    property Item [Index: Integer]: TData read GetItem write SetItem;
    property FirstItem: TData read GetFirstItem;
    property LastItem: TData read GetLastItem;

    constructor Create (Size: Integer);
    constructor Create;
    destructor Destroy; override;

    procedure AddItem (NewItem: TData); virtual;
    procedure AddAnotherCollection (AnotherCollection: TList);

    {
      Deletes the Index-th item from the list and return it.
      The item has not been freed, yet.
    }
    function Delete (Index: Integer): TData;

    procedure Load (Stream: TMyTextStream); virtual;

  end;

  { TGenericCollectionForBuiltInData }

  generic TGenericCollectionForBuiltInData<TData>= class (TObject)
  private
    function GetItem (Index: Integer): TData; virtual;
    procedure SetCount (AValue: Integer); virtual;
    procedure SetItem (Index: Integer; const AValue: TData); virtual;
  protected
    Items: array of TData;
    Capacity: Integer;
    FCount: Integer;

  public
    property Count: Integer read FCount write SetCount;
    property Item [Index: Integer]: TData read GetItem write SetItem;

    constructor Create (InitSize: Integer; InitValue: TData);

    {
    Note that Capacity is different from Count!
    }
    constructor Create (InitCap: Integer);
    constructor Create;
    destructor Destroy; override;

    procedure AddItem (NewItem: TData); virtual;
    procedure AddAnotherCollection (AnotherCollection: TGenericCollectionForBuiltInData);

    {
      Deletes the Index-th item from the list and return it.
    }
    function Delete (Index: Integer): TData;


  end;

implementation

{ TGenericCollection }

function TGenericCollection.GetFirstItem: TData;
begin
  Result:= TData (Get (0));

end;

function TGenericCollection.GetItem (Index: Integer): TData;
begin
  Result:= TData (Get (Index));

end;

function TGenericCollection.GetLastItem: TData;
begin
  Result:= TData (Get (Count- 1));

end;

procedure TGenericCollection.SetItem (Index: Integer; const AValue: TData);
begin
  Items [Index]:= Pointer (AValue);

end;

constructor TGenericCollection.Create (Size: Integer);
var
  i: Integer;

begin
  inherited Create;

  Count:= Size;
  for i:= 0 to Count- 1 do
    Item [i]:= TData.Create;

end;

constructor TGenericCollection.Create;
begin
  inherited Create;

end;

destructor TGenericCollection.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    Item [i].Free;

  inherited Destroy;

end;

procedure TGenericCollection.AddItem (NewItem: TData);
begin
  inherited Add (NewItem);

end;

procedure TGenericCollection.AddAnotherCollection (AnotherCollection: TList);
var
  i: Integer;

begin
  for i:= 0 to AnotherCollection.Count- 1 do
    Self.AddItem (TData (AnotherCollection [i]));

end;

function TGenericCollection.Delete (Index: Integer): TData;
begin
  Result:= Item [Index];
  inherited Delete (Index);

end;

procedure TGenericCollection.Load (Stream: TMyTextStream);
begin
  //Do nothing

end;

{ TGenericCollectionForBuiltInData }

function TGenericCollectionForBuiltInData.GetItem (Index: Integer): TData;
begin
  Result:= Items [Index];

end;

procedure TGenericCollectionForBuiltInData.SetCount(AValue: Integer);
begin
  if FCount< AValue then
  begin
    FCount:= AValue;
    SetLength (Items, FCount);

  end;

end;

procedure TGenericCollectionForBuiltInData.SetItem (Index: Integer; const AValue: TData);
begin
  Items [Index]:= AValue;

end;

constructor TGenericCollectionForBuiltInData.Create (InitSize: Integer;
  InitValue: TData);
var
  i: Integer;

begin
  inherited Create;

  SetLength (Items, InitSize);
  Capacity:= InitSize;
  FCount:= Capacity;

  for i:= 0 to FCount- 1 do
    Items [i]:= InitValue;

end;

constructor TGenericCollectionForBuiltInData.Create (InitCap: Integer);
begin
  inherited Create;

  SetLength (Items, InitCap);
  Capacity:= InitCap;
  FCount:= 0;

end;

constructor TGenericCollectionForBuiltInData.Create;
begin
  inherited Create;

  FCount:= 0;
  Capacity:= 0;
  SetLength (Items, Capacity);

end;

destructor TGenericCollectionForBuiltInData.Destroy;
begin
  SetLength (Items, 0);

  inherited Destroy;

end;

procedure TGenericCollectionForBuiltInData.AddItem (NewItem: TData);
begin
  if Count< Capacity then
  begin
    Items [Count]:= NewItem;
    Inc (FCount);

  end
  else
  begin
    SetLength (Items, 2* Capacity+ 1);
    Capacity:= 2* Capacity+ 1;
    AddItem (NewItem);

  end;


end;

procedure TGenericCollectionForBuiltInData.AddAnotherCollection (
     AnotherCollection: TGenericCollectionForBuiltInData);
var
  i: Integer;

begin
  for i:= 0 to AnotherCollection.Count- 1 do
    AddItem (AnotherCollection.Item [i]);

end;

function TGenericCollectionForBuiltInData.Delete (Index: Integer): TData;
var
  i: Integer;

begin
  Result:= Item [Index];
  for i:= Index+ 1 to Count - 1 do
    Items [i- 1]:= Items [i];
  Dec (FCount);

end;


end.

