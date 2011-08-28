unit GenericCollectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TGenericCollection }

  generic TGenericCollection<TData>= class (TList)
  private
    function GetFirstItem: TData;
    function GetItem (Index: Integer): TData;
    function GetLastItem: TData;
    procedure SetItem (Index: Integer; const AValue: TData);

  public
    property Item [Index: Integer]: TData read GetItem write SetItem;
    property FirstItem: TData read GetFirstItem;
    property LastItem: TData read GetLastItem;

    constructor Create (Size: Integer);
    constructor Create;
    destructor Destroy; override;

    procedure AddItem (NewItem: TData);
    procedure AddAnotherCollection (AnotherCollection: TList);

    {
      Deletes the Index-th item from the list and return it.
      The item has not been freed, yet.
    }
    function Delete (Index: Integer): TData;

  end;

  { TGenericCollectionForBuiltInData }

  generic TGenericCollectionForBuiltInData<TData>= class (TList)
  private
    function GetItem (Index: Integer): TData;
    procedure SetItem (Index: Integer; const AValue: TData);

  public
    property Item [Index: Integer]: TData read GetItem write SetItem;

    constructor Create (Size: Integer);
    constructor Create;
    destructor Destroy; override;

    procedure AddItem (NewItem: TData);
    procedure AddAnotherCollection (AnotherCollection: TList);

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
begin
  inherited Create;

  Count:= Size;

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

{ TGenericCollectionForBuiltInData }

function TGenericCollectionForBuiltInData.GetItem (Index: Integer): TData;
begin
  Result:= TData (Get (Index));

end;

procedure TGenericCollectionForBuiltInData.SetItem (Index: Integer; const AValue: TData);
begin
  Items [Index]:= Pointer (AValue);

end;

constructor TGenericCollectionForBuiltInData.Create (Size: Integer);
begin
  inherited Create;

  Count:= Size;

end;

constructor TGenericCollectionForBuiltInData.Create;
begin
  inherited;

end;

destructor TGenericCollectionForBuiltInData.Destroy;
begin
  inherited Destroy;

end;

procedure TGenericCollectionForBuiltInData.AddItem (NewItem: TData);
begin
  inherited Add (nil);
  Items [Count- 1]:= Pointer (NewItem);

end;

procedure TGenericCollectionForBuiltInData.AddAnotherCollection(
  AnotherCollection: TList);
var
  i: Integer;

begin
  for i:= 0 to AnotherCollection.Count- 1 do
    Add (Pointer (AnotherCollection [i]));

end;

function TGenericCollectionForBuiltInData.Delete (Index: Integer): TData;
begin
  Result:= Item [Index];
  inherited Delete (Index);

end;


end.

