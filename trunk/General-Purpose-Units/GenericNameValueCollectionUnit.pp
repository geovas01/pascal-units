unit GenericNameValueCollectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ENameNotFound= class (Exception);

  { TGenericNameValueCollection }

  generic TGenericNameValueCollection<TData>= class (TStringList)
  private
    function GetItem (Index: Integer): TData;
    function GetItemByName (const AName: AnsiString): TData;

  public
    property Item [Index: Integer]: TData read GetItem;
    property ItembyName [const AName: AnsiString]: TData read GetItemByName;

    procedure AddItem (AName: AnsiString; Data: TData);

    destructor Destroy; override;

    {
      Deletes the Index-th item from the list and return it.
      The item has not been freed, yet.
    }
    function Delete (Index: Integer): TData;

  end;

implementation

{ TGenericNameValueCollection }

function TGenericNameValueCollection.GetItem (Index: Integer): TData;
begin
  inherited Objects [Index];

end;

function TGenericNameValueCollection.GetItemByName (const AName: AnsiString): TData;
begin
  if 0<= Self.IndexOf (AName) then
    Result:= TData (Self.Objects [IndexOf (AName)])
  else
    raise ENameNotFound.Create (AName);

end;

procedure TGenericNameValueCollection.AddItem (AName: AnsiString; Data: TData);
begin
  Self.AddObject (AName, Data);

end;

destructor TGenericNameValueCollection.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    Item [i].Free;

  inherited Destroy;

end;

function TGenericNameValueCollection.Delete (Index: Integer): TData;
begin
  Result:= Item [Index];
  inherited Delete (Index);

end;

end.

