unit GenericNameValueCollectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit;

type

  { TGenericNameValueCollection }

  generic TGenericNameValueCollection<TData>= class (TStringList)
  private
    function GetItems(Index: Integer): TData;
    procedure SetItems(Index: Integer; const AValue: TData);
  public
    property Items [Index: Integer]: TData read GetItems write SetItems;

  end;

implementation

{ TGenericNameValueCollection }

function TGenericNameValueCollection.GetItems (Index: Integer): TData;
begin
  inherited Objects [Index];

end;

procedure TGenericNameValueCollection.SetItems(Index: Integer;
  const AValue: TData);
begin

end;

end.

