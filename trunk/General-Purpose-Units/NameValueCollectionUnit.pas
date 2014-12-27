unit NameValueCollectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, ExceptionUnit;
  
type
  //Variable not found in a Collection
  EVariableNotFound= class(Exception);

  { ENameExistsInCollection }

  ENameExistsInCollection= class(Exception)
  public
    constructor Create(VarName: String);

  end;

  EInvalidNameValueFormat= class(Exception);

  { TNameValueCollection }

  {
  This class stores data with a correcponding name and allows retrieving the values
  by either their names or indices.

  This class does not delete the objects inserted into it.
  }

  { TGenericNameValueCollection }

  generic TGenericNameValueCollection<TValue> = class(TStringList)
  private
    function GetValueByIndex(Index: Integer): TValue;
    function GetValueByName(Name: AnsiString): TValue; virtual;
  public type

    { TNameValue }

    TNameValue= class(TObject)
      Value: TValue;
      constructor Create(v: TValue);

    end;

  var private
    function GetSize: Integer;
    
  public
    property Size: Integer read GetSize;
    property ValueByName[Name: AnsiString]: TValue read GetValueByName;
    property ValueByIndex[Index: Integer]: TValue read GetValueByIndex;

    procedure AddNameValue(NewName: AnsiString; NewValue: TValue); virtual;
    
    destructor Destroy; override;
    {
    Sorts the names for faster search. Note that after calling this method, there might
    be some changes in the order of the objects in the collection.
    }
    procedure Finalize;

    {
      Updates the value corresponding to AName or add a new entry.

      Caller should take care of deleting the old value.
    }
    procedure UpdateValue(AName: AnsiString; AValue: TValue);

    {
      Removes the pair Aname and its corresponding value for collection
    }
    procedure EraseValue(AName: AnsiString);
    
  end;

implementation

{ TGenericNameValueCollection.TNameValue }

constructor TGenericNameValueCollection.TNameValue.Create(v: TValue);
begin
  inherited Create;

  Value := v;
end;

{ ENameExistsInCollection }

constructor ENameExistsInCollection.Create (VarName: String);
begin
  inherited Create('Name: '+ VarName+ ' is already exists in the collection!');

end;

{ TGenericNameValueCollection }

function TGenericNameValueCollection.GetValueByIndex(Index: Integer): TValue;
begin
  Result :=(TNameValue(Objects[Index])).Value;

end;

function TGenericNameValueCollection.GetValueByName(Name: AnsiString): TValue;
var
  Index: Integer;

begin
  Index := Self.IndexOf(UpperCase(Name));

  if 0<= Index then
    Exit(ValueByIndex[Index])
  else
    raise ENameNotFound.Create(Name);

end;

function TGenericNameValueCollection.GetSize: Integer;
begin
  Result := Count;

end;

procedure TGenericNameValueCollection.AddNameValue(NewName: AnsiString; NewValue: TValue);
var
  NewNameValue: TNameValue;

begin
  NewNameValue := TNameValue.Create(NewValue);
  Self.AddObject(UpperCase(NewName), NewNameValue);

end;

destructor TGenericNameValueCollection.Destroy;
var
  i: Integer;
  
begin
  for i := 0 to Size- 1 do
    Objects[i].Free;
    
  inherited Destroy;
end;

procedure TGenericNameValueCollection.Finalize;
begin
  Sort;

end;

procedure TGenericNameValueCollection.UpdateValue(AName: AnsiString; AValue: TValue);
var
  ANameValue: TNameValue;

begin
  try
    ValueByName[AName];
    ANameValue := TNameValue(Objects[IndexOf(AName)]);
    ANameValue.Value := AValue;

  except
    on e: ENameNotFound do
      AddNameValue(AName, AValue);

  end;

end;

procedure TGenericNameValueCollection.EraseValue(AName: AnsiString);
begin
  ValueByName[AName];

  Objects[IndexOf(AName)].Free;
  Delete(IndexOf(AName));

end;

end.

