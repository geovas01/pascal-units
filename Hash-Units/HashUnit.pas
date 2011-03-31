unit HashUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionBaseUnit, LinkedList;
  
type
  THashCode= Cardinal;
  THahfunction= function (Data: TObject): THashCode;
  
  { THashEntry }

  THashEntry= class (TData)
  public
    function GetHashCode: THashCode; virtual; abstract;

  end;
  
  { THashTable }

  THashTable= class (TBaseCollection)
  private
    function _GetDataAt(HashCode: THashCode): TLinkedListNode;
    
  private
    FSlotCount: Integer;
    property _DataAt [HashCode: THashCode]: TLinkedListNode read
       _GetDataAt;

    function GetDataAt (HashCode: THashCode): TLinkedListNode;
    function FindData (Data: THashEntry): TLinkedListNode;

  public
    property DataAt [HashCode: THashCode]: TLinkedListNode read
       GetDataAt;
       
    constructor Create (SlotCount: Integer);
    
    function IsExists (NewData: THashEntry): Boolean;
    procedure Insert (NewData: THashEntry);
    
  end;

implementation
uses
  MyTypes;
  
{ THashTable }

function THashTable._GetDataAt (HashCode: THashCode): TLinkedListNode;
begin
  HashCode:= HashCode mod FSlotCount;
  
  Result:= Member [HashCode] as TLinkedListNode;
  if Result= nil then
  begin
    Result:= TLinkedListNode.Create;
    FMembers [HashCode]:= Result;
    
  end;

end;

function THashTable.GetDataAt (HashCode: THashCode): TLinkedListNode;
begin
  HashCode:= HashCode mod FSlotCount;
  Result:= Member [HashCode] as TLinkedListNode;
  
end;

function THashTable.FindData (Data: THashEntry): TLinkedListNode;
var
  Node: TLinkedListNode;
  
begin
  Node:= DataAt [Data.GetHashCode];
  if Node= nil then
    Result:= nil
  else
  begin
    while Node.Next<> nil do
    begin
      if Data.CompareTo (Node.Data)= 0 then
      begin
      
      end;
      
    end;
  end;
  
end;

constructor THashTable.Create (SlotCount: Integer);
var
  i: Integer;
  Ptr: PObject;

begin
  inherited Create;
  
  Allocate (SlotCount);
  Ptr:= GetPointerToFirst;;
  
  for i:= 1 to Size do
  begin
    Ptr^:= nil;
    Inc (Ptr);
    
  end;
  FSlotCount:= SlotCount;
  
end;

function THashTable.IsExists (NewData: THashEntry): Boolean;
begin

end;

procedure THashTable.Insert (NewData: THashEntry);
var
  HashCode: THashCode;
  
begin
  HashCode:= NewData.GetHashCode;
  _DataAt [HashCode].Insert (NewData);
  
end;

end.

