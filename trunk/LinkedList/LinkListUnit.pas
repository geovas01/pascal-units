unit LinkListUnit;
{$mode objfpc}

interface

uses
  Classes, SysUtils;

type

  { TLinkListNode }

  generic TLinkList<TData>= class (TObject)
  private
    FData: TData;
  type private
    PLinkListNode= ^TLinkListNode;

    TLinkListNode= record
      Data: TData;
      Next: PLinkListNode;

    end;

    TCompareFunction=  function (const Item1, Item2: TData): Integer;

  var private
    FRoot: PLinkListNode;
    FCompareFunction: TCompareFunction;

    function GetNext (Node: PLinkListNode): PLinkListNode; inline;
    function GetData (Node: PLinkListNode): TData; inline;
    function CreateNewNode (d: TData): PLinkListNode; inline;

    function GetRoot: PLinkListNode; inline;
  var private

  public
    property Root: PLinkListNode read GetRoot;
    property Data: TData read FData;

    constructor Create (CompareFunction: TCompareFunction);
    destructor Destroy; override;

    procedure AddInLast (NewData: TData);
    procedure AddInFirstLast (NewData: TData);
    function IsExist (AData: TData): Boolean;
    function Find (AData: TData): PLinkListNode;

  end;

implementation

{ TLinkList }

function TLinkList.CreateNewNode (d: TData): PLinkListNode;
begin
  New (Result);
  Result^.Next:= nil;
  Result^.Data:= d;

end;

function TLinkList.GetNext (Node: PLinkListNode): PLinkListNode; inline;
begin
  Exit (Node^.Next);

end;

function TLinkList.GetData(Node: PLinkListNode): TData; inline;
begin
  Exit (Node^.Data);

end;

function TLinkList.GetRoot: PLinkListNode;
begin
  Exit (FRoot);

end;

constructor TLinkList.Create (CompareFunction: TCompareFunction);
begin
  inherited Create;

  FCompareFunction:= CompareFunction;

  FRoot:= nil;

end;

destructor TLinkList.Destroy;
var
  ActiveNode: PLinkListNode;

begin
  ActiveNode:= Root;

  while ActiveNode<> nil do
  begin
    FRoot:= ActiveNode^.Next;
    Dispose (ActiveNode);

  end;

  inherited Destroy;

end;

procedure TLinkList.AddInLast (NewData: TData);
var
  ActiveNode: PLinkListNode;

begin
  if Root= nil then
  begin
    FRoot:= CreateNewNode (NewData);
    Exit;

  end;

  ActiveNode:= Root;

  while ActiveNode^.Next<> nil do
    ActiveNode:= ActiveNode^.Next;

  ActiveNode^.Next:= CreateNewNode (NewData);

end;

procedure TLinkList.AddInFirstLast (NewData: TData);
var
  NewNode: PLinkListNode;

begin
  NewNode:= CreateNewNode (NewData);
  NewNode^.Next:= Root;
  FRoot:= NewNode;

end;

function TLinkList.IsExist (AData: TData): Boolean;
var
  ActiveNode: PLinkListNode;

begin
  ActiveNode:= Root;

  while ActiveNode<> nil do
  begin
    if FCompareFunction (ActiveNode^.Data, AData)= 0 then
      Exit (True);
    ActiveNode:= ActiveNode^.Next;

  end;

  Result:= False;

end;

function TLinkList.Find (AData: TData): PLinkListNode;
var
  ActiveNode: PLinkListNode;

begin
  ActiveNode:= Root;

  while ActiveNode<> nil do
  begin
    if FCompareFunction (ActiveNode^.Data, AData)= 0 then
      Exit (ActiveNode);
    ActiveNode:= ActiveNode^.Next;

  end;

  Result:= nil;

end;

end.

