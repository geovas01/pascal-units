unit HeapUnit; 
{
  Tested with mowlawn @ USACO OPEN11 GOLD.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { THeap }

  generic THeap<T>= class (TObject)
  private
    function Get (Index: Integer): T;

  private
    property Member [Index: Integer]: T read Get;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetMin: T;

    procedure Heapify (Index: Integer);
    function GetLeftChild (Index: Integer): Integer; inline;
    function GetRightChild (Index: Integer): Integer; inline;
    function GetParent (Index: Integer): Integer; inline;

  type
    TIsGreaterThanFunction= function (const a: T; const b: T): Boolean;

  var
    FMembers: TList;
    FIsGreaterThan: TIsGreaterThanFunction;

  public
    property Count: Integer read GetCount;
    property Capacity: Integer read GetCapacity;
    property Min: T read GetMin;

    constructor Create (GreaterThanFunction: TIsGreaterThanFunction;
        InitCapacity: Integer);
    {
    Destroy calls free of all the objects in the Heap objecty
    }
    destructor Destroy; override;
    procedure Clear;

    procedure Insert (Data: T);
    procedure DeleteMin;

    procedure Print;


  end;

implementation

{ THeap }

function THeap.Get (Index: Integer): T;
begin
  Result:= T (FMembers.Items [Index]);

end;

function THeap.GetCapacity: Integer;
begin
  Result:= FMembers.Capacity;

end;

function THeap.GetCount: Integer;
begin
  Result:= FMembers.Count;

end;

function THeap.GetMin: T;
begin
  Result:= Member [0];

end;

procedure THeap.Heapify (Index: Integer);
var
  ActiveIndex: Integer;
  MinOfChildrenIndex: Integer;
  MinOfChildren: T;
  Temp: T;

begin
  ActiveIndex:= Index;

  while 2* ActiveIndex+ 1< Count do
  begin
    MinOfChildrenIndex:= 2* ActiveIndex+ 1;
    MinOfChildren:= Member [MinOfChildrenIndex];

    if 2* ActiveIndex+ 2< Count then
      if FIsGreaterThan (Member [2* ActiveIndex+ 1],
                         Member [2* ActiveIndex+ 2]) then
      begin
        Inc (MinOfChildrenIndex);
        MinOfChildren:= Member [MinOfChildrenIndex];

      end;

    if FIsGreaterThan (Member [ActiveIndex], MinOfChildren) then
    begin
      Temp:= Member [ActiveIndex];
      FMembers [ActiveIndex]:= MinOfChildren;
      FMembers [MinOfChildrenIndex]:= Temp;

      ActiveIndex:= MinOfChildrenIndex;

    end
    else
      Break;

  end;

end;

function THeap.GetLeftChild (Index: Integer): Integer;
begin
  Result:= Index shl 1+ 1;

end;

function THeap.GetRightChild (Index: Integer): Integer;
begin
  Result:= Index shl 1+ 2;

end;

function THeap.GetParent (Index: Integer): Integer;
begin
  Result:= (Index- 1) shr 1;

end;

constructor THeap.Create (GreaterThanFunction: TIsGreaterThanFunction;
        InitCapacity: Integer);
begin
  inherited Create;

  FMembers:= TList.Create;
  FMembers.Capacity:= InitCapacity;

  FIsGreaterThan:= GreaterThanFunction;

end;

destructor THeap.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count - 1 do
    Member [i].Free;

  FMembers.Free;

  inherited Destroy;

end;

procedure THeap.Clear;
begin
  FMembers.Clear;

end;

procedure THeap.Insert (Data: T);
var
  ActiveIndex: Integer;
  Temp: T;

begin
  FMembers.Add (Data);

  ActiveIndex:= Count- 1;
  if 1< Count then
    while FIsGreaterThan (Member [GetParent (ActiveIndex)],
                          Member [ActiveIndex]) do
    begin
      Temp:= Member [ActiveIndex];
      FMembers [ActiveIndex]:= FMembers [GetParent (ActiveIndex)];
      FMembers [GetParent (ActiveIndex)]:= Temp;

      ActiveIndex:= GetParent (ActiveIndex);
      if ActiveIndex= 0 then
        Break;

    end;

end;

procedure THeap.DeleteMin;
begin
  FMembers.Items [0]:= FMembers.Items [Count- 1];

  Heapify (0);
  FMembers.Count:= Count- 1;

end;

procedure THeap.Print;
var
  i: Integer;
  Obj: T;

begin
  Write ('{');

  for i:= 0 to Count- 1 do
  begin
    Obj:= Member [i];
//    Write ('(', T (Obj).Value, ',', T (Obj).Pos, ')');

  end;

  WriteLn ('}');

end;

end.

