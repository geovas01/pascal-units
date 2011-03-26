unit BinarySearchTreeUnit;

{$Mode objfpc}

interface
uses
  SysUtils;

type

  { TBSTree }

  generic TBSTree<TKey, TData>= class (TObject)
  type private

    PBSTNode= ^TBSTNode;

    TBSTNode= record
      Key: TKey;
      Data: TData;
      LChild: PBSTNode;
      RChild: PBSTNode;
      ChildrenCount: Integer;
      Parent: PBSTNode;

    end;

    TCompareFunction= function (const Item1, Item2: TKey): Integer;

  var private
    FRoot: PBSTNode;
    Compare: TCompareFunction;

  private
    procedure FreeNodes (Node: PBSTNode);
    function GetCount: Integer;
    {Index>= 0}
    function GetDataByIndex (Index: Integer): TData;
    function GetKeyByIndex (Index: Integer): TKey;
    function GetDataByKey (Key: TKey): TData;

    property Root: PBSTNode read FRoot;

    function CreateNewBSTNode (Key: TKey; Data: TData; Parent: PBSTNode): PBSTNode;

  public
    property DataByKey [Key: TKey]: TData read GetDataByKey;
    property KeyByIndex [Index: Integer]: TKey read GetKeyByIndex;
    property DataByIndex [Index: Integer]: TData read GetDataByIndex;
    property Count: Integer read GetCount;

    procedure Add (Key: TKey; Data: TData);
    procedure Replace (Key: TKey; Data: TData);
    function AddOrReplace (Key: TKey; Data: TData): Boolean;

    constructor Create (CompareFunction: TCompareFunction);
    destructor Destroy; override;

  end;

  EInvalidIndex= class (Exception)
  end;

  EKeyExistsInTree= class (Exception)
  end;

implementation

{ TBST }

procedure TBSTree.FreeNodes (Node: PBSTNode);
begin
  if Node= nil then
    Exit;

  FreeNodes (Node^.LChild);
  FreeNodes (Node^.RChild);
  Dispose (Node);

end;

function TBSTree.GetCount: Integer;
begin
  if Root= nil then
    Exit (0)
  else
    Exit (Root^.ChildrenCount+ 1);

end;

function TBSTree.GetDataByIndex (Index: Integer): TData;
var
  ActiveNode: PBSTNode;

begin
  FillChar (Result, SizeOf (Result), 0);

  if Root= nil then
    Exit;

  assert (Index> 0);
  assert (Index<= Root^.ChildrenCount+ 1);

  ActiveNode:= Root;
  while ActiveNode<> nil do
  begin
    if ActiveNode^.LChild<> nil then
    begin

      if Index<= ActiveNode^.LChild^.ChildrenCount+ 1 then
        ActiveNode:= ActiveNode^.LChild

      else if Index= ActiveNode^.LChild^.ChildrenCount+ 2 then
        Exit (ActiveNode^.Data)

      else
      begin
        ActiveNode:= ActiveNode^.RChild;
        Index-= (ActiveNode^.LChild^.ChildrenCount+ 2);

      end;

    end
    else
    begin
      if Index= 1 then
        Exit (ActiveNode^.Data)

      else
      begin
        ActiveNode:= ActiveNode^.RChild;
        Dec (Index);

      end;

    end;

  end;

end;

function TBSTree.GetKeyByIndex (Index: Integer): TKey;
var
  ActiveNode: PBSTNode;

begin
  FillChar (Result, SizeOf (Result), 0);

  if Root= nil then
    Exit;

  assert (Index> 0);
  assert (Index<= Root^.ChildrenCount+ 1);

  ActiveNode:= Root;
  while ActiveNode<> nil do
  begin
    if ActiveNode^.LChild<> nil then
    begin

      if Index<= ActiveNode^.LChild^.ChildrenCount+ 1 then
        ActiveNode:= ActiveNode^.LChild

      else if Index= ActiveNode^.LChild^.ChildrenCount+ 2 then
        Exit (ActiveNode^.Key)

      else
      begin
        ActiveNode:= ActiveNode^.RChild;
        Index-= (ActiveNode^.LChild^.ChildrenCount+ 2);

      end;

    end
    else
    begin
      if Index= 1 then
        Exit (ActiveNode^.Key)

      else
      begin
        ActiveNode:= ActiveNode^.RChild;
        Dec (Index);

      end;

    end;

  end;

end;

function TBSTree.GetDataByKey (Key: TKey): TData;
var
  ActiveNode: PBSTNode;
  CompareResult: Integer;

begin
  FillChar (Result, SizeOf (Result), 0);

  if Root= nil then
    Exit
  else
  begin
    ActiveNode:= Root;

    CompareResult:= Compare (ActiveNode^.Key, Key);

    while CompareResult<> 0 do
    begin
      if CompareResult< 0 then
        ActiveNode:= ActiveNode^.LChild
      else
        ActiveNode:= ActiveNode^.RChild;

      if ActiveNode= nil then
        Exit;

      CompareResult:= Compare (ActiveNode^.Key, Key);

    end;

    Result:= ActiveNode^.Data;

  end;

end;

function TBSTree.CreateNewBSTNode (Key: TKey; Data: TData; Parent: PBSTNode): PBSTNode;
begin
  New (Result);
  Result^.LChild:= nil;
  Result^.RChild:= nil;
  Result^.Key:= Key;
  Result^.Data:= Data;
  Result^.Parent:= Parent;
  Result^.ChildrenCount:= 0;

end;

procedure TBSTree.Add (Key: TKey; Data: TData);
var
  ActiveNode, ParentNode: PBSTNode;
  CompareResult: Integer;

begin
  if Root= nil then
  begin
    FRoot:= CreateNewBSTNode (Key, Data, nil);

  end
  else
  begin
    ActiveNode:= Root;
    ParentNode:= nil;

    CompareResult:= Compare (ActiveNode^.Key, Key);

    while CompareResult<> 0 do
    begin
      ParentNode:= ActiveNode;
      if CompareResult< 0 then
        ActiveNode:= ActiveNode^.LChild
      else
        ActiveNode:= ActiveNode^.RChild;

      if ActiveNode= nil then
      begin
        ActiveNode:= CreateNewBSTNode (Key, Data, ParentNode);

        if CompareResult< 0 then
          ParentNode^.LChild:= ActiveNode
        else
          ParentNode^.RChild:= ActiveNode;

        Break;

      end;

      CompareResult:= Compare (ActiveNode^.Key, Key);

    end;

    if CompareResult= 0 then
      raise EKeyExistsInTree.Create ('');

    ActiveNode:= ActiveNode^.Parent;
    while ActiveNode<> nil do
    begin
      Inc (ActiveNode^.ChildrenCount);
      ActiveNode:= ActiveNode^.Parent;

    end;

  end;

end;

procedure TBSTree.Replace (Key: TKey; Data: TData);
begin

end;

function TBSTree.AddOrReplace(Key: TKey; Data: TData): Boolean;
begin

end;

constructor TBSTree.Create (CompareFunction: TCompareFunction);
begin
  inherited Create;

  FRoot:= nil;
  Compare:= CompareFunction;

end;

destructor TBSTree.Destroy;
begin
  FreeNodes (Root);

  inherited Destroy;

end;

end.
