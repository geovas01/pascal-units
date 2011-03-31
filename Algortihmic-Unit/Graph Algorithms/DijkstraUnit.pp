unit DijkstraUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GraphUnit;

const
  InfCost= MaxInt div 3;

type

  { TAdjMat }

  TAdjMat= class (TObject)
  private
    FMat: array of array of Integer;
    FCount: Integer;
    FDefaultCost: Integer;

    function GetMat (Source, Dest: Integer): Integer;
    procedure SetMat (Source, Dest: Integer; const AValue: Integer);

  public
    property Cost [Source, Dest: Integer]: Integer
       read  GetMat write SetMat;
    property Count: Integer read FCount;

    constructor Create (n: Integer; DefaultCost: Integer= InfCost);
    destructor Destroy; override;

    procedure AddEdge (Source, Dest, EdgeCost: Integer);

  end;


  TIntArray= array of Integer;

procedure DijkstraWithAdjMat (const Mat: TAdjMat; Source: Integer; var Cost: TIntArray);
procedure DijkstraWithAdjList (const Mat: TAdjList; Source: Integer; var Cost: TIntArray);

implementation
type
  TVertexCostPair= record
    v: Integer;
    Cost: Integer;

  end;

  { TVertexCostPairHeap }

  TVertexCostPairHeap= class (TObject)
  private
    FData: array of TVertexCostPair;
    FCount: Integer;
    FIndexInHeap: array of Integer;
    function GetCost(Vertex: Integer): Integer;
    function GetData(Index: Integer): TVertexCostPair;

  public
    property Count: Integer read FCount;
    property Data [Index: Integer]: TVertexCostPair read GetData;
    property Cost [Vertex: Integer]: Integer read GetCost;

    constructor Create (n: Integer);
    destructor Destroy; override;

    function DeleteMin: TVertexCostPair;
    procedure UpdateCost (Vertex: Integer; NewCost: Integer);

    procedure Print;

  end;

{ TVertexCostPairHeap }

function TVertexCostPairHeap.GetData (Index: Integer): TVertexCostPair;
begin
  Result:= FData [Index];

end;

function TVertexCostPairHeap.GetCost (Vertex: Integer): Integer;
begin
  if FIndexInHeap [Vertex]= -1 then
    raise Exception.Create ('Vertex is not in Heap');

  Result:= FData [FIndexInHeap [Vertex]].Cost;

end;

constructor TVertexCostPairHeap.Create (n: Integer);
var
  i: Integer;

begin
  inherited Create;

  FCount:= n;
  SetLength (FIndexInHeap, FCount);
  SetLength (FData, FCount);

  for i:= 0 to Count- 1 do
  begin
    FData [i].v:= i;
    FData [i].Cost:= InfCost;
    FIndexInHeap [i]:= i;

  end;

  SetLength (FIndexInHeap, Count);

end;

destructor TVertexCostPairHeap.Destroy;
begin
  SetLength (FIndexInHeap, 0);
  SetLength (FData, 0);

  inherited Destroy;

end;

function TVertexCostPairHeap.DeleteMin: TVertexCostPair;
var
  RChilldIndex, LChildIndex: Integer;
  Index: Integer;
  MinValue, CurrentValue: Integer;
  MinChildIndex: Integer;
  TempNode: TVertexCostPair;

begin
  if Count= 0 then
    raise Exception.Create ('Heap is empty!');

  Result:= Data [0];
  FIndexInHeap [Result.v]:= -1;

  Index:= 0;
  FData [0]:= FData [Count- 1];
  FIndexInHeap [Data [0].v]:= 0;
  Dec (FCount);

  while 2* Index+ 1< Count do
  begin
    CurrentValue:= Data [Index].Cost;

    LChildIndex:= 2* Index+ 1;
    MinChildIndex:= LChildIndex;
    MinValue:= Data [MinChildIndex].Cost;

    RChilldIndex:= 2* Index+ 2;
    if RChilldIndex< Count then
      if Data [RChilldIndex].Cost< MinValue  then
      begin
        MinValue:= Data [RChilldIndex].Cost;
        MinChildIndex:= RChilldIndex;

      end;

    if MinValue< CurrentValue then
    begin
      TempNode:= Data [Index];
      FData [Index]:= Data [MinChildIndex];
      FData [MinChildIndex]:= TempNode;

      FIndexInHeap [Data [MinChildIndex].v]:= MinChildIndex;
      FIndexInHeap [Data [IndeX].v]:= Index;

      Index:= MinChildIndex;

    end
    else
      Break;

  end;

end;

procedure TVertexCostPairHeap.UpdateCost (Vertex: Integer; NewCost: Integer);
var
  Index, ParentIndex: Integer;
  TempNode: TVertexCostPair;

begin
  if FIndexInHeap [Vertex]= -1 then
    raise Exception.Create ('Node is not in heap!');

  Index:= FIndexInHeap [Vertex];
  if NewCost< Data [Index].Cost then
  begin
    FData [Index].Cost:= NewCost;

    while Index<> 0 do
    begin
      ParentIndex:= (Index- 1) div 2;

      if Data [Index].Cost< Data [ParentIndex].Cost then
      begin
        TempNode:= Data [Index];
        FData [Index]:= Data [ParentIndex];
        FData [ParentIndex]:= TempNode;

        FIndexInHeap [Data [ParentIndex].v]:= ParentIndex;
        FIndexInHeap [Data [Index].v]:= Index;

        Index:= ParentIndex;

      end
      else
        Break;

    end;

  end
  else
    raise Exception.Create ('Cost can not increase!');

end;

procedure TVertexCostPairHeap.Print;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    Write ('(', FData [i].v, ' ', FData [i].Cost, ')');
  WriteLn;
  for i:= 0 to Count do
    Write ('(', i, ',', FIndexInHeap [i], ')');
  WriteLn;
  Flush (Output);

end;

procedure DijkstraWithAdjMat (const Mat: TAdjMat; Source: Integer; var Cost: TIntArray);
var
  i: Integer;
  MyHeap: TVertexCostPairHeap;
  ActiveNodeInfo: TVertexCostPair;
  j, v: Integer;

begin
  MyHeap:= TVertexCostPairHeap.Create (Mat.Count);

  SetLength (Cost, Mat.Count);
  for v:= 0 to Mat.Count- 1 do
    Cost [v]:= InfCost;

  MyHeap.UpdateCost (Source, 0);

  for i:= 1 to Mat.Count do
  begin
    ActiveNodeInfo:= MyHeap.DeleteMin;

    Cost [ActiveNodeInfo.v]:= ActiveNodeInfo.Cost;
    if ActiveNodeInfo.Cost= InfCost then
      Break;

    for v:= 0 to Mat.Count- 1 do
      if ActiveNodeInfo.Cost< Cost [v] then
        if ActiveNodeInfo.Cost+ Mat.Cost [ActiveNodeInfo.v, v]<
             MyHeap.GetCost (v) then
          MyHeap.UpdateCost (v,
                 ActiveNodeInfo.Cost+ Mat.Cost [ActiveNodeInfo.v, v]);

  end;

end;

procedure DijkstraWithAdjList (const Mat: TAdjList; Source: Integer;
  var Cost: TIntArray);
var
  i: Integer;
  MyHeap: TVertexCostPairHeap;
  ActiveNodeInfo: TVertexCostPair;
  j, v: Integer;
  vNode: TAdjNode;

begin
  MyHeap:= TVertexCostPairHeap.Create (Mat.Count);

  SetLength (Cost, Mat.Count+ 1);
  for v:= 0 to Mat.Count do
    Cost [v]:= InfCost;
  MyHeap.UpdateCost (Source, 0);

  for i:= 1 to Mat.Count do
  begin
    ActiveNodeInfo:= MyHeap.DeleteMin;

    Cost [ActiveNodeInfo.v]:= ActiveNodeInfo.Cost;

    if ActiveNodeInfo.Cost= InfCost then
      Break;

    v:= ActiveNodeInfo.v;
    vNode:= Mat.AdjList [v].Next;

    while vNode<> nil do
    begin
      if ActiveNodeInfo.Cost< Cost [vNode.Vertex] then
        if ActiveNodeInfo.Cost+ vNode.Cost<
               MyHeap.GetCost (vNode.Vertex) then
          MyHeap.UpdateCost (vNode.Vertex,
                   ActiveNodeInfo.Cost+ vNode.Cost);

      vNode:= vNode.Next;

    end;

  end;

end;

{ TAdjMat }

function TAdjMat.GetMat (Source, Dest: Integer): Integer;
begin
  Result:= FMat [Source, Dest];

end;

procedure TAdjMat.SetMat (Source, Dest: Integer; const AValue: Integer);
begin
  FMat [Source, Dest]:= AValue;

end;

constructor TAdjMat.Create (n: Integer; DefaultCost: Integer);
var
  i, j: Integer;

begin
  inherited Create;

  FCount:= n;
  SetLength (FMat, Count);
  FDefaultCost:= DefaultCost;
  ;
  for i:= 0 to Count- 1 do
  begin
    SetLength (FMat, Count);

    for j:= 0 to Count- 1 do
      FMat [i, j]:= DefaultCost;

  end;

end;

destructor TAdjMat.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    SetLength (FMat, 0);
  SetLength (FMat, 0);
  FCount:= 0;

  inherited Destroy;

end;

procedure TAdjMat.AddEdge (Source, Dest, EdgeCost: Integer);
begin
  if Cost [Source, Dest]<> FDefaultCost then
    if EdgeCost< Cost [Source, Dest] then
      Cost [Source, Dest]:= EdgeCost;

end;

end.

