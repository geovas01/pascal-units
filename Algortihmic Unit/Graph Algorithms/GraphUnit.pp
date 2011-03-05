unit GraphUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TNode }

  TNode= class (TObject)
    Next: TNode;
    Vertex: Integer;

    constructor Create (v: Integer);
    destructor Destroy; override;

    function Add (NewNode: TNode): TNode;
    procedure Print;

  end;

  { TAdjNode }

  TAdjNode= class (TObject)
  private
    FVertex: Integer;
    FCost: Integer;
    FNext: TAdjNode;

  public
    property Vertex: Integer read FVertex;
    property Cost: Integer read FCost;
    property Next: TAdjNode read FNext;

    constructor Create (v: Integer; c: Integer);
    destructor Destroy; override;

    function Add (v: Integer; c: Integer): TAdjNode;
    function Remove (v: Integer): TAdjNode;


  end;

  { TAdjList }

  TAdjList= class (TObject)
  private
    FAdjNodes: array of TAdjNode;
    FCount: Integer;
    FEdgeCount: Integer;
    FMaxNode: Integer;
    function GetAdjList (v: Integer): TAdjNode;
    function GetDeg(v: Integer): Integer;
    function GetNeighbor(v: Integer; Index: Integer): TAdjNode;

  public
    property AdjList [v: Integer]: TAdjNode read GetAdjList;
    property Neighbor [v: Integer; Index: Integer]: TAdjNode read GetNeighbor;
    property Deg [v: Integer]: Integer read GetDeg;
    property Count: Integer read FCount;
    property EdgeCount: Integer read FEdgeCount;

    constructor Create (n: Integer);
    destructor Destroy; override;

    function Copy: TAdjList;
    procedure Print;

    function IsConnected: Boolean;

    function Solve: TNode;

    procedure AddEdge (v1, v2, c: Integer);

  end;

  {
    TGraph is a class for storing directed graphs. This class stores both the adjacency matrix and
    the adjacency list for the graph.
  }
  TGraph= class (TObject)
  private
    FMat: array of array of Boolean;
    FAdjList: TAdjList;

  public
    constructor Create (VNo: Integer);
    destructor Destroy; override;

    procedure AddEdge (v1, v2: Integer);

  end;

implementation

{ TAdjList }

function TAdjList.GetAdjList (v: Integer): TAdjNode;
begin
  Result:= FAdjNodes [v];
  if FMaxNode< v then
    FMaxNode:= v;

end;

function TAdjList.GetDeg (v: Integer): Integer;
var
  TempNode: TAdjNode;

begin
  TempNode:= AdjList [v].Next;

  Result:= 0;

  while TempNode<> nil do
  begin
    Inc (Result);
    TempNode:= TempNode.Next;

  end;

end;

function TAdjList.GetNeighbor (v: Integer; Index: Integer): TAdjNode;
var
  i: Integer;
  ActiveNode: TAdjNode;

begin
  ActiveNode:= AdjList [v];

  for i:= 1 to Index do
    ActiveNode:= ActiveNode.Next;


  Result:= ActiveNode;
end;

constructor TAdjList.Create (n: Integer);
var
  i: Integer;

begin
  inherited Create;

  FCount:= n;
  FEdgeCount:= 0;
  SetLength (FAdjNodes, n);
  FMaxNode:= -1;
  for i:= 0 to High (FAdjNodes) do
    FAdjNodes [i]:= TAdjNode.Create (-1, 0);

end;

destructor TAdjList.Destroy;
var
  i: Integer;

begin
  for i:= 0 to High (FAdjNodes) do
    FAdjNodes [i].Free;

end;

function TAdjList.Copy: TAdjList;
var
  i: Integer;
  ActiveNode: TAdjNode;

begin
  Result:= TAdjList.Create (Length (FAdjNodes));

  for i:= 0 to High (FAdjNodes) do
  begin
    ActiveNode:= FAdjNodes [i].Next;

    while ActiveNode<> nil do
    begin
      Result.AdjList [i].Add (ActiveNode.Vertex, ActiveNode.Cost);
      ActiveNode:= ActiveNode.Next;

    end;

  end;

end;

procedure TAdjList.Print;
var
  i: Integer;
  ActiveNode: TAdjNode;

begin
  for i:= 0 to FMaxNode do
  begin
    ActiveNode:= AdjList [i].Next;

    if ActiveNode<> nil then
      Write (i, ':');

    while ActiveNode<> nil do
    begin
      Write (ActiveNode.Vertex, ',');
      ActiveNode:= ActiveNode.Next;

    end;

    if AdjList [i].Next<> nil then
      Writeln;

  end;
end;

function TAdjList.IsConnected: Boolean;
var
  Reachable: array of Boolean;

  procedure DFS (v: Integer);
  var
    ActiveNode: TAdjNode;

  begin
    Reachable [v]:= True;

    ActiveNode:= AdjList [v];
    ActiveNode:= ActiveNode.Next;

    while ActiveNode<> nil do
    begin
      if not Reachable [ActiveNode.Vertex] then
        DFS (ActiveNode.Vertex);

      ActiveNode:= ActiveNode.Next;

    end;

  end;

var
  i: Integer;
  ActiveNode: TAdjNode;

begin
  SetLength (Reachable, Length (FAdjNodes));
  for i:= 0 to High (FAdjNodes) do
    Reachable [i]:= False;

  for i:= 0 to High (FAdjNodes) do
    if FAdjNodes [i].Next<> nil then
    begin
      DFS (i);
      Break;

    end;

  for i:= 0 to High (FAdjNodes) do
  begin
    ActiveNode:= AdjList [i].Next;
    if ActiveNode<> nil then
      if not Reachable [i] then
        Exit (False);

  end;

  Result:= True;

end;

{ TAdjNode }

constructor TAdjNode.Create(v: Integer; c: Integer);
begin
  inherited Create;

  FVertex:= v;
  FCost:= c;
  FNext:= nil;

end;

destructor TAdjNode.Destroy;
begin
  FNext.Free;

  inherited Destroy;
end;

function TAdjNode.Add (v: Integer; c: Integer): TAdjNode;
begin
  if FNext= nil then
  begin
    FNext:= TAdjNode.Create (v, c);
    Result:= FNext;

  end
  else
    Result:= FNext.Add (v, c);

end;

function TAdjNode.Remove (v: Integer): TAdjNode;
var
  Temp: TAdjNode;

begin
  if FNext<> nil then
    if FNext.Vertex= v then
    begin
      Temp:= Next.Next;
      Next.FNext:= nil;
      Next.Free;
      FNext:= Temp;

    end
    else
      FNext.Remove (v);

end;

{ TNode }

constructor TNode.Create (v: Integer);
begin
  inherited Create;

  Vertex:= v;
  Next:= nil;

end;

destructor TNode.Destroy;
begin
  Next.Free;

  inherited Destroy;

end;

function TNode.Add (NewNode: TNode): TNode;
begin
  if Next= nil then
  begin
    Next:= NewNode;
    Result:= Next;

  end
  else
    Result:= Next.Add (NewNode);

end;

procedure TNode.Print;
begin
  WriteLn (Vertex);
  if Next<> nil then
    Next.Print;

  Flush (Output);

end;

var
  TotalNoOfEdges: Integer;

function TAdjList.Solve: TNode;
var
  v1, v2, StartingVertex: Integer;
  Degree: Integer;
  Flag: Boolean;
  ActiveNode: TAdjNode;

  function _Solve (v1, v2: Integer): TNode;
  var
    i: Integer;
    RemovedEdges: Integer;
    TempGraph, NewGraph: TAdjList;
    ActiveNode: TAdjNode;
    ActiveResult: TNode;

  begin
    NewGraph:= Self.Copy;
//    NewGraph.Print;

    NewGraph.AdjList [v1].Remove (v2);
    NewGraph.AdjList [v2].Remove (v1);

//    WriteLn ('------');
//    NewGraph.Print;
    Flush (Output);

    if not NewGraph.IsConnected then
    begin
      NewGraph.Free;
      Exit (nil);

    end;

    Result:= TNode.Create (v1);
    ActiveResult:= Result.Add (TNode.Create (v2));

    v1:= v2;

    Flag:= True;
    RemovedEdges:= 1;

    for i:= 2 to TotalNoOfEdges do
    begin
      ActiveNode:= NewGraph.AdjList [v1].Next;
      Flag:= False;

      while ActiveNode<> nil do
      begin
//        NewGraph.Print;
//        Result.Print;
//        WriteLn;

        v2:= ActiveNode.Vertex;

        TempGraph:= NewGraph.Copy;
        TempGraph.AdjList [v1].Remove (v2);
        TempGraph.AdjList [v2].Remove (v1);

        if (i< TotalNoOfEdges) and TempGraph.IsConnected and (0< TempGraph.Deg [v2]) then
        begin
          Flag:= True;
          ActiveResult.Add (TNode.Create (v2));
          v1:= v2;
          NewGraph.Free;
          NewGraph:= TempGraph;
          Inc (RemovedEdges);
          Break;

        end
        else if i= TotalNoOfEdges then
        begin
          Flag:= True;
          ActiveResult.Add (TNode.Create (v2));
          v1:= v2;
          NewGraph.Free;
          NewGraph:= TempGraph;
          Inc (RemovedEdges);
          Break;

        end
        else
          ActiveNode:= ActiveNode.Next;

      end;
      if ActiveNode= nil then
        Exit (nil);

      ActiveNode:= NewGraph.AdjList [v1].Next;

    end;


  end;

begin
  Flag:= False;
  StartingVertex:= -1;

  for v1:= 0 to FMaxNode do
  begin
    Degree:= Deg [v1];

    if Odd (Degree) then
    begin
      StartingVertex:= v1;
      Break;

    end;

  end;

  if StartingVertex<> -1 then
  begin
    ActiveNode:= AdjList [StartingVertex].Next;

    while ActiveNode<> nil do
    begin
      Result:= _Solve (StartingVertex, ActiveNode.Vertex);
      if Result<> nil then
        Break;

      ActiveNode:= ActiveNode.Next;

    end;

  end
  else
    for v1:= 0 to FMaxNode do
      if AdjList [v1].Next<> nil then
      begin
        Result:= _Solve (v1, AdjList [v1].Next.Vertex);
        Break;

      end;

  Result.Print;

end;

procedure TAdjList.AddEdge (v1, v2, c: Integer);
var
  ActiveNode: TAdjNode;
  NewNode: TAdjNode;

begin
  ActiveNode:= AdjList [v1];

  while True do
  begin
    if ActiveNode.Next= nil then
    begin
      ActiveNode.FNext:= TAdjNode.Create (v2, c);
      Exit;

    end
    else if ActiveNode.Next.Vertex<= v2 then
      ActiveNode:= ActiveNode.Next
    else if v2< ActiveNode.Next.Vertex then
    begin
      Inc (FEdgeCount);
      NewNode:= TAdjNode.Create (v2, c);
      NewNode.FNext:= ActiveNode.Next;
      ActiveNode.FNext:= NewNode;
      Exit;

    end;

  end;

end;

{ TGraph }
constructor TGraph.Create (VNo: Integer);
var
  v: Integer;

begin
  inherited Create;

  SetLength (FMat, VNo);
  for v:= 0 to VNo- 1 do
  begin
    SetLength (FMat [v], VNo);
    FillChar (FMat [v], SizeOf (FMat [v]), 0);

  end;
  FAdjList:= TAdjList.Create (VNo);

end;

destructor TGraph.Destroy;
var
  v: Integer;

begin
  for v:= 0 to High (FMat) do
    SetLength (FMat [v], 0);
  SetLength (FMat, 0);

  inherited Destroy;

end;

procedure TGraph.AddEdge(v1, v2: Integer);
begin
  FMat [v1, v2]:= True;
  FAdjList.AddEdge (v1, v2, 1);

end;

end.

