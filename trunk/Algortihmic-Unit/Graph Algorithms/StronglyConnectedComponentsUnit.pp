unit StronglyConnectedComponentsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, GraphUnit;

type

  { TComponent }

  TComponent= class (TObject)
    FCount: Integer;
    FNodes: array of Integer;

    constructor Create;

  private
    function GetNode (Index: Integer): Integer;

  public
    property Count: Integer read FCount;
    property Node [Index: Integer]: Integer read GetNode;

    destructor Destroy; override;

  end;

  TComponents= specialize TGenericCollection <TComponent>;

  TGraph= record
    n: Integer;
    FMatrix: array of array of Boolean;
  end;

  {
   I assumed the vertices are indexed from 0 to Graph.VertexCount- 1
  }
  function ExtractStronglyConnectedComponents (Graph: TGraph): TComponents;

implementation

{ TComponent }

constructor TComponent.Create;
begin
  inherited Create;

  SetLength (FNodes, 0);
  FCount:= 0;

end;

function TComponent.GetNode (Index: Integer): Integer;
begin
  Result:= FNodes [Index];

end;

destructor TComponent.Destroy;
begin
  SetLength (FNodes, 0);

  inherited Destroy;

end;

function ExtractStronglyConnectedComponents (Graph: TGraph): TComponents;
type
  TIntArray= array of Integer;

var
  Marked: array of Boolean;

  procedure FirstDFS (u: Integer; var Time: Integer; var NodeRank: TIntArray);
  var
    v: Integer;
    NextNode: TAdjNode;

  begin
    if Marked [u] then
      Exit;

    Marked [u]:= True;

    for v:= 1 to Graph.n do
      if Graph.FMatrix [u, v] then
        FirstDFS (NextNode.Vertex, Time, NodeRank);

    NodeRank [u]:= Time;
    Inc (Time);

  end;

  procedure SecondDFS (u: Integer; ID: Integer; var CompID: TIntArray);
  var
    v: Integer;

  begin
    if Marked [u] then
      Exit;

    Marked [u]:= True;
    CompID [u]:= ID;

    for v:= 0 to Graph.n- 1 do
      if Graph.FMatrix [u, v] then
      begin
        SecondDFS (v, ID, CompID);
      end;

  end;

var
  NodeRank: TIntArray;
  Ranking: TIntArray;
  v: Integer;
  Time: Integer;
  CompIndex: TIntArray;
  ID: Integer;

begin
  SetLength (NodeRank, Graph.n+ 1);
  SetLength (Marked, Graph.n+ 1);

  for v:= 0 to Graph.n- 1 do
  begin
    Time:= 1;
    if NodeRank [v]<> 0 then
      FirstDFS (v, Time, NodeRank);

  end;
  Assert (Time= Graph.n+ 1);

  SetLength (Ranking, Graph.n+ 1);
  for v:= 0 to Graph.n- 1 do
    Ranking [NodeRank [v]]:= v;

  SetLength (CompIndex, Graph.n+ 1);
  FillChar (Marked [0], SizeOf (Marked), 0);

  ID:= 1;
  for v:= 0 to Graph.n- 1 do
    if not Marked [v] then
    begin
      SecondDFS (v, ID, CompIndex);
      Inc (ID);


    end;

  SetLength (NodeRank, 0);
  SetLength (Marked, 0);


end;

end.

