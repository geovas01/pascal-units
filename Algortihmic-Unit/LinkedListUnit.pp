unit LinkedListUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TData= TObject;
  
type

  { TLinkedListNode }

  TLinkedListNode= class (TObject)
  protected
    FData: TData;
    FNext: TLinkedListNode;

  public
    property Data: TData read FData;
    property Next: TLinkedListNode read FNext;
    
    constructor Create (d: TData);
    destructor Destroy; override;
    
    function Add (AData: TData): TLinkedListNode; virtual;
    
  end;

  TLinkedList= class (TObject)
  protected
    FRoot: TLinkedListNode;
    FTail: TLinkedListNode;

  public
    property Root: TLinkedListNode read FRoot;
    property Tail: TLinkedListNode read FTail;

    procedure AddData (d: TData); virtual;

    constructor Create;
    destructor Destroy; override;

  end;

  { TDoubleLinkedListNode }

  TDoubleLinkedListNode= class (TLinkedListNode)
  private
    FPrev: TLinkedListNode;
    
  public
    property Prev: TLinkedListNode read FPrev;

    constructor Create (d: TData; PreviousNode: TLinkedListNode);
    destructor Destroy; override;

    function Add (AData: TData): TLinkedListNode; override;

  end;

  TDoubleLinkedList= class (TLinkedList)
  public

    procedure AddData (d: TData); override;

  end;


implementation

{ TLinkedListNode }

{constructor TLinkedListNode.Create;
begin
  inherited Create;
  
  FData:= nil;
  FNext:= nil;
  
end;
}

constructor TLinkedListNode.Create (d: TData);
begin
  inherited Create;
  
  FData:= d;
  FNext:= nil;
  
end;

destructor TLinkedListNode.Destroy;
begin
  FData.Free;
  FNext.Free;
  
  inherited Destroy;
  
end;

function TLinkedListNode.Add (AData: TData): TLinkedListNode;
begin
  if FNext<> nil then
    Result:= FNext.Add (AData)
  else
  begin
    FNext:= TLinkedListNode.Create (AData);
    Result:= FNext;

  end;
    
end;

{ TDoubleLinkedList }

constructor TDoubleLinkedListNode.Create(d: TData; PreviousNode: TLinkedListNode
  );
begin
  inherited Create (d);
  
  FPrev:= PreviousNode;
  
end;

destructor TDoubleLinkedListNode.Destroy;
begin
  FNext.Free;
  FData.Free;
  
  FNext:= nil;
  FPrev:= nil;
  FData.Free;
  
  inherited Destroy;

end;

function TDoubleLinkedListNode.Add (AData: TData): TLinkedListNode;
begin
  if FNext<> nil then
    Result:= FNext.Add (AData)
  else
  begin
    FNext:= TDoubleLinkedListNode.Create (AData, Self);
    Result:= FNext;
    
  end;

end;

{ TLinkedList }

procedure TLinkedList.AddData (d: TData);
begin
  if FRoot= nil then
  begin
    FRoot:= TLinkedListNode.Create (d);
    FTail:= FRoot;

  end
  else
    FTail:= FRoot.Add (d);

end;

constructor TLinkedList.Create;
begin
  inherited;

  FRoot:= nil;
  FTail:= nil;
end;

destructor TLinkedList.Destroy;
begin
  inherited Destroy;
end;

{ TDoubleLinkedList }

procedure TDoubleLinkedList.AddData (d: TData);
begin
  if FRoot= nil then
    FRoot:= TDoubleLinkedListNode.Create (d, nil)
  else
    inherited AddData(d);

end;

end.

