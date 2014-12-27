unit PriorityQueueUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
    // assume priority is an integer
  { TPair }

  { TQPair }

  generic TQPair<TFirst, TSecond>= class(TObject)
  private
    FFirst: TFirst;
    FSecond: TSecond;
  public
    property First: TFirst read FFirst;
    property Second: TSecond read FSecond;
    constructor Create(f: TFirst; s: TSecond);
  end;


  { TPQueue }

  generic TPQueue<TKey, TPriority>= class(TObject)
  private
    type
      TQElementSpec = specialize TQPair<Tkey, TPriority>;

    TPairList = specialize TFPGList<TQElementSpec>;
    var
      FMembers: TPairList;//FM[0] -> highest priority

    function GetCount: Integer; inline;
    function GetKey(Index: Integer): TKey;
    function GetMember(Index: Integer): TQElementSpec; inline;
    function GetPriority(Index: Integer): TPriority;
  public
   property Count : Integer read GetCount;
    property Key[Index: Integer]: TKey read GetKey;
    property Priority[Index: Integer]: TPriority read GetPriority;

    procedure Insert(K: TKey; Prior: TPriority);
    function DeleteMin: TKey;
    // returns the new index
    function DecreaseKey(k: TKey; Delta: TPriority): Integer;

    constructor Create(Capacity: Integer = 100);
    destructor Destroy; override;
  protected
    property Member[Index: Integer] : TQElementSpec read GetMember;

  private
    function ParentIndex(Index: Integer): Integer; inline;
    procedure Heapify (Index: Integer);

  end;

implementation


{ TQPair }

constructor TQPair.Create(f: TFirst; s: TSecond);
begin
  inherited Create;

  FFirst := f;
  FSecond := s;
end;

{ TPQueue }

function TPQueue.GetMember(Index: Integer): TQElementSpec;
begin
  Result := FMembers[Index];
end;

function TPQueue.GetPriority(Index: Integer): TPriority;
begin
  Result := FMembers[Index].Second;
end;

function TPQueue.GetCount: Integer;
begin
  Result := FMembers.Count;
end;

function TPQueue.GetKey(Index: Integer): TKey;
begin
  Result := FMembers[Index].First;
end;

procedure TPQueue.Insert(K: TKey; Prior: TPriority);
var
  ActiveIndex: Integer;
  Temp: TQElementSpec;
begin
  FMembers.Add(TQElementSpec.Create(K, Prior));

  ActiveIndex:= FMembers.Count - 1;
  if 1< FMembers.Count then
    while Member[ParentIndex(ActiveIndex)].Second <
                         Member[ActiveIndex].Second do
    begin
      Temp := Member[ActiveIndex];
      FMembers[ActiveIndex]:= Member[ParentIndex(ActiveIndex)];
      FMembers[ParentIndex(ActiveIndex)]:= Temp;

      ActiveIndex := ParentIndex(ActiveIndex);
      if ActiveIndex = 0 then
        Break;
    end;
end;

function GetLeftChild(Index : Integer) : Integer;
begin
  Result := Index shl 1 + 1;
end;

function GetRightChild(Index : Integer) : Integer;
begin
  Result := Index shl 1 + 2;
end;

function TPQueue.DeleteMin: TKey;
begin
  Result := Member[0].First;
  FMembers[0].Free;

  FMembers.Items [0]:= FMembers.Items [FMembers.Count - 1];
  FMembers.Count:= Count - 1;

  Heapify (0);

end;

function TPQueue.DecreaseKey(k: TKey; Delta: TPriority): Integer;
begin

end;

constructor TPQueue.Create(Capacity: Integer);
begin
  inherited Create;

  FMembers := TPairList.Create;
  FMembers.Capacity := Capacity;
end;

destructor TPQueue.Destroy;
begin
  FMembers.Free;

  inherited Destroy;
end;

function TPQueue.ParentIndex(Index: Integer): Integer;
begin
  Result := (Index - 1) shr 1;
end;

procedure TPQueue.Heapify (Index: Integer);
var
  ActiveIndex: Integer;
  MaxOfChildrenIndex: Integer;
  MaxOfChildren: TQElementSpec;
  Temp: TQElementSpec;
begin
  ActiveIndex:= Index;

  while 2 * ActiveIndex + 1< Count do
  begin
    MaxOfChildrenIndex := 2 * ActiveIndex + 1;
    MaxOfChildren := Member[MaxOfChildrenIndex];

    if 2* ActiveIndex + 2 < Count then
      if Member[2 * ActiveIndex + 1].Second <
                         Member[2 * ActiveIndex + 2].Second then
      begin
        Inc(MaxOfChildrenIndex);
        MaxOfChildren := Member[MaxOfChildrenIndex];
      end;

    if MaxOfChildren.Second < Member[ActiveIndex].Second then
    begin
      Temp := Member[ActiveIndex];
      FMembers[ActiveIndex] := MaxOfChildren;
      FMembers[MaxOfChildrenIndex] := Temp;

      ActiveIndex := MaxOfChildrenIndex;
    end
    else
      Break;

  end;

end;


end.

