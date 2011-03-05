unit BlockingQueueUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SemaphoreUnit, QueueUnit;

type

  { EQueueIsFull }

  EQueueIsFull= class (Exception)
    constructor Create;

  end;

  { EQueueIsEmpty }

  EQueueIsEmpty= class (Exception)
    constructor Create;
  end;

  { TBlockingQueue }

  generic TBlockingQueue<T>= class (TObject)
  private
    FList: TList;
    CS: TRTLCriticalSection;
    Semaphore: TSemaphore;

    function GetCount: Integer;

  public
    property Count: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;

    procedure Insert (Data: T); virtual;
    function Delete: T; virtual;
    function Front: T; virtual;

    function IsEmpty: Boolean; virtual;
    function IsFull: Boolean; virtual;

  end;


  { TBlockingCircularQueue }

  generic TBlockingCircularQueue<T>= class (TObject)
  private
    FList: TList;
    FoQ, EoQ: Integer;
    FCount: Integer;
    CS: TRTLCriticalSection;
    FSize: Integer;
    EmptyCount, FillCount: TSemaphore;

    function GetCount: Integer;

  public
    property Count: Integer read GetCount;
    property Size: Integer read FSize;

    constructor Create (QueueSize: Integer);
    destructor Destroy; override;

    procedure Insert (Data: T);
    function Delete: T;
    function Front: T;

    function IsEmpty: Boolean;
    function IsFull: Boolean;

  end;

implementation

{ TBlockingQueue }

function TBlockingQueue.GetCount: Integer;
begin
  EnterCriticalsection (CS);

  Result:= FList.Count;

  LeaveCriticalsection (CS);

end;

constructor TBlockingQueue.Create;
begin
  inherited Create;

  FList:= TList.Create;
  InitCriticalSection (CS);
  Semaphore:= TSemaphore.Create;

end;

destructor TBlockingQueue.Destroy;
begin
  FList.Free;
  Semaphore.Free;
  DoneCriticalsection (CS);

  inherited Destroy;

end;

procedure TBlockingQueue.Insert (Data: T);
begin
  EnterCriticalsection (CS);
  FList.Add (Data);
  LeaveCriticalsection (CS);

  Semaphore.Up;

end;

function TBlockingQueue.Delete: T;
begin
  Semaphore.Down;

  EnterCriticalsection (CS);

  Result:= T (FList [0]);
  FList.Delete (0);

  LeaveCriticalsection (CS);

end;

function TBlockingQueue.Front: T;
begin
  EnterCriticalsection (CS);

  Result:= T (FList [0]);

  LeaveCriticalsection (CS);

end;

function TBlockingQueue.IsEmpty: Boolean;
begin
  Result:= Count= 0;

end;

function TBlockingQueue.IsFull: Boolean;
begin
  Result:= False;

end;

{ EQueueIsFull }

constructor EQueueIsFull.Create;
begin
  inherited Create ('');

end;

{ EQueueIsEmpty }

constructor EQueueIsEmpty.Create;
begin
  inherited Create ('');

end;

{ TBlockingCircularQueue }

function TBlockingCircularQueue.GetCount: Integer;
begin
  Result:= FCount;

end;

constructor TBlockingCircularQueue.Create (QueueSize: Integer);
var
  i: Integer;

begin
  inherited Create;

  FSize:= QueueSize;
  FList:= TList.Create;
  FList.Count:= FSize;
  FCount:= 0;
  FoQ:= 0; EoQ:= 0;

  InitCriticalSection (CS);

  EmptyCount:= TSemaphore.Create (Size);
  FillCount:= TSemaphore.Create;
  WriteLn (EmptyCount.Value);

end;

destructor TBlockingCircularQueue.Destroy;
begin
  FList.Free;
  EmptyCount.Free;
  FillCount.Free;
  DoneCriticalsection (CS);

  inherited Destroy;

end;

procedure TBlockingCircularQueue.Insert (Data: T);
begin
  EmptyCount.Down;

  EnterCriticalsection (CS);

  FList [EoQ]:= Data;
  EoQ:= (EoQ+ 1) mod FSize;
  Inc (FCount);

  LeaveCriticalsection (CS);

  FillCount.Up;

end;

function TBlockingCircularQueue.Delete: T;
begin
  FillCount.Down;

  EnterCriticalsection (CS);

  Result:= T (FList [FoQ]);
  FoQ:= (FoQ+ 1) mod FSize;
  Dec (FCount);

  LeaveCriticalsection (CS);

  EmptyCount.Up;

end;

function TBlockingCircularQueue.Front: T;
begin
  EnterCriticalsection (CS);

  if IsEmpty then
  begin
    LeaveCriticalsection (CS);
    raise EQueueIsEmpty.Create;

  end;

  LeaveCriticalsection (CS);

end;

function TBlockingCircularQueue.IsEmpty: Boolean;
begin
  EnterCriticalsection (CS);

  Result:= FCount= 0;

  LeaveCriticalsection (CS);

end;

function TBlockingCircularQueue.IsFull: Boolean;
begin
  EnterCriticalsection (CS);

  Result:= FCount= FSize;

  LeaveCriticalsection (CS);

end;

end.

