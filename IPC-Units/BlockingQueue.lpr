program BlockingQueue;

{$DEFINE UseCThreads}
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, BlockingQueueUnit, QueueUnit, SysUtils
  { you can add units after this };


type
  TObjectBlockingQueue= specialize TBlockingQueue <TObject>;
  TObjectBlockingCirQueue= specialize TBlockingCircularQueue <TObject>;


  { TInserterThread }

  TInserterThread= class (TThread)
  private
    ID: Integer;
    FQueue: TObjectBlockingQueue;
    FCirQueue: TObjectBlockingCirQueue;
    FStop: Boolean;

  public
    constructor Create (n: Integer; Q: TObjectBlockingQueue);
    constructor Create (n: Integer; Q: TObjectBlockingCirQueue);
    procedure Execute; override;

    procedure Stop;

  end;

  { TDeleterThread }

  TDeleterThread= class (TThread)
  private
    ID: Integer;
    FQueue: TObjectBlockingQueue;
    FCirQueue: TObjectBlockingCirQueue;

  public
    constructor Create (n: Integer; Q: TObjectBlockingQueue);
    constructor Create (n: Integer; Q: TObjectBlockingCirQueue);
    procedure Execute; override;

  end;

{ TDeleterThread }

constructor TDeleterThread.Create(n: Integer; Q: TObjectBlockingQueue);
begin
  inherited Create (True);

  ID:= n;
  FQueue:= Q;
  FCirQueue:= nil;
  FreeOnTerminate:= False;

end;

constructor TDeleterThread.Create(n: Integer; Q: TObjectBlockingCirQueue);
begin
  inherited Create (True);

  ID:= n;
  FCirQueue:= Q;
  FQueue:= nil;
  FreeOnTerminate:= False;

end;

procedure TDeleterThread.Execute;
var
  SleepTime: Integer;
  Counter: Integer;
  Data: TStringList;

begin
   Counter:= 0;

  while True do
  begin
    SleepTime:= Random (100);
//    WriteLn ('Deleter ID=', ID, 'I am going to sleep for ', SleepTime);
    Sleep (SleepTime);
//    WriteLn ('Deleter ID=', ID, 'I am awake!');
    if FCirQueue= nil then
      Data:= TStringList (FQueue.Delete)
    else
      Data:= TStringList (FCirQueue.Delete);

    WriteLn ('ID=', ID, 'I Deleted ', Data.Text);
    Data.Free;

  end;

end;

{ TInserterThread }

constructor TInserterThread.Create (n: Integer; Q: TObjectBlockingQueue);
begin
  inherited Create (True);

  ID:= n;
  FreeOnTerminate:= False;
  FQueue:= Q;
  FCirQueue:= nil;
  FStop:= False;

end;

constructor TInserterThread.Create(n: Integer; Q: TObjectBlockingCirQueue);
begin
  inherited Create (True);

  ID:= n;
  FreeOnTerminate:= False;
  FCirQueue:= Q;
  FQueue:= nil;
  FStop:= False;


end;

procedure TInserterThread.Execute;
var
  SleepTime: Integer;
  Counter: Integer;
  Data: TStringList;

begin
   Counter:= 0;

  while not FStop do
  begin
    SleepTime:= Random (100);
//    WriteLn ('Inserter ID=', ID, 'I am going to sleep for ', SleepTime);
    Sleep (SleepTime);
//    WriteLn ('Inserter ID=', ID, 'I am awake!');
    Data:= TStringList.Create;
    Inc (Counter);
    Data.Add (IntToStr (ID)+ '-'+ IntToStr (Counter));
    WriteLn ('ID=', ID, 'I am going to insert ', Data.Text);
    if FCirQueue= nil then
      FQueue.Insert (Data)
    else
      FCirQueue.Insert (Data);

  end;

end;

procedure TInserterThread.Stop;
begin
  FStop:= True;

end;

var
  InsThreads: array [0..100] of TInserterThread;
  DelThreads: array [0..100] of TDeleterThread;
  i: Integer;
  P: Pointer;
  Queue: TObjectBlockingQueue;
  CirQueue: TObjectBlockingCirQueue;

begin
  if Paramcount< 3 then
  begin
    WriteLn ('Invalid Usage!');
    WriteLn (ApplicationName+ ' NumberofInserterThreads NumberOfDeleterThreads QueueSize');
    Exit;

  end;

  Queue:= nil;
  CirQueue:= nil;
  if ParamStr (3)= '-1' then
    Queue:= TObjectBlockingQueue.Create
  else
    CirQueue:= TObjectBlockingCirQueue.Create (StrToInt (ParamStr (3)));

  for i:= 1 to StrToInt (ParamStr (1)) do
  begin
    if CirQueue= nil then
      InsThreads [i]:= TInserterThread.Create (i, Queue)
    else
      InsThreads [i]:= TInserterThread.Create (i, CirQueue);
    InsThreads [i].Resume;

  end;

  for i:= 1 to StrToInt (ParamStr (2)) do
  begin
    if CirQueue= nil then
      DelThreads [i]:= TDeleterThread.Create (i, Queue)
    else
      DelThreads [i]:= TDeleterThread.Create (i, CirQueue);

    DelThreads [i].Resume;

  end;

  ReadLn;

  for i:= 1 to StrToInt (ParamStr (1)) do
  begin
    InsThreads [i].Stop;
    InsThreads [i].Terminate;
    InsThreads [i].Free;

  end;

  ReadLn;
  for i:= 1 to StrToInt (ParamStr (2)) do
    DelThreads [i].Terminate;


end.

