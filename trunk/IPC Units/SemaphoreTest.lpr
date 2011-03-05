program SemaphoreTest;

{$mode objfpc}{$H+}

uses
  cthreads, pthreads,
  Classes, SemaphoreUnit, sysutils
  { you can add units after this };

type

  { TConsumer }

  TConsumer= class (TThread)
  private
    FID: Integer;
    FSem: TSemaphore;

  public
    constructor Create (ID: Integer; Sem: TSemaphore);
    destructor Destroy; override;

    procedure Execute; override;

  end;

  { TProducer }

  TProducer= class (TThread)
  private
    FID: Integer;
    FSem: TSemaphore;

  public
    constructor Create (ID: Integer; Sem: TSemaphore);
    destructor Destroy; override;

    procedure Execute; override;

  end;

{ TProducer }

constructor TProducer.Create(ID: Integer; Sem: TSemaphore);
begin
  inherited Create (True);

  FSem:= Sem;
  FID:= ID;

end;

destructor TProducer.Destroy;
begin

  inherited Destroy;
end;

procedure TProducer.Execute;
var
  SleepTime: Integer;

begin
  while True do
  begin
    SleepTime:= Random (200);
//    WriteLn ('Producer ', FID, ' is going to sleep for ', SleepTime);
    Sleep (SleepTime);
//    WriteLn ('Produce ', FID, ' is awake');
    FSem.Up;
    WriteLn ('Producer ', FID, ' produces a new item [Total Items:', FSem.Value, ']');

  end;

end;

{ TConsumer }

constructor TConsumer.Create (ID: Integer; Sem: TSemaphore);
begin
  inherited Create (True);

  FID:= ID;
  FSem:= Sem;

end;

destructor TConsumer.Destroy;
begin
  WriteLn ('Thread ', FID, ' destroyed!');
  inherited Destroy;

end;

procedure TConsumer.Execute;
var
  SleepTime: Integer;

begin
  while True do
  begin
    SleepTime:= Random (100);
//    WriteLn ('Thread ', FID, ' is going to sleep for ', SleepTime);
    Sleep (SleepTime);

//    WriteLn ('Thread ', FID, ' is awake');
    FSem.Down;
    WriteLn ('Thread ', FID, ' consumed one item [Remaining Items:', FSem.Value, ']');

  end;

end;


var
  i, c, p: Integer;
  Threads: array of TThread;
  Sem: TSemaphore;
  SleepTime: Integer;

begin
  if Paramcount< 2 then
  begin
    WriteLn ('Invalid Usage!');
    WriteLn (ApplicationName, ' ConsumerThreadCount ProducerThreadCount');
    Exit;

  end;

  c:= StrToInt (ParamStr (1));
  p:= StrToInt (ParamStr (2));
  SetLength (Threads, c+ p+ 1);

  Sem:= TSemaphore.Create;

  for i:= 1 to c do
    Threads [i]:= TConsumer.Create (i, Sem);
  for i:= c+ 1 to c+ p do
    Threads [i]:= TProducer.Create (i- c, Sem);

  for i:= 1 to c+ p do
   Threads [i].Resume;

  ReadLn;

  Sem.Free;

end.

