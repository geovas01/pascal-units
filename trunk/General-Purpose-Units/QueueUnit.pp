unit QueueUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type

  { EQueueIsFull }

  EQueueIsFull= class (Exception)
  public
    constructor Create;
    
  end;
  
  { EQueueIsEmpty }

  EQueueIsEmpty= class (Exception)
  public
    constructor Create;

  end;

  { TAbstractQueue }

  TAbstractQueue=  class (TObject)
  public

  end;

  { TGenericCircularQueue }

  generic TGenericCircularQueue<T>= class (TObject)
  private
    FData: array of T;
    SoQ, EoQ: Integer;
    FSize: Integer;

  public
    constructor Create (Size: Integer);
    destructor Destroy; override;

    procedure Insert (Entry: T);
    function Delete (var LastElement: T): Boolean;

  end;

  { TCircularQueue }

  TCircularQueue= class (TObject)
  private
    FData: array of TObject;
    SoQ, EoQ: Integer;
    FSize: Integer;

  public
    constructor Create (Size: Integer);
    destructor Destroy; override;

    procedure Insert (Entry: TObject);
    function Delete: TObject;

  end;

  { TGenericQueue }

  generic TGenericQueue<T>= class (TObject)
  private
    FData: TList;
    Size: Integer;

  public
    constructor Create;
    destructor Destroy;

    function IsEmpty: Boolean;
    function IsFull: Boolean;

    procedure Insert (Obj: T);
    function Delete: T;

  end;

  { TQueue }

  TQueue= class (TObject)
  private
    FData: array of TObject;
    Size,
    SoQ, EoQ: Integer;

  public
    constructor Create (n: Integer);

    procedure Insert (Obj: TObject);
    function Delete: TObject;

  end;

implementation

{ TQueue }

constructor TQueue.Create (n: Integer);
begin
  inherited Create;
  
  SetLength (FData, n);
  Size:= n;
  SoQ:= -1;
  EoQ:= -1;
  
end;

procedure TQueue.Insert (Obj: TObject);
begin
  if SoQ= Size then
  begin
    FData [SoQ]:= Obj;
    Inc (SoQ);
    
  end;
  
end;

function TQueue.Delete: TObject;
begin
  raise Exception.Create ('Not implemeted Yet!');

  if EoQ= SoQ then
  begin

  end;

end;

{ EQueueIsFull }

constructor EQueueIsFull.Create;
begin
  inherited Create ('Queue is full!');
  
end;

{ EQueueIsEmpty }

constructor EQueueIsEmpty.Create;
begin
  inherited Create ('Queue is empty!');

end;

{ TCircularQueue }

constructor TCircularQueue.Create (Size: Integer);
begin
  inherited Create;

  SetLength (FData, Size);

  SoQ:= 0;
  EoQ:= 0;
  FSize:= Size;

end;

destructor TCircularQueue.Destroy;
begin
  SetLength (FData, 0);
  
  inherited;
  
end;

procedure TCircularQueue.Insert (Entry: TObject);
begin
  if (EoQ+ 1) mod FSize= SoQ then
    raise EQueueIsFull.Create;

  FData [EoQ]:= Entry;
  EoQ:= (EoQ+ 1) mod FSize;

end;

function TCircularQueue.Delete: TObject;
begin
  if SoQ< EoQ then
  begin
    Result:= FData [SoQ];
    Inc (SoQ);

  end
  else
    Result:= nil;

end;

{ TGenericCircularQueue }

constructor TGenericCircularQueue.Create (Size: Integer);
begin                             
  inherited Create;

  SetLength (FData, Size);

end;

destructor TGenericCircularQueue.Destroy;
begin
  SetLength (FData, 0);

  inherited;

end;

procedure TGenericCircularQueue.Insert (Entry: T);
begin
  if (EoQ+ 1) mod FSize= SoQ then
    raise EQueueIsFull.Create;

  FData [EoQ]:= Entry;
  EoQ:= (EoQ+ 1) mod FSize;

end;

function TGenericCircularQueue.Delete (var LastElement: T): Boolean;
begin
  if SoQ< EoQ then
  begin
    Result:= True;
    LastElement:= FData [SoQ];
    Inc (SoQ);

  end
  else
    Result:= False;

end;

{ TGenericQueue }

constructor TGenericQueue.Create;
begin
  inherited Create;

  Size:= 0;
end;

destructor TGenericQueue.Destroy;
begin
  FData.Free;

  inherited Destroy;

end;

function TGenericQueue.IsEmpty: Boolean;
begin
  Result:= Size= 0;

end;

function TGenericQueue.IsFull: Boolean;
begin
  Result:= False;

end;

procedure TGenericQueue.Insert (Obj: T);
begin
  Inc (Size);
  FData.Add (Obj);

end;

function TGenericQueue.Delete: T;
var
  i: Integer;
begin
  if IsEmpty then
    raise EQueueIsEmpty.Create;

  Result:= FData [0];
  Dec (Size);
  FData.Delete (0);

end;

end.

