unit SemaphoreUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unixtype, pthreads;

type
  ESemaphoreException = class(Exception);


  { TSemaphore }

  TSemaphore= class (TObject)
  private
    Handle: Sem_t;
    {Returns the value of the semaphore }
    function GetValue: Integer;

  public
    property Value: Integer read GetValue;

    constructor Create; overload;
    constructor Create (InitValue: Integer); overload;
    destructor Destroy; override;

    {
    Decreases the value of semphore, if it is greater than zero and returns immediately.
    or blocks the caller (if the value of semaphore is zero) until the value of semaphore
    is greater than one, i.e., someone calls Up method on the same object.
    }
    procedure Down;
    {
    Increases the value of semaphore.
    }
    procedure Up;

  end;

implementation
uses
  cthreads;

{ TSemaphore }

function TSemaphore.GetValue: Integer;
var
  cResult: cInt;

begin
  Result:= sem_getvalue (Handle, cResult);
  if Result<> 0 then
    raise ESemaphoreException.Create ('GetValue<> 0');

  Result:= cResult;

end;

constructor TSemaphore.Create;
begin
  inherited Create;
  if Sem_Init (Handle, 0, 0)<> 0 then
    raise ESemaphoreException.Create ('Sem_Init<> 0');

end;

constructor TSemaphore.Create(InitValue: Integer);
begin
  inherited Create;
  if Sem_Init (Handle, 0, InitValue)<> 0 then
    raise ESemaphoreException.Create ('Sem_Init<> 0');

end;

destructor TSemaphore.Destroy;
begin
  if Sem_Destroy (Handle)<> 0 then
    raise ESemaphoreException.Create ('Sem_Destroy<> 0');

  inherited Destroy;
end;

procedure TSemaphore.Down;
begin
  if Sem_Wait (Handle)<> 0 then
    raise ESemaphoreException.Create ('Sem_Wait<> 0');

end;

procedure TSemaphore.Up;
begin
  if Sem_Post (Handle)<> 0 then
    raise ESemaphoreException.Create ('Sem_Post<> 0');

end;

end.

