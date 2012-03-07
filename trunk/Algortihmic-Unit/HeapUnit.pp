unit HeapUnit; 
{
  Tested with mowlawn @ USACO OPEN11 GOLD.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { THeap }

  generic THeap<T>= class (TObject)
  private
    function GetCount: Integer;
    function GetMin: T;
    function GetSize: Integer; inline;

  type private
    TIsGreaterThanFunction= function (const a: T; const b: T): Boolean;
    TArrayOfT= array of T;

  var private
    FMembers: TArrayOfT;
    FCount: Integer;
    FIsGreaterThan: TIsGreaterThanFunction;

  public
    property Count: Integer read GetCount;
    property Size: Integer read GetSize;
    property Min: T read GetMin;

    constructor Create (HeapSize: Integer;
        GreaterThanFunction: TIsGreaterThanFunction);
    {
    Destroy does not Free the object passed to it.
    }
    destructor Destroy; override;

    procedure Insert (Data: T);
    procedure DeleteMin;

    procedure Print;


  end;

implementation

{ THeap }

function THeap.GetCount: Integer;
begin
  Result:= FCount;

end;

function THeap.GetMin: T;
begin
  Result:= FMembers [1];

end;

function THeap.GetSize: Integer;
begin
  Result:= High (FMembers)+ 1;

end;

constructor THeap.Create(HeapSize: Integer;
  GreaterThanFunction: TIsGreaterThanFunction);
begin
  inherited Create;

  SetLength (FMembers, HeapSize+ 1);
  FillChar (FMembers [0], SizeOf (FMembers), 0);
  FCount:= 1;
  FIsGreaterThan:= GreaterThanFunction;

end;

destructor THeap.Destroy;
begin
  SetLength (FMembers, 0);

  inherited Destroy;

end;

procedure THeap.Insert (Data: T);
var
  ActiveIndex: Integer;
  Temp: T;

begin
  FMembers [FCount]:= Data;
  Inc (FCount);
  if FCount= 2 then
    Exit;

  ActiveIndex:= FCount- 1;

  while FIsGreaterThan (FMembers [ActiveIndex div 2],
                        FMembers [ActiveIndex]) do
  begin
    Temp:= FMembers [ActiveIndex];
    FMembers [ActiveIndex]:= FMembers [ActiveIndex div 2];
    FMembers [ActiveIndex div 2]:= Temp;

    ActiveIndex:= ActiveIndex div 2;
    if ActiveIndex= 1 then
      Break;

  end;

end;

procedure THeap.DeleteMin;
var
  ActiveIndex: Integer;
  MinOfChildrenIndex: Integer;
  MinOfChildren: T;
  Temp: T;

begin
  FMembers [1]:= FMembers [FCount- 1];
  ActiveIndex:= 1;
  Dec (FCount);

  while 2* ActiveIndex< FCount do
  begin
    MinOfChildrenIndex:= 2* ActiveIndex;
    MinOfChildren:= FMembers [MinOfChildrenIndex];

    if MinOfChildrenIndex< FCount- 1 then
      if FIsGreaterThan (FMembers [2* ActiveIndex],
                         FMembers [2* ActiveIndex+ 1]) then
      begin
        Inc (MinOfChildrenIndex);
        MinOfChildren:= FMembers [MinOfChildrenIndex];

      end;

    if FIsGreaterThan (FMembers [ActiveIndex], MinOfChildren) then
    begin
      Temp:= FMembers [ActiveIndex];
      FMembers [ActiveIndex]:= MinOfChildren;
      FMembers [MinOfChildrenIndex]:= Temp;

      ActiveIndex:= MinOfChildrenIndex;

    end
    else
      Break;

  end;

end;

procedure THeap.Print;
{var
  i, j: Integer;
}
begin
  {
  Write ('(');

  for i:= 1 to FCount- 1 do
  begin
    j:= FMembers [i];
    Write (j, ' ');

  end;

  WriteLn (')');
 }
  Halt (1);

end;

end.

