unit ScreenManagerUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, CollectionBaseUnit;

type

  { TARow }

  TARow= class (TBaseCollection)
  private
    function GetPColor (Index: Integer): PColor;
    
    function GetColor (Index: Integer): TColor;
    procedure SetColor (Index: Integer; const AValue: TColor);
    
  public
    property Pixels [Index: Integer]: TColor read GetColor
       write SetColor;
    
    constructor Create (Sz: Integer);
    procedure Free;
    
  end;
  
  { PColorCollection }

  TPColorCollection= class (TBaseCollection)
  private
    function GetColorPtr (Index: Integer): PColor;
    
  public
    property ColorPtr [Index: Integer]: PColor read GetColorPtr;
    
    procedure Free;
    procedure Add (NewPtr: PColor);
    
  end;
  
  { TScreenManager }

  TScreenManager= class (TBaseCollection)
  private
    function GetRow (Index: Integer): TARow;
    AllRowPtrs: TPColorCollection;
    
  private
    property Row [Index: Integer]: TARow read GetRow;
    
  public
//    property
    constructor Create (Rows, Cols: Integer);
    procedure Free;
    
  end;
  
implementation
uses
  MyTypes;
  
{ TARow }

function TARow.GetPColor (Index: Integer): PColor;
begin
  Result:= PColor (Member [Index]);
  
end;

function TARow.GetColor (Index: Integer): TColor;
begin
  Result:= GetPColor (Index)^;
  
end;

procedure TARow.SetColor (Index: Integer; const AValue: TColor);
begin
  GetPColor (Index)^:= AValue;
  
end;

constructor TARow.Create (Sz: Integer);
var
  i: Integer;
  
begin
  inherited Create;

  for i:= 1 to sz do
    Add (TObject (New (PColor)));

end;

procedure TARow.Free;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointer;
  for i:= 1 to Size do
  begin
    Dispose (PColor (Ptr));
    Inc (Ptr);
    
  end;
  
  inherited;
  
end;

{ TPColorCollection }

function TPColorCollection.GetColorPtr (Index: Integer): PColor;
begin
  Result:= PColor (Member [Index]);
  
end;

procedure TPColorCollection.Free;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointer;
  
  for i:= 1 to Size do
  begin
    Dispose (PColor (Ptr^));
    Inc (Ptr);
    
  end;
  
  inherited Free;
  
end;

procedure TPColorCollection.Add (NewPtr: PColor);
begin
  inherited Add (TObject (NewPtr));
  
end;

{ TScreenManager }

function TScreenManager.GetRow(Index: Integer): TARow;
begin
  Result:= Member [Index] as TARow;
  
end;

constructor TScreenManager.Create (Rows, Cols: Integer);
var
  i: Integer;
  
begin
  inherited Create;
  
  AllRowPtrs:= TPColorCollection.Create;
  
  for i:= 1 to Cols do
  begin
//    Add (TARow.Create (Rows));
//    AllRowPtrs.Add (Row [i- 1].GetPointer (0)^);
    
    
  end;
  
end;

procedure TScreenManager.Free;
begin

end;

end.

