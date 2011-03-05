unit Polynomial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TVariable }

  TVariable= class (TObject)
  private
    FName: String;

  public
    property Name: String read FName write FName;
    constructor Create (const AName: String);

  end;

  { TVariableCollection }

  TVariableCollection= class (TList)
  private
    function GetVariableByIndex(Index: Integer): TVariable;
    function GetVariableByName (const VarName: String): TVariable;
  public
    property VariableByIndex [Index: Integer]: TVariable read GetVariableByIndex;
    property VariableByName [VarName: String]: TVariable read GetVariableByName;

    function Copy: TVariableCollection;

    procedure Add (AVariable: TVariable);

  end;

  TCoef= class (TObject)
  private
  public

  end;

implementation

{ TVariable }

constructor TVariable.Create (const AName: String);
begin
  inherited Create;

  FName:= AName;

end;

{ TVariableCollection }

function TVariableCollection.GetVariableByIndex (Index: Integer): TVariable;
begin
  Result:= TVariable (Self [Index]);

end;

function TVariableCollection.GetVariableByName (const VarName: String): TVariable;
var
  UpperCaseName: String;
  i: Integer;

begin
  UpperCaseName:= UpperCase (VarName);

  for i:= 0 to Count- 1 do
    if UpperCase (VariableByIndex [i].Name)= UpperCaseName then
      Exit (VariableByIndex [i]);

  Result:= nil;

end;

function TVariableCollection.Copy: TVariableCollection;
var
  i: Integer;

begin
  Result:= TVariableCollection.Create;

  for i:= 0 to Count- 1 do
    Result.Add (VariableByIndex [i]);

end;

procedure TVariableCollection.Add (AVariable: TVariable);
begin
  inherited Add (AVariable);

end;

end.

