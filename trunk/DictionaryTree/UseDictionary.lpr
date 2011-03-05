program UseDictionary;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  { you can add units after this } DictionaryTreeUnit, Classes, StreamUnit;

{$IFDEF WINDOWS}{$R UseDictionary.rc}{$ENDIF}

type

  { TStringKey }

  TStringKey= class (TAbstractKey)
  protected
    function GetMaxValue: Integer; override;
    function GetMinValue: Integer; override;
    function GetSize: Integer; override;
    function GetValueAt (Index: Integer): Integer; override;

    FKey: String;

  public
    constructor Create (Key: String);
    procedure SetKey (Key: String);

  end;

{ TStringKey }

function TStringKey.GetMaxValue: Integer;
begin
  Result:= 255;

end;

function TStringKey.GetMinValue: Integer;
begin
  Result:= 0;

end;

function TStringKey.GetSize: Integer;
begin
  Result:= Length (FKey);

end;

function TStringKey.GetValueAt(Index: Integer): Integer;
begin
  Result:= Ord (FKey [Index+ 1]);

end;

constructor TStringKey.Create (Key: String);
begin
  inherited Create;

  FKey:= Key;

end;

procedure TStringKey.SetKey (Key: String);
begin
  FKey:= Key;

end;

const
  ToBeInsertedStrings: array [1..4] of String=
     ('Amir', 'Amin', 'AMin', 'Min');

var
  Tree: TDictionaryTree;
  StrKey: TstringKey;
  Data: TObject;
  PStr: PString;
  Stream: TFileStream;
  i: Integer;

begin
  Tree:= TDictionaryTree.Create;
  StrKey:= TStringKey.Create ('');

  for i:= Low (ToBeInsertedStrings) to High (ToBeInsertedStrings) do
  begin
    PStr:= new (PString);
    PStr^:= ToBeInsertedStrings [i];
    StrKey.SetKey (PStr^);

    Tree.Insert (StrKey, TAbstractDataInNode (PStr), False);
    Data:= Tree.GetDataByKey (StrKey);
    PStr:= PString (Data);
    WriteLn (PStr^);

  end;

  Tree.Free;

end.

