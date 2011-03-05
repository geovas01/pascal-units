unit ConfigurationFileUnit;

{$mode objfpc}{$H+}

interface

uses
  MyFileStreamUnit,
  Classes, SysUtils, NameValueCollectionUnit;

type
  TStringNameValueCollection= specialize TGenericNameValueCollection<String>;

  { TConfigurationParser }

  TConfigurationParser= class (TObject)
  private
    FFileName: AnsiString;

  public
    constructor Create (Filename: AnsiString);
    destructor Destroy; override;

    function Load: TStringNameValueCollection;
    procedure Save (Configuration: TStringNameValueCollection);

  end;

implementation

{ TConfigurationParser }

constructor TConfigurationParser.Create (Filename: AnsiString);
begin
  inherited Create;

  FFileName:= Filename;

end;

destructor TConfigurationParser.Destroy;
begin
  inherited Destroy;

end;

function TConfigurationParser.Load: TStringNameValueCollection;
var
  FileStream: TMyFileStream;
  S, T: AnsiString;

begin
  FileStream:= TMyFileStream.Create (FFileName, fmOpenRead);

  Result:= TStringNameValueCollection.Create;

  while FileStream.EoS do
  begin
    S:= FileStream.ReadLn;
    if S<> '' then
      if S [1]<> '#' then
      begin
        T:= FileStream.ReadLn;
        Result.AddNameValue (S, T);

      end;

  end;

  Result.Finalize;
  FileStream.Free;

end;

procedure TConfigurationParser.Save (Configuration: TStringNameValueCollection);
var
  i: Integer;
  FileStream: TMyFileStream;

begin
  FileStream:= TMyFileStream.Create (FFilename, fmCreate);

  for i:= 0 to Configuration.Count- 1 do
  begin
    FileStream.WriteLn (Configuration.Strings [i]);
    FileStream.WriteLn (Configuration.ValueByIndex [i]);

  end;

  FileStream.Free;

end;

initialization

end.

