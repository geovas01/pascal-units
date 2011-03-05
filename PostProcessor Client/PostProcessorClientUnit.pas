unit PostProcessorClientUnit;

interface
uses
  IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, SysUtils, FeatureUnit, CollectionUnit, Classes,
  PostProcessorUnit;

type
  EDictionaryNotFound= class (Exception)
  public
    constructor Create (Msg: String);
    
  end;

  TDictionaryHandle= String;

  TPostProcessorClient= class (TObject)
  private
    PostProcessorEngineTCPConnection: TIdTCPClient;
    LoadedSVMHandles: TStringList;
    function GetDicHandle(Index: Integer): TDictionaryHandle;

  public
    property DicHandle [Index: Integer]: TDictionaryHandle read GetDicHandle;

    constructor Create (ServerHost: String; ServerPort: Integer); overload;

    procedure LoadDictionary (DicHandle: String);

    function AskQuery (DicHandle: TDictionaryHandle;
      Query: TPostProcessorWord; Depth: Integer= 5): TStringList;

    procedure LoadDictionaries (ConfigFilename: String);
    destructor Destroy; override;

  end;

implementation
uses
  Math;
  
{ TPostProcessorClient }

function TPostProcessorClient.AskQuery (DicHandle: TDictionaryHandle;
  Query: TPostProcessorWord; Depth: Integer= 5): TStringList;
var
  i, j, n: Integer;
  TempString: String;
   
begin
  PostProcessorEngineTCPConnection.WriteLn ('QUERY');
  PostProcessorEngineTCPConnection.WriteLn (DicHandle);
  PostProcessorEngineTCPConnection.WriteLn (IntToStr (Length (Query)));

  for i:= 0 to High (Query) do
  begin
    TempString:= '';
    for j:= 0 to Min (High (Query [i]), Depth) do
      TempString:= TempString+ ' '+ IntToStr (Query [i][j]);

    Delete (TempString, 1, 1);
    PostProcessorEngineTCPConnection.WriteLn (TempString);

  end;

  TempString:= '';
  n:= StrToInt (PostProcessorEngineTCPConnection.ReadLn);
  Result:= TStringList.Create;
   
  while 0< n do
  begin
    TempString:= PostProcessorEngineTCPConnection.ReadLn;
    Result.Add (TempString);
    Dec (n);

  end;

end;

constructor TPostProcessorClient.Create (ServerHost: String;
  ServerPort: Integer);
begin
  PostProcessorEngineTCPConnection:= TIdTCPClient.Create (nil);
  PostProcessorEngineTCPConnection.Host:= ServerHost;
  PostProcessorEngineTCPConnection.Port:= ServerPort;

  PostProcessorEngineTCPConnection.Connect;
  
end;

destructor TPostProcessorClient.Destroy;
begin
  PostProcessorEngineTCPConnection.Disconnect;
  PostProcessorEngineTCPConnection.Free;

  inherited;
  
end;

function TPostProcessorClient.GetDicHandle(
  Index: Integer): TDictionaryHandle;
begin

end;

procedure TPostProcessorClient.LoadDictionaries(ConfigFilename: String);
var
  InputFile: TextFile;
  S: String;

  function ReadNextLine: String;
  var
    S: String;

  begin
    Result:= '';
    
    while not Eof (InputFile) do
    begin
      ReadLn (InputFile, S);
      if S= '' then
        Continue;

      if S [1] in ['%', '\', '{'] then
        Continue;

      Result:= S;
      Exit;


    end;

    
  end;

begin
  AssignFile (InputFile, ConfigFilename);
  Reset (InputFile);

  try
    while not Eof (InputFile) do
    begin
      S:= ReadNextLine;
      if S<> '' then
        LoadDictionary (S);

    end;
    
  finally
    CloseFile (InputFile);
    
  end;


end;

procedure TPostProcessorClient.LoadDictionary (DicHandle: String);
var
  S: String;
  
begin
  PostProcessorEngineTCPConnection.WriteLn ('Load');
  PostProcessorEngineTCPConnection.WriteLn (DicHandle);
  S:= PostProcessorEngineTCPConnection.ReadLn;

  if UpperCase (S)<> UpperCase ('Success!') then
    raise EDictionaryNotFound.Create (DicHandle);  

end;

{ EDictionaryNotFound }

constructor EDictionaryNotFound.Create(Msg: String);
begin
  inherited Create ('No Dictionary with name= '+ Msg+ ' is founded!');
  
end;

end.
