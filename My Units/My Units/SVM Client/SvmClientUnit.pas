unit SvmClientUnit;

interface
uses
  IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, SysUtils, FeatureUnit, CollectionUnit;
  
type
  ESVMNotFound= class (Exception)
  public
    constructor Create (KernelFileName, SvmFileName: String);
    
  end;

  TSVMHandle= Integer;

  TSVMClient= class(TObject)
  private
    RecognitionEngineTCPConnection: TIdTCPClient;
    LoadedSVMs: array of TSVMHandle;
    function GetSVMHandle (SVMName, Kernel: String): TSVMHandle;

  public
    property SVMHandle [SVMName, KernelName: String]: TSVMHandle read GetSVMHandle;

    constructor Create (ServerHost: String; ServerPort: Integer); overload;

    function OpenSVM (KernelFileName, SVMFileName: String): TSVMHandle;
    procedure WriteIntoSVM (SVMHandle: TSVMHandle;
      FeatureVectorBasedOnGradiant: TFeatureVectorBasedOnGradiant);
    function AskQuery (SVMHandle: TSVMHandle; FeatureVectorBasedOnGradiant: TFeatureVectorBasedOnGradiant): Extended;

    procedure LoadSVMs (ConfigFilename: String);
    procedure Free; virtual;

  end;

  TSVMClients= class (TBaseCollection)
  private
    function GetSVMClient(Index: Integer): TSVMClient;

  public
    property SVMClient [Index: Integer]: TSVMClient read GetSVMClient;
    procedure AddNewSVMClient (SVMClient: TSVMClient);

    procedure Free;

  end;
  
  TByteArray= array of Byte;

  TKohonenClient= class (TObject)
  private
    RecognitionEngineTCPConnection: TIdTCPClient;
    
  public
    constructor Create (ServerHost: string; ServerPort: Integer;
      MyIP: String);
    procedure Free;

    function AskQuery (Query: TByteArray): Integer;

  end;
implementation

uses
  ExceptionUnit, Dialogs;

{ TSVMClient }

function TSVMClient.AskQuery (SVMHandle: TSVMHandle;
  FeatureVectorBasedOnGradiant: TFeatureVectorBasedOnGradiant): Extended;
var
  i: Integer;

begin
  RecognitionEngineTCPConnection.WriteLn ('ExecQuery');
  RecognitionEngineTCPConnection.WriteLn (IntToStr (SVMHandle));
  RecognitionEngineTCPConnection.WriteLn (IntToStr (FeatureVectorBasedOnGradiant.Size));
  
  for i:= 0 to FeatureVectorBasedOnGradiant.Size- 1 do
    RecognitionEngineTCPConnection.WriteLn (
     FloatToStr(FeatureVectorBasedOnGradiant.Member [i]));

  Result:= StrToFloat (RecognitionEngineTCPConnection.ReadLn)
  
end;

constructor TSVMClient.Create (ServerHost: String; ServerPort: Integer);
begin
  RecognitionEngineTCPConnection:= TIdTCPClient.Create (nil);
  RecognitionEngineTCPConnection.Host:= ServerHost;
  RecognitionEngineTCPConnection.Port:= ServerPort;
  
  try
    RecognitionEngineTCPConnection.Connect;

    RecognitionEngineTCPConnection.WriteLn ('SRRF');
    RecognitionEngineTCPConnection.WriteLn ('OCR');

  except

  end;

end;

procedure TSVMClient.Free;
begin
  if RecognitionEngineTCPConnection.Connected then
    RecognitionEngineTCPConnection.Disconnect;
    
  RecognitionEngineTCPConnection.Free;

  SetLength (LoadedSVMs, 0);
  
end;

function TSVMClient.GetSVMHandle (SVMName, Kernel: String): TSVMHandle;
begin
  raise ENotImplemented.Create ('GetSVMHandle');
  
end;

procedure TSVMClient.LoadSVMs (ConfigFilename: String);
var
  InputFile: TextFile;
  KernelFileName, SVMFileName: String;

begin
  AssignFile (InputFile, ConfigFilename);
  Reset (InputFile);
  while not Eof (InputFile) do
  begin
    ReadLn (InputFile, KernelFileName);
    ReadLn (InputFile, SVMFileName);
    OpenSVM (KernelFileName, SVMFileName);

  end;

  CloseFile (InputFile);

end;

function TSVMClient.OpenSVM (KernelFileName,
  SVMFileName: String): TSVMHandle;
begin
  RecognitionEngineTCPConnection.WriteLn ('LoadSVM');
  RecognitionEngineTCPConnection.WriteLn (KernelFileName);
  RecognitionEngineTCPConnection.WriteLn (SVMFileName);

  Result:= RecognitionEngineTCPConnection.ReadInteger;
  
  if Result= -1 then
    raise ESVMNotFound.Create (KernelFileName, SVMFileName);

end;

procedure TSVMClient.WriteIntoSVM (SVMHandle: TSVMHandle;
  FeatureVectorBasedOnGradiant: TFeatureVectorBasedOnGradiant);
var
  i: Integer;
//  DoublePtr: PDouble;
  
begin

  RecognitionEngineTCPConnection.WriteLn (IntToStr (SVMHandle));
  for i:= 0 to FeatureVectorBasedOnGradiant.Size- 1 do
    RecognitionEngineTCPConnection.WriteLn (
     FloatToStr(FeatureVectorBasedOnGradiant.Member [i]));
     
end;

{ ESVMNotFound }

constructor ESVMNotFound.Create(KernelFileName, SvmFileName: String);
begin
  inherited Create ('One or both of '+
    KernelFileName+ ' '+ SvmFileName+ ' not Found!');

end;

{ THopfieldClient }

function TKohonenClient.AskQuery (Query: TByteArray): Integer;
var
  SourcePtr, TargetPtr: PByte;
  Source, Target: Integer;
  i: Integer;
  Last18Ch, Ch: char;
  Last18Str: String;
                    
begin
  RecognitionEngineTCPConnection.WriteInteger ($0A000000);
  RecognitionEngineTCPConnection.WriteInteger ($04000000);
  RecognitionEngineTCPConnection.WriteInteger (0);

  Source:= Length (Query);
  Target:= 0;
  TargetPtr:= @Target;
  Inc (TargetPtr, 3);
  SourcePtr:= @Source;

  for i:= 1 to 4 do
  begin
    TargetPtr^:= SourcePtr^;
    Inc (SourcePtr);
    Dec (TargetPtr);

  end;
  RecognitionEngineTCPConnection.WriteInteger (Target);

  RecognitionEngineTCPConnection.WriteBuffer (Query [0], Length (Query), True);
  Last18Ch:= #0;
  Ch:= RecognitionEngineTCPConnection.ReadChar;
  Last18Str:= '';
  i:= 0;
  
  while Last18Str<> #0'F'#0'i'#0'n'#0'i'#0's'#0'h'#0'e'#0'd'#0'!' do
  begin
    Last18Str:= Last18Str+ Ch;
    if 18< i then
    begin
      Last18Ch:= Last18Str [1];
      Delete (Last18Str, 1, 1);

    end;

    if Copy (Last18Str, 1, Length (#0'F'#0'i'#0'n'#0'i'#0's'#0'h'#0'e'#0'd'#0'!'))=
       #0'F'#0'i'#0'n'#0'i'#0's'#0'h'#0'e'#0'd'#0'!' then
      Break;

    Ch:= RecognitionEngineTCPConnection.ReadChar;
    Inc (i);

  end;

  Result:= Integer (Last18Ch);

end;

constructor TKohonenClient.Create(ServerHost: string;
  ServerPort: Integer; MyIP: String);
var
  RevLen,
  Len: Integer;
  Source, Dest: PByte;
  i,
  IntTemp: Integer;

begin
  RecognitionEngineTCPConnection:= TIdTCPClient.Create (nil);
  RecognitionEngineTCPConnection.Host:= ServerHost;
  RecognitionEngineTCPConnection.Port:= ServerPort;

  try
    RecognitionEngineTCPConnection.Connect;

    RecognitionEngineTCPConnection.WriteInteger ($0B000000);
    RecognitionEngineTCPConnection.WriteInteger ($04000000);
    RecognitionEngineTCPConnection.WriteInteger (0);

    Len:= 2* Length (MyIP);
    RevLen:= 0;
    Source:= @Len;
    Dest:= @RevLen;
    Inc (Dest, 3);

    for i:= 0 to 3 do
    begin
      Dest^:= Source^;
      Dec (Dest);
      Inc (Source);

    end;

    RecognitionEngineTCPConnection.WriteInteger (RevLen);
    Len:= Len div 2;
    for i:= 1 to Len do
    begin
      IntTemp:= Ord (MyIp [i])* 256;
      IntTemp:= IntTemp mod 65536;
      RecognitionEngineTCPConnection.WriteSmallInt (IntTemp);
      
    end;

{    RecognitionEngineTCPConnection.WriteLn ('SRRF');
    RecognitionEngineTCPConnection.WriteLn ('LPR');
}
  except

  end;


end;

procedure TKohonenClient.Free;
begin
  RecognitionEngineTCPConnection.Disconnect;
  RecognitionEngineTCPConnection.Free;
  
  inherited;

end;

{ TSVMClients }

procedure TSVMClients.AddNewSVMClient (SVMClient: TSVMClient);
begin
  inherited Add (SVMClient);
  
end;

procedure TSVMClients.Free;
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    TSVMClient (Ptr^).Free;
    Inc (Ptr);

  end;

  inherited Free (False);
  
end;

function TSVMClients.GetSVMClient (Index: Integer): TSVMClient;
begin
  Result:= Member [Index] as TSVMClient;
  
end;

end.
