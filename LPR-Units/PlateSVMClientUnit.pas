unit PlateSVMClientUnit;

interface
uses
  SvmClientUnit, FeatureUnit, CollectionUnit, FMLImage;

const
  NumberOfClassInPlate= 9+ 18;

type
  TAnswer= record
    Point: Extended;
    Text: String;

  end;

  TPlateSVMClient= class (TSVMClient)
  private
    IthSVMHandles: array [0..NumberOfClassInPlate] of Integer;

  public
    constructor Create (ConfigFile: String);
    procedure Free;

    function QueryOnSign (FeatureVector: TFeatureVectorBasedOnGradiant):
                 TDoubleCollection;

  
  public
    function FindPattern (FeatureVectorBasedOnGradiant:
      TFeatureVectorBasedOnGradiant): TAnswer;

  end;

  TPlateKohonenClient= class (TKohonenClient)
  private
  public
    constructor Create (ConfigFile: String);
    procedure Free;

    function AskQuery (FMLImage: TFMLImage; Number: Boolean): Integer;
    
  end;
  
implementation
uses
  SysUtils, ExceptionUnit, Classes;


{ TPlateSVMClient }

constructor TPlateSVMClient.Create (ConfigFile: String);
var
  InputFile: TextFile;
  ServerHost, SVMFile, KernelFile,
  TempString: String;
  ServerPort: Integer;
  i: Integer;

begin
  AssignFile (InputFile, ConfigFile);
  Reset (InputFile);

  Readln (InputFile, TempString);
  Readln (InputFile, ServerHost);
  Readln (InputFile, TempString);
  Readln (InputFile, ServerPort);
  Readln (InputFile, TempString);

  inherited Create (ServerHost, ServerPort);

  for i:= 1 to NumberOfClassInPlate do
  begin
    Readln (InputFile, TempString);

    SVMFile:= TempString;
    Readln (InputFile, TempString);
    KernelFile:= TempString;

    IthSVMHandles [i]:= OpenSVM (KernelFile, SVMFile);

  end;

  CloseFile (InputFile);
  
end;

function TPlateSVMClient.FindPattern(
  FeatureVectorBasedOnGradiant: TFeatureVectorBasedOnGradiant): TAnswer;

begin
  raise ENotImplemented.Create ('');
   
end;

procedure TPlateSVMClient.Free;
begin
  inherited Free;
  
end;

function TPlateSVMClient.QueryOnSign (
  FeatureVector: TFeatureVectorBasedOnGradiant): TDoubleCollection;
var
  i: Integer;

begin
  Result:= TDoubleCollection.Create;

  for i:= 0 to NumberOfClassInPlate do
    Result.Add (Self.AskQuery (IthSVMHandles [i],
      FeatureVector));
      
end;

{ TPlateKohonenClient }

function TPlateKohonenClient.AskQuery (FMLImage: TFMLImage; Number: Boolean): Integer;
var
  QueryArray: SVMClientUnit.TByteArray;
  IntPtr: PInteger;
  BytePtr: PByte;
  r, c: Integer;

begin
  if FMLImage.ImageType<> itMonoChrome then
    raise EInvalidImage.Create ('A MonoChrome Image is needed!');

  SetLength (QueryArray, 1+ 1+ 1+ FMLImage.Row* FMLImage.Column);
  BytePtr:= @QueryArray [0];

  if Number then
    BytePtr^:= 1
  else
    BytePtr^:= 0;
    
  Inc (BytePtr);
  BytePtr^:= FMLImage.Row;
  Inc (BytePtr);
  BytePtr^:= FMLImage.Column;
  Inc (BytePtr);

  for r:= 0 to FMLImage.Row- 1 do
  begin
    IntPtr:= FMLImage.ScanLine [r];
    
    for c:= 0 to FMLImage.Column- 1 do
    begin
      BytePtr^:= IntPtr^;
      Inc (BytePtr);
      Inc (IntPtr);

    end;

  end;

  Result:= inherited AskQuery (QueryArray);
  
end;

constructor TPlateKohonenClient.Create (ConfigFile: String);
var
  InputFile: TextFile;
  ServerHost, SVMFile, KernelFile,
  MyIP,
  TempString: String;
  ServerPort: Integer;
  i: Integer;

begin
  AssignFile (InputFile, ConfigFile);
  Reset (InputFile);

  Readln (InputFile, TempString);
  Readln (InputFile, ServerHost);
  Readln (InputFile, TempString);
  Readln (InputFile, ServerPort);
  Readln (InputFile, TempString);
  Readln (InputFile, MyIP);

  inherited Create (ServerHost, ServerPort, MyIP);

  CloseFile (InputFile);
  
end;

procedure TPlateKohonenClient.Free;
begin
  inherited;
  
end;

end.
