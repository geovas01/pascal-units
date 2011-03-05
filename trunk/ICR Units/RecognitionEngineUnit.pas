unit RecognitionEngineUnit;

interface
uses
  SysUtils, SVMClientUnit, FMLImage, FeatureUnit;
 
type
  ENotSVMClientForPersianDigits= class (Exception)
  public
    constructor Create;
    
  end;

  TRecognitionEngine= class (TObject)
  private
    FSVMClients: TSVMClients;
    (*This string is filled up with ImageKind(s). i.e., LD*)
    (*It is not complete and should be updated to an array of TCellInfoKind*)
    FMappingString: String;

    procedure RemoveSmallComponents (AnImage: TFMLImage);
    
    function FindClassForPersianDigit (AnImage: TFMLImage): String;
    function FindClassForPersianLetter (AnImage: TFMLImage): String;
    function FindClassForPersianNumber (AnImage: TFMLImage): String;//i.e., A series of digits in an image.
    function FindClassForPersianWord (AnImage: TFMLImage): String;//Not implemented yet!!

  public
    constructor Create (SVMClients: TSVMClients; MappingString: String);
    destructor Destroy; override;

    function Classify (AnImage: TFMLImage; ImageKind: String): String;
    
  end;

implementation

uses GeometryUnit, ComponentsUnit, MyTypes;

{ TRecognitionEngine }

function TRecognitionEngine.Classify (AnImage: TFMLImage; ImageKind: String):
         String;
begin

  if ImageKind= 'PL' then
    Result:= FindClassForPersianLetter (AnImage)
  else if ImageKind= 'PD' then
    Result:= FindClassForPersianDigit (AnImage)
  else if ImageKind= 'PN' then
    Result:= FindClassForPersianNumber (AnImage)
  else if ImageKind= 'PW' then
    Result:= ''//FindClassForPersianWord (AnImage)
  else
    raise EInvalidUsage.Create ('There is no imagetype= '+ ImageKind);

end;

constructor TRecognitionEngine.Create (SVMClients:  TSVMClients;
         MappingString: String);
begin
  inherited Create;

  FSVMClients:= SVMClients;
  FMappingString:= MappingString;

end;

destructor TRecognitionEngine.Destroy;
begin

  inherited;
end;

function TRecognitionEngine.FindClassForPersianDigit (
  AnImage: TFMLImage): String;
const
  PersianDigitImageKind: Char= 'D';
  PersianDigitNewSize: Integer= 80;
  PersianDigitSmoothDegree: Integer= 2;
(*$J+*)
  SVMClient: TSVMClient= nil;
  Index: Integer= 0;
(*$J-*)

var
  i: Integer;
  F: TFeatureVectorBasedOnGradiant;
  
begin
  if SVMClient= nil then
  begin
    for i:= 1 to Length (FMappingString) do
      if FMappingString [i]= PersianDigitImageKind then
      begin
        SVMClient:= FSVMClients.SVMClient [i- 1];
        Break;
        
      end;
      
    if SVMClient= nil then
      raise ENotSVMClientForPersianDigits.Create;
      
  end;

  RemoveSmallComponents (AnImage);
  AnImage.Crop;
//  AnImage.ThickTheImage;

  F:= AnImage.ExtractFeatures (PersianDigitNewSize, PersianDigitSmoothDegree);
  Result:= SVMClient.FindClass (F);
  F.Free;
  
end;

function TRecognitionEngine.FindClassForPersianLetter (
  AnImage: TFMLImage): String;
const
  PersianLetterImageKind: Char= 'L';
  PersianLetterNewSize: Integer= 80;
  PersianLetterSmoothDegree: Integer= 2;
(*$J+*)
  SVMClient: TSVMClient= nil;
(*$J+*)

var
  i: Integer;
  F: TFeatureVectorBasedOnGradiant;
  
begin
  if SVMClient= nil then
  begin
    for i:= 1 to Length (FMappingString) do
      if FMappingString [i]= PersianLetterImageKind then
      begin
        SVMClient:= FSVMClients.SVMClient [i- 1];
        Break;
        
      end;
      
    if SVMClient= nil then
      raise ENotSVMClientForPersianDigits.Create;
      
  end;

  RemoveSmallComponents (AnImage);
  AnImage.Crop;
  
  F:= AnImage.ExtractFeatures (PersianLetterNewSize, PersianLetterSmoothDegree);
  Result:= SVMClient.FindClass (F);
  F.Free;
  
end;

function TRecognitionEngine.FindClassForPersianNumber (
  AnImage: TFMLImage): String;
(*$J+*)
const
  TL: TPoint= nil;
  BR: TPoint= nil;
(*$J-*)
  MinHeight: Integer= 5;//1mm
  MinWidth: Integer= 0;
  MaxHeight: Integer= 1200;//10cm
  MaxWidth: Integer= 1200;//10cm

var
  i: Integer;
  ActiveImage, ComImage: TFMLImage;
  ActiveComponent: TComponent;
  CompCol: TComponentCollection;
  F: TFeatureVectorBasedOnGradiant;
  Ptr: PObject;

begin
  if TL= nil then
  begin
    TL:= TPoint.Create (0, 0);
    BR:= TPoint.Create (0, 0);

  end;
  BR.r:= AnImage.Row- 1;
  BR.c:= AnImage.Column- 1;

  Result:= '';
  CompCol:= AnImage.FindAllComponentsInBox (TL, BR);
  Ptr:= CompCol.GetPointerToFirst;

  for i:= 1 to CompCol.Size do
  begin
    ActiveComponent:= Ptr^ as TComponent;
    Inc (Ptr);
    
    if (MinHeight< ActiveComponent.Height) and
       (MinWidth< ActiveComponent.Width) and
       (ActiveComponent.Height< MaxHeight) and
       (ActiveComponent.Width< MaxWidth) then
    begin
      ComImage:= TFMLImage.Create (ActiveComponent);
      Result:= Result+ FindClassForPersianDigit (ComImage);

      ComImage.Free;
      
    end;

  end;

  CompCol.Free;
  
end;

function TRecognitionEngine.FindClassForPersianWord(
  AnImage: TFMLImage): String;
const
  PersianWordImageKind: Char= 'W';
  PersianWordNewSize: Integer= 80;
  PersianWordSmoothDegree: Integer= 2;
(*$J+*)
  SVMClient: TSVMClient= nil;
(*$J+*)

var
  i: Integer;
  F: TFeatureVectorBasedOnGradiant;
  
begin
  if SVMClient= nil then
  begin
    for i:= 1 to Length (FMappingString) do
      if FMappingString [i]= PersianWordImageKind then
      begin
        SVMClient:= FSVMClients.SVMClient [i- 1];
        Break;

      end;

    if SVMClient= nil then
      raise ENotSVMClientForPersianDigits.Create;

  end;

  F:= AnImage.ExtractFeatures (PersianWordNewSize, PersianWordNewSize);
  Result:= SVMClient.FindClass (F);
  F.Free;

end;

procedure TRecognitionEngine.RemoveSmallComponents (AnImage: TFMLImage);
(*$J+*)
const
  TL: TPoint= nil;
  BR: TPoint= nil;
(*$J-*)
  Threshold: Integer= 10;

var
  CompCol: TComponentCollection;
  ActiveCom: TComponent;
  Ptr, PointPtr: PObject;
  i, j: Integer;
  White: Integer;
  
begin
  if TL= nil then
  begin
    TL:= TPoint.Create (0, 0);
    BR:= TPoint.Create (0, 0);
    
  end;
  BR.r:= AnImage.Row- 1;
  BR.c:= AnImage.Column- 1;
  White:= AnImage.GetWhiteColor;
  
  CompCol:= AnImage.FindAllComponentsInBox (TL, BR);
  Ptr:= CompCol.GetPointerToFirst;
  
  for i:= 1 to CompCol.Size do
  begin
    ActiveCom:= Ptr^ as TComponent;
    Inc (Ptr);
    
    if ActiveCom.Size< Threshold then
    begin
      PointPtr:= ActiveCom.GetPointerToFirst;
      
      for j:= 1 to ActiveCom.Size do
      begin
        AnImage.SetPixelColor ((PointPtr^ as TMyPixel).Location, WHITE);
        Inc (PointPtr);
        
      end;

    end;

  end;
  
  CompCol.Free;
  
end;

{ ENotSVMClientForPersianDigits }

constructor ENotSVMClientForPersianDigits.Create;
begin
  inherited Create ('No svmclient for persian digits is supplied!');
  
end;

end.
