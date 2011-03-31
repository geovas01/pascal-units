unit FMLImage;
 // (*$Define General_Debug*)
// (*$DEFINE REVERSE_MODE*)
//(*$DEFINE DEBUG_MODE*)

interface
uses
  Windows, Classes, SysUtils, GeometryUnit,
  FeatureUnit, CollectionUnit, ComponentsUnit,
  ICLFeatureUnit, Graphics, MyTypes;
    
const
  UnknownPattern: Integer= 9999;
  UnImportantPattern: Integer= 9998;

type
  EFileNotFound= class (Exception);
  EInvalidImage= class (Exception);


  TByteFile= file of Byte;
  TInputKind= (ikNumeral, ikAlphabet, ikCheckBox, ikPicture, ikHelpBar);
  TMyBoolean= (mbTrue, mbFalse, mbUnSet);

  TFMLImage= class;
  TImageCollection= class;

  TImageType= (itMonoChrome, it8bit, it24bit, it32Bit, itNone);
  TBlackPixelCountInRow= Integer;
  TBlackPixelCountInColumn= Integer;
  TBlackPixelCountInRows= array of TBlackPixelCountInRow;
  TBlackPixelCountInColumns= array of TBlackPixelCountInColumn;

  TComponentCollection= class (TBaseCollection)
  private

    function GetComponent (Index: Integer): TComponent;
    function GetMaxPoint: TPoint;
    function GetMinPoint: TPoint;

    procedure FindMaxPoint;
    procedure FindMinPoint;
    procedure SetComponent (Index: Integer; const Value: TComponent);
  protected
    MinR, MinC, MaxR, MaxC: Integer;
    procedure AddComponent(Component: TComponent);

  public
    property Component [Index: Integer]: TComponent read GetComponent
      write SetComponent;
    property MinPoint: TPoint read GetMinPoint;
    property MaxPoint: TPoint read GetMaxPoint;

    constructor Create;
    destructor Destroy; override;

    function IsExists (r, c: Integer): Boolean;
    procedure RemoveInvalidComponents;
    procedure Delete (Index: Integer); override;

    procedure Add (NewComponent: TComponent);  
    procedure Clear; override;  

  end;

  TVector = class (TObject)
  private
    FLength: Extended;
    FTeta: Extended;
    Fx, Fy: Extended;
    function Getx: Extended;
    function Gety: Extended;
    
  public
    property Len: Extended read FLength;
    property Teta: Extended read FTeta;
    property x: Extended read Getx;
    property y: Extended read Gety;

    constructor Create; overload;
    constructor Create (r, Teta: Extended); overload;//Teta is in Degree;
    constructor CreateXY (x, y: Extended); overload;//Teta is in Degree;

    function Add (AnotherVector: TVector): TVector;//Create a new Vector
    function Multiply (ASize: Extended): TVector;//Create a new Vector

  end;

  TArrayofArrayofInt= array of array of Integer;
  TColoredImage= class;
  
  TBaseImage= class (TObject)
  private
    function GetPattern: Integer;
    function GetBodyColor (r, c: Integer): Integer;
    function GetScanLine (RowIndex: Integer): PInteger;

    procedure SetPattern (const Value: Integer);
    procedure SetRow (const Value: Integer);
    procedure SetColumn (const Value: Integer);
    procedure SetImageKind(const Value: TInputKind);

    function DoSmooth: TColoredImage;

  protected
    BLACK, WHITE: Integer;
    FBody: TArrayofArrayofInt;
    FRow, FColumn, FPattern: Integer;
    FImageKind: TInputKind;
    FIsBlank: TMyBoolean;
    HistogramIsCalced: Boolean;
    FImageType: TImageType;
    FBlackPixelCountInRows: TBlackPixelCountInRows;
    FBlackPixelCountInColumns: TBlackPixelCountInColumns;
    BlackPixCount: Integer;
    FCenterOfMass: TPoint;

    function NewInstance: TBaseImage; virtual; abstract;

  public
    function GetBlackColor: Integer; virtual; abstract;
    function GetWhiteColor: Integer; virtual; abstract;

    property ImageKind: TInputKind read FImageKind;
    (*ImageType determines how the image is stored in the FBody array*)
    property ImageType: TImageType read FImageType;
    property Pattern: Integer read GetPattern write SetPattern;
    property Row: Integer read FRow write SetRow;
    property Column: Integer read FColumn write SetColumn;
    property Kind: TInputKind read FImageKind write SetImageKind;
    property IsBlank: TMyBoolean read FIsBlank write FIsBlank;
    (*returns the value of pixel located in r and c*)
    property Body [r, c: Integer]: Integer read GetBodyColor;
    (*returns a pointer to the first integer in the rowindex'th row*)
    property ScanLine [RowIndex: Integer]: PInteger read GetScanLine;

    constructor Create (ImageType: TImageType= itMonoChrome);overload;
    destructor Destroy; override;

    procedure SaveAsBitmap (FileName: string);
    procedure LoadBitMap (FileName: String); overload;
    procedure LoadBitMap (Bitmap: TBitmap; Pattern: Integer= 9999);overload; virtual; abstract;
    function GetAsBitmap: TBitmap; virtual; abstract;

    (*Note than, FCenterofMass holds the sum of x and y of black pixels,
    not the centerofmass. Use this function to get COM.
    The ReCalc can be used to force the function to recalculates the
    COM*)
    function GetCenterOfMass (Recalc: Boolean= False):  TPoint;
    (*Returns the FBody array which stored the pixels of body.
      It can be used for fast work with pixels*)
    function GetBodyArray: TArrayofArrayofInt;

    (*Returns the ration of the black pixel in the column ColIndex by the BotRow- TopRow+ 1*)
    function IsHorizentalLineBlack (RowIndex: Integer; LeftCol, RightCol: Integer;
      Heigth: Integer= 1): Real;
    function IsVerticalLineBlack (ColIndex: Integer; TopRow, BotRow: Integer;
      Width: Integer= 1): Real;

    (*
      Copies a rectange from the image.
    *)
    function CopyPixels (TL, BR: TPoint): TBaseImage; virtual;

    (*Saves the image as text file.
      If the PrintBodyValue is false then it prints out the characters W and B.
      Otherwise, it prints the value of FBody.
      !!Note that PrintBodyValue= False  only works for images whose imagetype is itMonochrome*)
    procedure SaveAsText (FileName: String; PrintBodyValue: Boolean= False);

    (*Smoothes the image*)
    function Smooth (RepeatCount: Integer): TColoredImage;
    function ApplySobelAndGetGradiantIn8Dir: T8DirGradiantFeature;
    function GetSampleGradiantFor8Dir: TSampleGradiantIn8Dir;

    (*Returns the number of black pixel in the image. Current implementation
    doesn't look at Recalc and always re-calculate the value*)
    function BlackPixCountInImage (ReCalc: Boolean= False): Integer;
    
  end;

  TFMLImage= class (TBaseImage)
  private

    function RemoveColors(AColor: Integer): TFMLImage;

    procedure LoadPhotoCopiedImage (Bitmap: TBitmap); overload;

    function DeleteImage (NoiseColor: TColor; NoiseThr: Integer): TFMLImage;
    function FindAllComponents: TComponentCollection;
    function DoSmooth1: TFMLImage;
    function DoSmooth2: TFMLImage;

  protected
    function NewInstance: TBaseImage; override;
    
  public
    function GetBlackColor: Integer; override;
    function GetWhiteColor: Integer; override;

    (*Create an image whose black pixel are stored in PixelCollection.
    Note!! this constructor crops the image*)
    constructor Create (PixelCollection: TComponent);overload;
    (*Create an image whose black pixel are stored in ComponentCollection (which is a collection of PixelCollection)*)
    constructor Create (ComponentCollection: TComponentCollection);overload;
    (*Create an image whose black pixel are stored in a BlackPoints (which is a collection of TPoint)*)
    constructor Create (BlackPoints: TPointCollection);overload;

    (*Saves the image in the file whose handle in OutputFile by FML format*)
    procedure SaveInFMLFile (var OutputFile: TByteFile); overload;
    (*Saves the image in the stream whose handle in OutputStream by FML format*)
    procedure SaveInFMLStream (OutputStream: TFileStream); overload;
    (*Saves the image in the filename by FML Format*)
    procedure SaveInFMLFile (Filename: String); overload;

    (*Load an image which is filtered by photoshop "Photocopy" fileter.
      The pixels of transformation of an image under this filter will be
        0 or 255.*)
    procedure LoadPhotoCopiedImage (FileName: string); overload;

    procedure LoadBitMap (Bitmap: TBitmap; Pattern: Integer= 9999);override;

    procedure LoadFromFMLFile (var InputFile: TByteFile); overload;
    procedure LoadFromFMLStream (InputStream: TFileStream); 

    procedure LoadFromBMLFile (var InputFile: TByteFile); overload;
    procedure LoadFromBMLStream (InputStream: TFileStream);

    function GetAsBitmap: TBitmap; override;

    (*Adds the pixels in ImaePixels Component to the image (Self) and
      returns the self*)    
    function MixImage (ImagePixels: TComponent): TFMLImage;
    (*Sets the pixel in location r and c to black
      NOTE: This procedures do not perform range checking*)
    procedure SetPixelColor (r, c: Integer; NewColor: Integer); overload;
    procedure SetPixelColor (Point: TPoint; NewColor: Integer); overload;

    (*Clear the r'th row of the image
      NOTE: This procedure works when the ImageType is itMonochrome*)
    procedure ClearLine (r: Integer);
    (*Clear the c'th column of the image
      NOTE: This procedure works when the ImageType is itMonochrome*)
    procedure ClearColumn (c: Integer);

    (*Counts the black pixel in each row of image and returns them in an array.
    NOTE:: It works for Monochrome and 8 bit images. For 8 bit images, it calculates
    a threshold and ...*)
    function BlackPixelCountInRows: TBlackPixelCountInRows;
    function BlackPixelCountInColumns: TBlackPixelCountInColumns;

    (*
      This function finds all connected components () in the image using a BFS.
      TopLeft and BotRight indicate where the search area is and the boolean
      UseDialateBefExt indicates that if the image should be dialated or not.
    *)
    function FindAllComponentsInBox (TopLeft, BottomRight: TPoint): TComponentCollection;
    (*
      NOTE:: This procedure is not implemented Yet!!
    *)
    function FindAllComponentsHavePointInBox (TopLeft, BottomRight: TPoint): TComponentCollection;

    (*Dialtes the image and returns Self*)
    function Dilate (Mask: TArrayofArrayofInt): TFMLImage;
    (*Erodes the image and returns Self*)
    function Erode (Mask: TArrayofArrayofInt): TFMLImage;
    (*Apply Opening operator on the image and returns Self*)
    function Opening (Mask: TArrayofArrayofInt): TFMLImage;
    (*Apply Opening operator on the image and returns Self*)
    function Closing (Mask: TArrayofArrayofInt): TFMLImage;
    (*Apply Opening operator on the image and returns Self*)
    function HitAndMiss (Mask: TArrayofArrayofInt): TFMLImage;

    (*Thicks the image and returns Self*)
    function ThickTheImage: TFMLImage;
    (*Thins the image and returns Self*)
    function ThinTheImage: TFMLImage;
    (*returns an estimate for the pen width*)
    function ImageThickness: Integer;
    
    (*Crops the image and returns self*)
    function Crop: TFMLImage;

    (*
      NOTE:: This procedure is not implemented Yet!!
    *)
    procedure Write (PrintToFile: Boolean= False; FileName: String= '');

    (*
      NOTE:: This procedure is not implemented Yet!!
    *)
    procedure Add (Component: TComponent);

    (*
      Copies the rows in the range TopRowIndex and BottomRowIndex, inclusivly.
    *)
    function CopyRows (TopRowIndex, BottomRowIndex: Integer): TFMLImage;
    (*
      Rotates the image, and return the rotated image. 
      NOTE:: This function only works with it8bit images.
    *)
    function Rotate (AngleInDeg: Integer): TFMLImage;

    (*Resizes the image
    NOTE:: These functions are <B>not suitable</B> when one wants to shrink the image *)
    function Resize (NewRow, NewColumn: Integer): TFMLImage;
    (*Resizes the image*)
    function NewResize (NewRow, NewColumn: Integer; SaveAspectRatio: Boolean= False): TFMLImage;

    function GetComponentByOnePoint (APoint: TPoint): TComponent;


    function ExtractFeatures (NewSize: Integer; SmoothDegree: Integer;
       NumberOfMasks: Integer= 5): TFeatureVectorBasedOnGradiant; virtual;      
    function ExtractFreemanFeature: TFreemanFeature; virtual;

    (*Deletes a row from image*)
    procedure DeleteRow (Index: Integer);
    procedure DeleteRowsInRange (TopIndex, BotIndex: Integer);
    (*NOTE:: This function does not update the center of mass*)
    procedure DeleteColumnsInRange (TopIndex, BotIndex: Integer);

    (*Deletes the rows who have more than Percentage black pixel in them*)
    procedure DeleteVerticalBlackLine (Percentage: Extended= 1/2);
    procedure DeleteHorizentalBlackLine (Percentage: Extended= 1/2);

    (*Reverse the color of an FML Image, flip the WHITE pixels to BLACK and
      vice versa, and returns Self*)
    function ReverseColor: TFMLImage;

    destructor Destroy; override;

    function Copy (TL, BR: TPoint): TFMLImage;

  end;

  TColoredImage= class (TBaseImage)
  private

    procedure CalculateHistogram;
    function GetHistogram(Color: Integer): Integer;


  protected
    FHistogram: array [0..255] of Integer;
    function NewInstance: TBaseImage; override;

  public
    (*Histogram returns the number of occurence of each color. It only works when ImageType= it8bit*)
    property Histogram [Color: Integer]: Integer read GetHistogram;

    (*This function returns a new bitmap in Monochrome style, but doesn't change
     the image itself*)
    function ConvertToGrayScale: TColoredImage;
    (*Find a threshold for convertToBinary. It works for the images whose ImageType is it8Bit, only*)
    function GrayThreshold: Integer;

    function ConvertToBinary: TFMLImage;

    procedure LoadBitmap (Bitmap: TBitmap; Pattern: Integer= 9999); override;

    (*Returns a 8 or 24 bits Bitmap image*)
    function GetAsBitmap: TBitmap; override;

    constructor Create (ImageType: TImageType);
    function Copy (TL, BR: TPoint): TColoredImage;

    function GetBlackColor: Integer; override;
    function GetWhiteColor: Integer; override;

  end;

  TImageCollection= class (TBaseCollection)
  private
    function GetImageNumber: Integer;
    function GetImage (Index: Integer): TFMLImage;

  public
    property ImageNumber: Integer read GetImageNumber;
    property Image [Index: Integer]: TFMLImage read GetImage;

    procedure LoadFromFMLFile (FileName: string);
    procedure LoadFromBMLFile (FileName: string);
    procedure SaveToFile (FileName: string);
    procedure SaveFilesAsBitmap (BaseFileName: String);
    procedure AddImage (Image: TFMLImage);
    procedure AddImageCollection (ImageCollection: TimageCollection);
    procedure Dilate (Mask: TArrayofArrayofInt);

    constructor Create; overload;
    
    function ExtractAllImagesFeatures (NewSize, SmoothDegree: Integer): TFeatureVectorBasedOnGradiantCollection;
    function GetAllWithPattern (PatternIndex: Integer): TImageCollection;
    
  end;

implementation
uses
  {Borland.Vcl.Controls, System.Xml.XPath, }
  
  Math, ExceptionUnit, TypInfo, VectorUnit, JPeg;
  
type
  EFMLImageNotInitialized= class (Exception);
  ERangeCheckError= class (Exception);

  TArrArrInt= array of array of Integer;
  TArrInt= array of Integer;

function MyArcTan (x, y: Extended): Extended;//Result is in Degree.
begin
  if abs (x)< 1e-10 then
  begin
    if y< 0 then
      Result:= 3* Pi/ 2.0
    else
      Result:= Pi/ 2.0;

  end
  else
    Result:= ArcTan2 (x, y);

end;

function TFMLImage.GetAsBitmap: TBitmap;
var
//  Temp: Byte;
  r, c: Integer;
  PixPtr: PInteger;
  RowPtr: PByte;

begin
  Result:= TBitmap.Create;

  if IsBlank= mbTrue then
  begin
    Result.Width:= 1;
    Result.Height:= 1;
    Result.Monochrome:= True;
    Exit;

  end;

  if ImageType= itMonoChrome then
  begin
    Result.PixelFormat:= pf1bit;
    Result.Monochrome:= True;
    Result.Height:= FRow;
    Result.Width:= FColumn;


    for r:= 0 to Row- 1 do
    begin
      RowPtr:= Result.ScanLine [r];
      PixPtr:= @FBody [r, 0];
      RowPtr^:= 0;
      c:= 0;

      while c< FColumn do
      begin
        RowPtr^:= (1- PixPtr^) shl (7- c mod 8)+ RowPtr^;

        Inc (PixPtr);
        Inc (c);
        if c mod 8= 0 then
        begin
          Inc (RowPtr);
          RowPtr^:= 0;

        end;

      end;

    end;

  end
  else
    raise EInvalidImage.Create ('MonoChrome Image is needed');

end;

{
procedure TFMLImage.Load (var InputHandle: &File);
var
  r, c: Integer;
  b1, b2: Byte;
begin
  Read (InputHandle, b1, b2);
  Pattern:= b1+ b2 shl 8;

  b1:= Row mod 256;
  b2:= Row div 256;
  Write (InputHandle, b1, b2);

  b1:= Column mod 256;
  b2:= Column div 256;
  Write (InputHandle, b1, b2);

  for r:= 0 to Row- 1 do
    for c:= 0 to Column do
    begin
      b1:= FBody [r][c] mod 256;
      b2:= FBody [r][c] div 256;
      Write (InputHandle, b1, b2);
    end;
  b1:= 255;
  b2:= 255;
  Write (InputHandle, b1, b2);
end;
 }

procedure TFMLImage.LoadBitMap (Bitmap: TBitmap; Pattern: Integer);
var
  r, c, i: Integer;
  PixPtr: PInteger;
  RowPtr: PByte;
  cIndex: Integer;

begin
  Row:= Bitmap.Height;
  Column:= Bitmap.Width;
  
  if FCenterOfMass<> nil then
    FCenterOfMass.Free;
  FCenterOfMass:= TPoint.Create (0, 0);

  BlackPixCount:= 0;

  if Bitmap.PixelFormat= pf1bit then
  begin
    FImageType:= itMonoChrome;

    for r:= 0 to FRow- 1 do
    begin
      RowPtr:= Bitmap.ScanLine [r];
      PixPtr:= ScanLine [r];

      cIndex:= 0;
      for c:= 0 to (FColumn- 1) div 8- 1 do
      begin
        for i:= 0 to 7 do
        begin

 (*$IFNDEF REVERSE_MODE*)
          if (RowPtr^ shr (7- i)) and 1= 1 then
          begin
            PixPtr^:= BLACK;
            FCenterOfMass.Move (r, cIndex);
            Inc (BlackPixCount);

          end
          else
            PixPtr^:= WHITE;

 (*$ELSE*)
          if (RowPtr^ shr (7- i)) and 1= 0 then
          begin
            PixPtr^:= BLACK;
            FCenterOfMass.Move (r, cIndex);
            Inc (BlackPixCount);

          end
          else
              PixPtr^:= WHITE;
 (*$ENDIF*)

          Inc (cIndex);
          Inc (PixPtr);

        end;
        Inc (RowPtr);

      end;
      
      for i:= 0 to (FColumn- 1) mod 8 do
      begin

 (*$IFNDEF REVERSE_MODE*)
        if (RowPtr^ shr (7- i)) and 1= 1 then
        begin
          PixPtr^:= BLACK;
          FCenterOfMass.Move (r, cIndex);
          Inc (BlackPixCount);

        end
        else
          PixPtr^:= WHITE;
 (*$ELSE*)
        if (RowPtr^ shr (7- i)) and 1= 0 then
        begin
          PixPtr^:= BLACK;
          FCenterOfMass.Move (r, cIndex);
          Inc (BlackPixCount);

        end
        else
            PixPtr^:= WHITE;
 (*$ENDIF*)

        Inc (cIndex);
        Inc (PixPtr);
        
      end;
      
    end;
    
  end
  else
    raise Exception.Create ('Invalid filetype!');

  FPattern:= Pattern;
  
end;

{ TImageCollection }

procedure TImageCollection.AddImage (Image: TFMLImage);
begin
  inherited Add (Image);
  
end;

constructor TImageCollection.Create;
begin
  inherited;
  
end;

procedure TImageCollection.Dilate (Mask: TArrayofArrayofInt);
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    TFMLImage (Ptr^).Dilate (Mask);
    Inc (Ptr);
    
  end;


end;


function TImageCollection.ExtractAllImagesFeatures
    (NewSize, SmoothDegree: Integer): TFeatureVectorBasedOnGradiantCollection;
var
  i: Integer;

begin
  Result:= TFeatureVectorBasedOnGradiantCollection.Create;

  for i:= 0 to ImageNumber- 1 do
    Result.Add (Image [i].ExtractFeatures (NewSize, SmoothDegree));

end;

function TImageCollection.GetImage (Index: Integer): TFMLImage;
begin
  Result:= Member [Index] as TFMLImage;
  
end;

function TImageCollection.GetImageNumber: Integer;
begin
  Result:= Size;
  
end;

procedure TImageCollection.SaveToFile (FileName: string);
var
  i: Integer;
  OutputStream: TFileStream;
  b1, b2: Byte;
  
begin
  OutputStream:= TFileStream.Create (FileName, fmCreate);
{
  AssignFile (OutputFile, FileName);
  Rewrite (OutputFile);
}
  b1:= ImageNumber and 255;
  b2:= ImageNumber shr 8;
  OutputStream.Write (b1, 1);
  OutputStream.Write (b2, 1);
  
  for i:= 0 to ImageNumber- 1 do
    Image [i].SaveInFMLStream (OutputStream);

  OutputStream.Free;

end;

procedure TFMLImage.Write (PrintToFile: Boolean; FileName: String);
{var
  OutputFile: TextFile;
  i, j: Integer;
}
begin
  raise Exception.Create ('Not Implemented Yet!');
{
  if PrintToFile then
  begin
    AssignFile (OutputFile, FileName);
    Rewrite (OutputFile);
    for i:= 0 to Row- 1 do
    begin
      for j:= 0 to Column- 1 do
        Borland.Delphi.System.Write (OutputFile, Body [i, j]);
      Borland.Delphi.System.Writeln (OutputFile);
    end;

    Close (OutputFile);
  end;
  }
end;

function TFMLImage.Dilate (Mask: TArrayofArrayofInt): TFMLImage;
var
  NewBody: TArrayofArrayofInt;
  BodyPtr: PInteger;
  r, c,
  ir, ic,
  MaskRow, MaskCol: Integer;

begin
  Result:= Self;
  
  if IsBlank= mbTrue then
    Exit;
    
  MaskRow:= High (Mask);
  MaskCol:= High (Mask [0]);

  SetLength (NewBody, Row+ MaskRow);
  for r:= 0 to Row+ MaskRow- 1 do
  begin
    SetLength (NewBody [r], Column+ MaskCol);
    FillChar (NewBody [r, 0], SizeOf (NewBody [r]), WHITE);

  end;


  for r:= 0 to Row- 1 do
  begin
    BodyPtr:= ScanLine [r];
    
    for c:= 0 to Column- 1 do
    begin
      if BodyPtr^= BLACK then
        for ir:= 0 to MaskRow- 1 do
          for ic:= 0 to MaskCol- 1 do
            if Mask [ic, ic]= BLACK then
              NewBody [r+ ir, c+ ic]:= BLACK;

      Inc (BodyPtr);

    end;

  end;

  for r:= 0 to FRow- 1 do
    Move (NewBody [r][0], FBody [r][0], SizeOf (Integer)* FColumn);

  for r:= 0 to FRow+ MaskRow- 1 do
    SetLength (NewBody [r], 0);
  SetLength (NewBody, 0);

end;

function TFMLImage.Erode (Mask: TArrayofArrayofInt): TFMLImage;
var
  BodyPtr: PInteger;
  NewBody: TArrayofArrayofInt;
  r, c,
  ir, ic,
  MaskRow, MaskCol: Integer;

begin
  MaskRow:= Length (Mask);
  MaskCol:= Length (Mask [0]);

  SetLength (NewBody, Row);
  for r:= 0 to Row- 1 do
  begin
    SetLength (NewBody [r], FColumn);
    Move (FBody [r, 0], NewBody [r, 0], SizeOf (Integer)* FColumn);

  end;

  for r:= 0 to Row- MaskRow- 1 do
  begin
    BodyPtr:= ScanLine [r];

    for c:= 0 to Column- MaskCol- 1 do
    begin
      if BodyPtr^= WHITE then
        for ir:= 0 to MaskRow- 1 do
          for ic:= 0 to MaskCol- 1 do
            if Mask [ir, ic]= BLACK then
              NewBody [r+ ir, c+ ic]:= WHITE;
        
      Inc (BodyPtr);

    end;

  end;

  for r:= 0 to Row- 1 do
    SetLength (FBody [r], 0);
  SetLength (FBody, 0);

  FBody:= NewBody;
  Result:= Self;

end;

function TFMLImage.DeleteImage (NoiseColor: TColor; NoiseThr: Integer): TFMLImage;
begin
  raise ENotImplemented.Create ('TFMLImage.DeleteImage');

end;

constructor TFMLImage.Create (PixelCollection: TComponent);
var
  r, c,
  MinR, MinC,
  i: Integer;
  MinPoint, MaxPoint: TPoint;
  Pixel: TMyPixel;
  Ptr: PInteger;
  
begin
  inherited Create;

  FImageType:= itMonoChrome;
  HistogramIsCalced:= False;             
  FRow:= -1; FColumn:= -1; FPattern:= -1;
  FIsBlank:= mbUnSet;

  MinPoint:= PixelCollection.GetMinimum;
  MaxPoint:= PixelCollection.GetMaximum;
  MinC:= MinPoint.c; MinR:= MinPoint.r;

  Row:= MaxPoint.r- MinPoint.r+ 1;
  Column:= MaxPoint.c- MinPoint.c+ 1;

  for r:= 0 to Row- 1 do
  begin
    Ptr:= @FBody [r, 0];
    
    for c:= 0 to Column- 1 do
    begin
      Ptr^:= WHITE;
      Inc (Ptr);
      
    end;

  end;

  if PixelCollection.Count= 0 then
    FIsBlank:= mbTrue
  else
  begin
    FCenterOfMass:= TPoint.Create (0, 0);
    for i:= 0 to PixelCollection.Count- 1 do
    begin
      Pixel:= PixelCollection.GetPixel (i);
      FBody [Pixel.Location.r- MinR, Pixel.Location.c- MinC]:= BLACK;
      FCenterOfMass.Move (Pixel.Location);
      FCenterOfMass.Move (-MinR, -MinC);

    end;

  end;

  FPattern:= 0;
//  Self.SaveInFMLFile ('C:\ImageFromComp.FML');//??!!
  MinPoint.Free;
  MaxPoint.Free;

end;

constructor TFMLImage.Create (ComponentCollection: TComponentCollection);
var
  PixelCollection: TComponent;
  MinC, MinR,
  i, j: Integer;
  MinPoint, MaxPoint: TPoint;
  Pixel: TMyPixel;
  
begin
  inherited Create;

  FImageType:= itMonoChrome;
  HistogramIsCalced:= False;
  FRow:= -1; FColumn:= -1; FPattern:= UnImportantPattern;

  if ComponentCollection.Size= 0 then
  begin
    FIsBlank:= mbTrue;
    Exit;
    
  end
  else
    FIsBlank:= mbFalse;

  MinPoint:= ComponentCollection.MinPoint;
  MaxPoint:= ComponentCollection.MaxPoint;
  MinC:= MinPoint.c; MinR:= MinPoint.r;

  Row:= MaxPoint.r- MinPoint.r+ 1;
  Column:= MaxPoint.c- MinPoint.c+ 1;

  FCenterOfMass:= TPoint.Create (0, 0);
  
  for i:= 0 to ComponentCollection.Size- 1 do
  begin
    PixelCollection:= ComponentCollection.Component [i];

    for j:= 0 to PixelCollection.Count- 1 do
    begin
      Pixel:= PixelCollection.GetPixel (j);
      FBody [Pixel.Location.r- MinR, Pixel.Location.c- MinC]:= BLACK;
      FCenterOfMass.Move (Pixel.Location);

    end;

  end;
  
  MinPoint.Free;
  MaxPoint.Free;

end;

procedure TImageCollection.SaveFilesAsBitmap (BaseFileName: String);
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    TFMLImage (Ptr^).SaveAsBitmap (BaseFileName+ IntToStr (i)+ '.bmp');
    Inc (Ptr);
    
  end;
    
end;

procedure TImageCollection.LoadFromFMLFile (FileName: string);
var
  i: Integer;
  b1, b2: Byte;
  ImageNo: Integer;
  NewImage: TFMLImage;
  InputStream: TFileStream;
  Ptr: PObject;
  
begin
  if not FileExists (FileName) then
    raise EFileNotFound.Create (FileName);

  InputStream:= TFileStream.Create (FileName, fmOpenRead);
  InputStream.Read (b1, 1);
  InputStream.Read (b2, 1);
  ImageNo:= b1+ 256* b2;
  Allocate (ImageNo);

  Ptr:= GetPointerToFirst;
  for i:= 1 to ImageNo do
  begin
    NewImage:= TFMLImage.Create;
    NewImage.LoadFromFMLStream (InputStream);
    Ptr^:= NewImage;
    Inc (Ptr);
    
  end;

  InputStream.Free;
  
end;

function TImageCollection.GetAllWithPattern (
  PatternIndex: Integer): TImageCollection;
var
  i: Integer;

begin
  Result:= TImageCollection.Create;

  for i:= 0 to Size- 1 do
    if Image [i].Pattern= PatternIndex then
      Result.AddImage (Image [i]);

end;


procedure TFMLImage.Add (Component: TComponent);
begin
  raise ENotImplemented.Create ('TFMLImage.Add (Component: TComponent)');
{
  if FRow< Component.MaxX then
    for i:= 0 to Component.Count- 1 do
    begin
    end;
}  
end;

procedure TFMLImage.SetPixelColor (r, c: Integer; NewColor: Integer);
begin
  FBody [r, c]:= NewColor;

end;

procedure TFMLImage.SetPixelColor (Point: TPoint; NewColor: Integer);
begin
  FBody [Point.r, Point.c]:= NewColor;
    
end;


function TFMLImage.FindAllComponentsInBox (TopLeft, BottomRight: TPoint): TComponentCollection;
(*$J+*)
const
  P1: TPoint= nil;
  P2: TPoint= nil;
(*$J-*)

var
  NewImage: TFMLImage;

begin
  if P1= nil then
  begin
    P1:= TPoint.Create;
    P2:= TPoint.Create;
    
  end;

  P1.r:= Max (0, TopLeft.r);
  P1.c:= Max (0, TopLeft.c);
  P2.r:= Min (Row- 1, BottomRight.r);
  P2.c:= Min (Column- 1, BottomRight.c);
      
  NewImage:= Self.Copy (P1, P2);

  
(*$IFDEF DEBUG_MODE*)
  NewImage.SaveInFMLFile ('C:\Temp.FML');
(*$ENDIF*)

  Result:= NewImage.FindAllComponents;
  
  NewImage.Free;
  
end;

function TFMLImage.FindAllComponents: TComponentCollection;
var
  i, j, Turn,
  MaxCompIndex,
  r, c: Integer;
  PixPtr, LeftPixPtr,
  UpPixPtr, UpperLeftPixPtr,
  UpperRightPixPtr: PInteger;
  CurRowPtr,
  CurLeftPtr, LastRowPtr, LastLeftRowPtr,
  LastRightRowPtr
  : PInteger;
  ActiveComponent: TComponent;
  IsCopied: array of Boolean;
  LastRow, CurRow: TIntegerArray;
  CompPtr: PObject;
  ToBeReplacedComponent,
  ToBeRemovedComponent: TComponent;
  
(*$J+*)
const
  Rows: array [0..1] of TIntegerArray= (nil, nil);
  PtrArray: array [0..3] of ^PInteger= (nil, nil, nil, nil);
(*$J-*)
begin

  if Length (Rows [0])< FColumn then
  begin
    SetLength (Rows [0], FColumn);
    SetLength (Rows [1], FColumn);

  end;

  Result:= TComponentCollection.Create;

  FillChar (Rows [0, 0], SizeOf (Integer)* Length (Rows [0]), 255);
  FillChar (Rows [1, 0], SizeOf (Integer)* Length (Rows [1]), 255);

  PixPtr:= @FBody [0, 0];
  ActiveComponent:= nil;

  Turn:= 0;
  CurRow:= Rows [Turn];
  c:= 0;
//c is 0 and r is also 0
  if PixPtr^= BLACK then
  begin
    ActiveComponent:= TComponent.Create (Result.Size);
    ActiveComponent.Add (0, c);

    Result.Add (ActiveComponent);
    CurRow [c]:= Result.Size- 1;

  end;
  
  for c:= 1 to FColumn- 1 do
  begin
    LeftPixPtr:= PixPtr;
    Inc (PixPtr);

    if PixPtr^= BLACK then
    begin
      if LeftPixPtr^= BLACK then
      begin
        ActiveComponent.Add (0, c);
        CurRow [c]:= CurRow [c- 1];

      end
      else
      begin
        ActiveComponent:= TComponent.Create (Result.Size);
        ActiveComponent.Add (0, c);

        Result.Add (ActiveComponent);
        CurRow [c]:= Result.Size- 1;

      end;

    end
    else
     CurRow [c]:= -1;

  end;

  LastRow:= nil;
  PtrArray [0]:= @CurLeftPtr;
  PtrArray [1]:= @LastRowPtr;
  PtrArray [2]:= @LastLeftRowPtr;
  PtrArray [3]:= @LastRightRowPtr;

  for r:= 1 to FRow- 1 do
  begin
    Turn:= Turn xor 1;
    LastRow:= CurRow;
    CurRow:= Rows [Turn];

    PixPtr:= ScanLine [r];
    UpPixPtr:= ScanLine [r- 1];

    CurRowPtr:= @CurRow [0];
    LastRowPtr:= @LastRow [0];

    if PixPtr^= BLACK then
    begin
      if UpPixPtr^= Black then
      begin
        Result.Component [LastRowPtr^].Add (r, 0);
        CurRowPtr^:= LastRowPtr^;

      end
      else
      begin
        ActiveComponent:= TComponent.Create (Result.Size);
        ActiveComponent.Add (r, 0);

        Result.Add (ActiveComponent);
        CurRowPtr^:= Result.Size- 1;

      end;

    end;

    for c:= 1 to FColumn- 2 do
    begin
//      LeftPixPtr:= PixPtr;
//      UpperLeftPixPtr:= UpPixPtr;
      Inc (PixPtr);
//      Inc (UpPixPtr);

      CurLeftPtr:= CurRowPtr;
      LastLeftRowPtr:= LastRowPtr;
      Inc (CurRowPtr);
      Inc (LastRowPtr);
      LastRightRowPtr:= LastRowPtr;
      Inc (LastRightRowPtr);

      if PixPtr^= BLACK then
      begin
        MaxCompIndex:= Max (Max (CurLeftPtr^, LastRowPtr^),
          Max (LastLeftRowPtr^, LastRightRowPtr^));

        if MaxCompIndex= -1 then// All visited adjanced are white
        begin
          ActiveComponent:= TComponent.Create (Result.Size);
          ActiveComponent.Add (r, c);

          Result.AddComponent (ActiveComponent);
          CurRowPtr^:= Result.Size- 1;

        end
        else
        begin
          Result.Component [MaxCompIndex].Add (r, c);
          CurRowPtr^:= MaxCompIndex;

          for i:= 0 to 3 do
            if (PtrArray [i]^^<> -1) and  (PtrArray [i]^^<> MaxCompIndex) then
            begin
              Result.Component [MaxCompIndex].Merge (Result.Component [PtrArray [i]^^]);
              ToBeReplacedComponent:= Result.Component [MaxCompIndex];
              ToBeRemovedComponent:= Result.Component [PtrArray [i]^^];
              if ToBeReplacedComponent.ID<> ToBeRemovedComponent.ID then
              begin
                CompPtr:= Result.GetPointerToFirst;
                for j:= 1 to Result.Size  do
                begin
                  if CompPtr^= ToBeRemovedComponent then
                    CompPtr^:= ToBeReplacedComponent;
                  Inc (CompPtr);

                end;
                ToBeRemovedComponent.Free;

              end;
              
            end;

        end;

      end
      else
        CurRowPtr^:= -1;

    end;
    
    c:= FColumn- 1;
//    LeftPixPtr:= PixPtr;
//    UpperLeftPixPtr:= UpPixPtr;
    Inc (PixPtr);
//    Inc (UpPixPtr);

    CurLeftPtr:= CurRowPtr;
    LastLeftRowPtr:= LastRowPtr;
    Inc (CurRowPtr);
    Inc (LastRowPtr);
    
    if PixPtr^= BLACK then
    begin
      MaxCompIndex:= Max (Max (CurLeftPtr^, LastRowPtr^),
        LastLeftRowPtr^);

      if MaxCompIndex= -1 then// All visited adjanced are white
      begin
        ActiveComponent:= TComponent.Create (Result.Size);
        ActiveComponent.Add (r, c);

        Result.AddComponent (ActiveComponent);
        CurRowPtr^:= Result.Size- 1;

      end
      else
      begin
        Result.Component [MaxCompIndex].Add (r, c);
        CurRowPtr^:= MaxCompIndex;

        for i:= 0 to 2 do
          if (PtrArray [i]^^<> -1) and  (PtrArray [i]^^<> MaxCompIndex) then
          begin
            Result.Component [MaxCompIndex].Merge (Result.Component [PtrArray [i]^^]);
            ToBeReplacedComponent:= Result.Component [MaxCompIndex];
            ToBeRemovedComponent:= Result.Component [PtrArray [i]^^];
            if ToBeReplacedComponent.ID<> ToBeRemovedComponent.ID then
            begin
              CompPtr:= Result.GetPointerToFirst;
              for j:= 1 to Result.Size  do
              begin
                if CompPtr^= ToBeRemovedComponent then
                  CompPtr^:= ToBeReplacedComponent;
                Inc (CompPtr);

              end;
              ToBeRemovedComponent.Free;

            end;
              
          end;

      end;

    end;

  end;

  if Length (IsCopied)< Result.Size+ 1 then
    SetLength (IsCopied, Result.Size+ 1);
  FillChar (IsCopied [0], SizeOf (IsCopied), 0);

  CompPtr:= Result.GetPointerToFirst;
  j:= 0;
  for i:= 1 to Result.Size do
  begin
    if not IsCopied [TComponent (CompPtr^).ID] then
    begin
      Result.Component [j]:= TComponent (CompPtr^);
      Inc (j);
//      WriteLn (i, ' Saved: ', Result.FComponents [i].ID);
      IsCopied [TComponent (CompPtr^).ID]:= True;

    end;
    
    Inc (CompPtr);
    
  end;

  CompPtr:= Result.GetPointerToFirst;
  Inc (CompPtr, j);
  for i:= j to Result.Size- 1 do
  begin
    CompPtr^:= nil;
    Inc (CompPtr);
    
  end;
  Result.Allocate (j);

end;

procedure TBaseImage.SaveAsText (FileName: String; PrintBodyValue: Boolean);
var
  r, c: Integer;
  OutputFile: TextFile;

begin
  AssignFile (OutputFile, FileName);
  Rewrite (OutputFile);
  
  if FImageType= itMonoChrome then
    for r:= 0 to FRow- 1 do
    begin
      for c:= 0 to FColumn- 1 do
        if FBody [r, c]= WHITE then
          if PrintBodyValue then
            System.Write (OutputFile, ' (', FBody [r, c], ')')
          else
            System.Write (OutputFile, 'W')
        else
          if PrintBodyValue then
            System.Write (OutputFile, ' (', FBody [r, c], ')')
          else
            System.Write (OutputFile, 'B');

      Writeln (OutputFile);
    
    end
    
  else
  if FImageType= it8bit then
    for r:= 0 to FRow- 1 do
    begin
      for c:= 0 to FColumn- 1 do
        System.Write (OutputFile, ' (', FBody [r, c], ')');
      Writeln (OutputFile);
    
    end
    
  else
    raise EInvalidImage.Create ('The image format should be monochrome');
    
  CloseFile (OutputFile);

end;

function TFMLImage.FindAllComponentsHavePointInBox (TopLeft, BottomRight: TPoint):
  TComponentCollection;
begin
  raise ENotImplemented.Create ('FindAllComponentsHavePointInBox (');

end;

function TFMLImage.ExtractFeatures (NewSize: Integer; SmoothDegree: Integer;
       NumberOfMasks: Integer): TFeatureVectorBasedOnGradiant;
var
  ResizedImage: TFMLImage;
  SmoothedImage: TColoredImage;
  GradiantIn8Dir: T8DirGradiantFeature;

begin
  ResizedImage:= Self.NewResize (NewSize, NewSize, True);
 (*$IFDEF DEBUG_MODE*)
  ResizedImage.SaveAsText ('C:\Resized.00.txt');
 (*$ENDIF DEBUG_MODE*)

  SmoothedImage:= ResizedImage.Smooth (SmoothDegree);
 (*$IFDEF DEBUG_MODE*)
  SmoothedImage.ConvertToBinary.SaveAsText ('C:\Smoothed.00.txt');
  SmoothedImage.SaveAsBitmap ('C:\SmoothedImage.bmp');
 (*$ENDIF DEBUG_MODE*)

  GradiantIn8Dir:= SmoothedImage.ApplySobelAndGetGradiantIn8Dir;
  Result:= GradiantIn8Dir.SampleGradiant (NumberOfMasks);

  GradiantIn8Dir.Free;
  SmoothedImage.Free;
  ResizedImage.Free;

end;

function TBaseImage.Smooth (RepeatCount: Integer): TColoredImage;
var
  TempImage: TColoredImage;
  i: Integer;

begin
  if 0< RepeatCount then
  begin
    TempImage:= Self.DoSmooth;
    
    Result:= TempImage;
    
    for i:= 2 to RepeatCount do
    begin
      (*$IFDEF DEBUG_MODE*)
        TempImage.ConvertToBinary.SaveAsText ('R'+ IntToStr (i- 1)+ '.txt');
      (*$ENDIF DEBUG_MODE*)
      TempImage:= Result.DoSmooth;
      Result.Free;
      Result:= TempImage;
    
    end;
  end
  else
    Result:= nil;

end;

function TFMLImage.Resize (NewRow, NewColumn: Integer): TFMLImage;
var
  ix, iy: Integer;
  xc, yc: Double;
  xprimc, yprimc: Double;
  m10, m00, m01: Int64;
  Alpha, Beta,
  L: Double;
  r, c: Integer;
  Dummy: Integer;
  RowPtr: PInteger;
  
begin
  m10:= 0; m00:= 0; m01:= 0;

  for r:= 0 to FRow- 1 do
  begin
    RowPtr:= Self.ScanLine [r];

    for c:= 0 to FColumn- 1 do
    begin
      dummy:= RowPtr^;
      Inc (m00, dummy);
      Inc (m10, c* dummy);
      Inc (m01, r* dummy);
        
      Inc (RowPtr);
      
    end;
      
  end;

  xc:= m10/ m00;
  yc:= m01/ m00;
  xprimc:= FColumn/ 2.0;
  yprimc:= FRow/ 2.0;
    
  L:= Math.Max (Row, Column);

  if L= FRow then
  begin
    Beta:= NewRow/ L;
    Alpha:= Beta;

  end
  else
  begin
    Alpha:= NewColumn/ L;
    Beta:= Alpha;
      
  end;

  Result:= TFMLImage.Create;
  Result.FImageType:= Self.ImageType;
  Result.Column:= NewColumn;
  Result.Row:= NewRow;

  for r:= 0 to NewRow- 1 do
  begin
    RowPtr:= Result.ScanLine [r];
      
    for c:= 0 to NewColumn- 1 do
    begin
      ix:= Round ( (c- xprimc)/ Alpha+ xc);
      iy:= Round ( (r- yprimc)/ Beta+ yc);

      if (ix< FColumn) and (0<= ix ) and (iy< FRow) and (0<= iy) then
        RowPtr^:= FBody [iy, ix]
      else
        RowPtr^:= WHITE;

      Inc (RowPtr);

    end;

  end;

end;

 (*
  This function generate a new TFMLImage.
  The value of each pixel of the new FMLImage, is equal to the average of all members of it, in the original Image

  The argument RepeatCount told how many times it should be applied.
------------------------------------------------------------------
  The DP approach is to simply set
    Result.Pixels [r, c]:=
      (Result.Pixels [r- 1, c- 1]+ Result.Pixels [r- 1, c]+...+ Result.Pixels [r+ 1, c+ 1]) div 9;
    The Exectution time of this approach is 9*Row* Column.

  In this function, I implemented the folowing approach.
    At first step, set 
    TempResult.Pixels [r, c]:=
       Sum of all pixles in rectangle from (0, 0) to (r, c).
    So,
    TempResult.Pixels [r, c]:= TempResult.Pixels [r- 1, c]+
           TempResult.Pixels [r, c- 1]- TempResult.Pixels [r- 1, c- 1]+ Source [r, c];

    At second step, calculate the real value of each pixel:
      Result.Pixels [r, c]:= TempResult.Pixels [r+ 1, c+ 1]+
           Result.Pixels [r- 1, c- 1]- Result.Pixels [r- 1, c+ 1]
           - Result.Pixels [r+ 1, c- 1];
*)

function TBaseImage.DoSmooth: TColoredImage;
const
{$J+}
  SumOfPixels: array of array of Integer= nil;
  {The SumOfPixels is an static variable}
{$J-}

var
  r, c: Integer;
  SourcePtr,//
  TargetPtr//,
//  LastPixTPtr,//Pixel [r, c- 1]
//  LastRowTPtr,//Pixel [r- 1, c]
//  LastRowCelTPtr//Pixel [r- 1, c- 1]
  : PInteger;

procedure AllocateSumOfPixels;
var
  r: Integer;
  
begin
  if SumOfPixels= nil then
    SetLength (SumOfPixels, Row+ 4);

  if Length (SumOfPixels)< Row+ 4 then
  begin
   for r:= 0 to High (SumOfPixels) do
     SetLength (SumOfPixels [r], 0);
    SetLength (SumOfPixels, 0);

    SetLength (SumOfPixels, Row+ 4);
    for r:= 0 to Row+ 3 do
      SetLength (SumOfPixels [r], Column+ 4);

  end;

  if Length (SumOfPixels [0])< Column+ 4 then
    for r:= 0 to High (SumOfPixels) do
      SetLength (SumOfPixels [r], Column+ 4);

  for r:= 0 to Row+ 3 do
    FillChar (SumOfPixels [r][0], SizeOf (SumOfPixels [r]), 0);

end;

var
  ActiveSumOfPixelsPtr,// Points to SUmofPixels [r, c]
  R_1CSumOfPixelPtr, // Points to SUmofPixels [r- 1, c]
  RC_1SumOfPixelPtr, // Points to SUmofPixels [r, c- 1]
  R_1C_1SumOfPixelPtr,// Points to SUmofPixels [r- 1, c- 1]
  R__1C__1SumOfPixelPtr, // Points to SUmofPixels [r+ 1, c+ 1]
  R_2C_2SumOfPixelPtr, // Points to SUmofPixels [r- 2, c- 2]
  R_2C__1SumOfPixelPtr,// Points to SUmofPixels [r- 2, c+ 1]
  R__1C_2SumOfPixelPtr// Points to SUmofPixels [r+ 1, c- 2]
  : PInteger;
  MainImageType: TImageType;
  
begin
  MainImageType:= Self.ImageType;
  
  if Self.ImageType= itMonoChrome then
  begin
    for r:= 0 to FRow- 1 do
    begin
      SourcePtr:= Self.ScanLine [r];
      for c:= 0 to FColumn- 1 do
      begin
        SourcePtr^:= (1- SourcePtr^)* 255;
        Inc (SourcePtr);
        
      end;

    end;
    FImageType:= it8bit;
    
  end;
(*$IFDEF DEBUG_MODE*)
  Self.SaveAsText ('C:\Image.txt', True);
(*$ENDIF*)

  AllocateSumOfPixels;
  
  Result:= TColoredImage.Create (it8bit);
  Result.FImageType:= it8bit;
  Result.FPattern:= FPattern;
  Result.Column:= Column;
  Result.Row:= Row;
  
{Step 1: Calculating SumofPixels array }
   {putting white pixel around the main image and
    SumOfPixels [r, c] holds the sum of the pixels [i,j]
    for which i<= r and j<= c.
    0<= r<= Row+1, 0<= c<= Column+1,
    }

  {Calculating SumOfPixels for 0's Row}
  ActiveSumOfPixelsPtr:= @SumOfPixels [0][0];
  ActiveSumOfPixelsPtr^:= 255;
  RC_1SumOfPixelPtr:= ActiveSumOfPixelsPtr;
  Inc (ActiveSumOfPixelsPtr);

  for c:= 1 to FColumn+ 3 do
  begin
    ActiveSumOfPixelsPtr^:=
       RC_1SumOfPixelPtr^+ 255;

    Inc (ActiveSumOfPixelsPtr);
    Inc (RC_1SumOfPixelPtr);
    
  end;

  ActiveSumOfPixelsPtr:= @SumOfPixels [1][0];
  R_1CSumOfPixelPtr:= @SumOfPixels [0][0];
  for c:= 0 to FColumn+ 3 do
  begin
    ActiveSumOfPixelsPtr^:= 2* R_1CSumOfPixelPtr^;
    Inc (ActiveSumOfPixelsPtr);
    Inc (R_1CSumOfPixelPtr);

  end;


  for r:= 2 to FRow+ 1 do
  begin
  {Calculating SumOfPixels for r's Row}
    ActiveSumOfPixelsPtr:= @SumOfPixels [r][0];
    ActiveSumOfPixelsPtr^:= (r+ 1)* 255;
    ActiveSumOfPixelsPtr:= @SumOfPixels [r][1];
    ActiveSumOfPixelsPtr^:= 2* (r+ 1)* 255;

    R_1CSumOfPixelPtr:= @SumOfPixels [r- 1][2];
    R_1C_1SumOfPixelPtr:= @SumOfPixels [r- 1][1];
    RC_1SumOfPixelPtr:= @SumOfPixels [r][1];
    ActiveSumOfPixelsPtr:= @SumOfPixels [r][2];
    SourcePtr:= ScanLine [r- 2];

    for c:= 2 to Column+ 1 do
    begin
      ActiveSumOfPixelsPtr^:= R_1CSumOfPixelPtr^+
        RC_1SumOfPixelPtr^- R_1C_1SumOfPixelPtr^+ SourcePtr^;

      Inc (ActiveSumOfPixelsPtr);
      Inc (R_1CSumOfPixelPtr);
      Inc (R_1C_1SumOfPixelPtr);
      Inc (RC_1SumOfPixelPtr);
      Inc (SourcePtr);
      
    end;
    //SumOfPixels [r][FCol+ 2]
    ActiveSumOfPixelsPtr^:= R_1CSumOfPixelPtr^+
      RC_1SumOfPixelPtr^- R_1C_1SumOfPixelPtr^+ 255;

    Inc (ActiveSumOfPixelsPtr);
    Inc (R_1CSumOfPixelPtr);
    Inc (R_1C_1SumOfPixelPtr);
    Inc (RC_1SumOfPixelPtr);
    //SumOfPixels [r][FCol+ 3]
    ActiveSumOfPixelsPtr^:= R_1CSumOfPixelPtr^+
      RC_1SumOfPixelPtr^- R_1C_1SumOfPixelPtr^+ 255;

  end;

  {Calculating SumOfPixels for FRow+2's Row}
  for r:= FRow+ 2 to FRow+ 3 do
  begin
    ActiveSumOfPixelsPtr:= @SumOfPixels [r][0];
    ActiveSumOfPixelsPtr^:= (r+ 1)* 255;
    Inc (ActiveSumOfPixelsPtr);
    ActiveSumOfPixelsPtr^:= 2* (r+ 1)* 255;

    R_1CSumOfPixelPtr:= @SumOfPixels [r- 1][2];
    R_1C_1SumOfPixelPtr:= @SumOfPixels [r- 1][1];
    RC_1SumOfPixelPtr:= @SumOfPixels [r][1];
    ActiveSumOfPixelsPtr:= @SumOfPixels [r][2];

    for c:= 2 to Column+ 1 do
    begin
      ActiveSumOfPixelsPtr^:= R_1CSumOfPixelPtr^+
        RC_1SumOfPixelPtr^- R_1C_1SumOfPixelPtr^+ 255;

      Inc (ActiveSumOfPixelsPtr);
      Inc (R_1CSumOfPixelPtr);
      Inc (R_1C_1SumOfPixelPtr);
      Inc (RC_1SumOfPixelPtr);
      
    end;
    //SumOfPixels [r][FCol+ 2]
    ActiveSumOfPixelsPtr^:= R_1CSumOfPixelPtr^+
      RC_1SumOfPixelPtr^- R_1C_1SumOfPixelPtr^+ 255;

    Inc (ActiveSumOfPixelsPtr);
    Inc (R_1CSumOfPixelPtr);
    Inc (R_1C_1SumOfPixelPtr);
    Inc (RC_1SumOfPixelPtr);
    //SumOfPixels [r][FCol+ 3]
    ActiveSumOfPixelsPtr^:= R_1CSumOfPixelPtr^+
      RC_1SumOfPixelPtr^- R_1C_1SumOfPixelPtr^+ 255;

  end;

(*$IFDEF DEBUG_MODE*)
  AssignFile (Output, 'C:\Output.txt');
  Rewrite (Output);

  for r:= 0 to FRow+ 1 do
  begin
    for c:= 0 to FColumn+ 1 do
      System.Write (SumOfPixels [r][c], ' ');
    WriteLn;

  end;
  
  CloseFile (Output);
  
(*$ENDIF*)

{Step 2: Calculating Results Value }
  for r:= 2 to FRow+ 1 do
  begin
    TargetPtr:= Result.ScanLine [r- 2];

//    Result.FBody [r- 1, 0]:= SumOfPixels [r- 1, 
    R__1C__1SumOfPixelPtr:= @SumOfPixels [r+ 1][3];
    R__1C_2SumOfPixelPtr:= @SumOfPixels [r+ 1][0];
    R_2C__1SumOfPixelPtr:= @SumOfPixels [r- 2][3];
    R_2C_2SumOfPixelPtr:= @SumOfPixels [r- 2][0];
    for c:= 2 to FColumn+ 1 do
    begin
      TargetPtr^:= (R__1C__1SumOfPixelPtr^+
      R_2C_2SumOfPixelPtr^-
      R_2C__1SumOfPixelPtr^-
      R__1C_2SumOfPixelPtr^) div 9;
     
      Inc (R__1C__1SumOfPixelPtr);
      Inc (R__1C_2SumOfPixelPtr);
      Inc (R_2C__1SumOfPixelPtr);
      Inc (R_2C_2SumOfPixelPtr);
      Inc (TargetPtr);
      
    end;

  end;

(*$IFDEF DEBUG_MODE*)
  Result.SaveAsText ('C:\SmoothedImage.txt', True);
(*$ENDIF*)

  if MainImageType= itMonoChrome then
  begin
    for r:= 0 to FRow- 1 do
    begin
      SourcePtr:= Self.ScanLine [r];
      for c:= 0 to FColumn- 1 do
      begin
        SourcePtr^:= 1- SourcePtr^ div 255;
        Inc (SourcePtr);
        
      end;

    end;
    FImageType:= itMonoChrome;
  
  end;

end;

function TFMLImage.DoSmooth2: TFMLImage;
var
  S, i, j, k,
  r, c: Integer;

begin
  Result:= TFMLImage.Create;
  Result.Column:= Column;
  Result.Row:= Row;

  for r:= 0 to Row- 1 do
    for c:= 0 to Column- 1 do
    begin
      k:= 9;
      S:= 0;
      for i:= -1 to 1 do
        for j:= -1 to 1 do
          try
            S:= S+ Body [r+ i, c+ j];
          except
            on E: ERangeCheckError do
              Dec (k);
              
          end;

      if S div 2< k then
        Result.FBody [r, c]:= WHITE
      else
        Result.FBody [r, c]:= BLACK;
        
    end;

end;

function TFMLImage.DoSmooth1: TFMLImage;
var
  r, c: Integer;
  SourcePtr,//

  TargetPtr//Pixel [r, c]
  : PInteger;
  Count, ExcepCounter,
  Sum: Integer;
  TempResult: TFMLImage;
  
begin

  Result:= TFMLImage.Create;
  TempResult:= TFMLImage.Create;
  Result.Column:= Column;
  Result.Row:= Row;
  TempResult.Row:= Row+ 2;
  TempResult.Column:= Column+ 2;
//  Self.SaveAsText ('BeforeSmooth.txt');
  
{Step 1: Calculating TempResult's Values }

  for r:= 0 to Row- 1 do
  begin
    for c:= 0 to Column- 1 do
    begin
      Sum:= Self.FBody [r, c];
      Count:= 9;
      ExcepCounter:= 0;

      try
        Inc (Sum, TempResult.Body [r- 1, c]);
        
      except
        Dec (Count, 3);
        Inc (ExcepCounter);
        
      end;
      
      try
        Inc (Sum, TempResult.Body [r, c- 1]);
        
      except
        Dec (Count, 3);
        if ExcepCounter<> 0 then
          Inc (Count);
        
      end;

      try
        Dec (Sum, TempResult.Body [r- 1, c- 1]);
      except
        on E: ERangeCheckError do;
        
      end;

      if Sum< Count div 2 then
        TempResult.FBody [r, c]:= BLACK
      else
        TempResult.FBody [r, c]:= WHITE;

    end;

  end;

  for r:= 0 to Row do
    TempResult.FBody [r, Column+ 1]:=
            TempResult.FBody [r, Column];
              
  TargetPtr:= TempResult.ScanLine [Row+ 1];
  SourcePtr:= TempResult.ScanLine [Row];
    
  for c:= 0 to Column+ 1 do
  begin
    TargetPtr^:= SourcePtr^;
      
    Inc (TargetPtr);
    Inc (SourcePtr);
      
  end;

//    TempResult.SaveAsText ('TempResult.txt', True);

{Step 2: Calculating Results Value }
  r:= 0;
  TargetPtr:= Result.ScanLine [r];

  TargetPtr^:= TempResult.FBody [r+ 2, 2];

  if TargetPtr^< 3 then
    TargetPtr^:= BLACK
  else
    TargetPtr^:= WHITE;

  Inc (TargetPtr);

  for c:= 1 to Column- 1 do
  begin
    TargetPtr^:= TempResult.FBody [r+ 2, c+ 2]
       - TempResult.FBody [r+ 2, c- 1];

    if TargetPtr^< 3 then
      TargetPtr^:= BLACK
    else
      TargetPtr^:= WHITE;

    Inc (TargetPtr);

  end;
    
  for r:= 1 to Row- 1 do
  begin
    TargetPtr:= Result.ScanLine [r];

    TargetPtr^:= TempResult.FBody [r+ 2, 2]
       - TempResult.FBody [r- 1, 2];

    if r= Row- 1 then
    begin
      if TargetPtr^< 3 then
        TargetPtr^:= BLACK
      else
        TargetPtr^:= WHITE;

    end
    else
    begin
      if TargetPtr^< 5 then
        TargetPtr^:= BLACK
      else
        TargetPtr^:= WHITE;

    end;

    Inc (TargetPtr);

    for c:= 1 to Column- 1 do
    begin
      TargetPtr^:= TempResult.FBody [r+ 2, c+ 2]+
         TempResult.FBody [r- 1, c- 1]- TempResult.FBody [r- 1, c+ 2]
         - TempResult.FBody [r+ 2, c- 1];

      if r= Row- 1 then
      begin
        if TargetPtr^< 3 then
          TargetPtr^:= BLACK
        else
          TargetPtr^:= WHITE;

      end
      else
      begin
        if TargetPtr^< 5 then
          TargetPtr^:= BLACK
        else
          TargetPtr^:= WHITE;

      end;

      Inc (TargetPtr);

    end;

  end;
    
  TempResult.Free;

end;

function TBaseImage.ApplySobelAndGetGradiantIn8Dir: T8DirGradiantFeature;
var
  GradiantsFeatures: array [0..7] of TGradiantFeature;
  FeaturePtr: array [0..7] of PExtended;
  GradiantVector: TVector;
  Teta, Step,
  NewX, NewY,
  TetaX: Extended;
  DirectionIndex: Integer;

  xValueAfterSobel, yValueAfterSobel: Extended;
  i,
  r, c: Integer;

  CurRowPtr, CurRowPtr_1, CurRowPtr__1,
  NextRowPtr, NextRowPtr_1, NextRowPtr__1,
  LastRowPtr, LastRowPtr__1, LastRowPtr_1: PInteger;

  TempOutput: TextFile;
  
begin
 (*$IFDEF DEBUG_MODE*)
  AssignFile (TempOutput, 'Temp.Txt');
  ReWrite (TempOutput);

  Self.SaveAsText ('Image.txt', True);
 (*$ENDIF*)

  Result:= T8DirGradiantFeature.Create;
  Result.Prepare (FRow, FColumn);
  for i:= 0 to 7 do
    GradiantsFeatures [i]:= Result.GradiantFeature [i];

//  TargetPtr:= Result.ScanLine [0];
  for r:= 1 to Row- 2 do
  begin
    for i:= 0 to 7 do
      FeaturePtr [i]:= GradiantsFeatures [i].ScanLine [r];

    NextRowPtr:= ScanLine [r+ 1];
    NextRowPtr_1:= NextRowPtr;
    NextRowPtr__1:= NextRowPtr_1;
    Inc (NextRowPtr);
    Inc (NextRowPtr__1, 2);

    CurRowPtr:= ScanLine [r];
    CurRowPtr_1:= CurRowPtr;
    CurRowPtr__1:= CurRowPtr_1;
    Inc (CurRowPtr__1, 2);

    LastRowPtr:= ScanLine [r- 1];
    LastRowPtr_1:= LastRowPtr;
    LastRowPtr__1:= LastRowPtr_1;
    Inc (LastRowPtr);
    Inc (LastRowPtr__1, 2);

    for c:= 1 to Column- 2 do
    begin
      for i:= 0 to 7 do
        Inc (FeaturePtr [i]);

      xValueAfterSobel:= (LastRowPtr__1^+ 2* CurRowPtr__1^+
        NextRowPtr__1^- LastRowPtr_1^- 2* CurRowPtr_1^-
        NextRowPtr_1^)/ 255;

      yValueAfterSobel:= (LastRowPtr_1^+ 2* LastRowPtr^+
        LastRowPtr__1^- NextRowPtr__1^- 2* NextRowPtr^-
        NextRowPtr_1^)/ 255;
        
 (*$IFDEF DEBUG_MODE*)
      WriteLn (TempOutput, r, ',', c, ':', xValueAfterSobel:0:3, ' ', yValueAfterSobel:0:3);
 (*$ENDIF*)

      GradiantVector:= TVector.CreateXY (xValueAfterSobel,
            yValueAfterSobel);
      Step:= c* Pi/ 4.0;
      Step:= Step/ c;

      if GradiantVector.Len<> 0 then
      begin
        Teta:= GradiantVector.Teta;

        if 0<= Teta then
        begin
          DirectionIndex:= Floor (Teta/ Step);
          TetaX:= Teta- (DirectionIndex)* Step;
          
          NewX:= GradiantVector.Len* Cos (TetaX);
          NewY:= GradiantVector.Len* Sin (TetaX);

          FeaturePtr [DirectionIndex]^:=
            NewX- NewY;
          FeaturePtr [DirectionIndex+ 1]^:=
              NewY* Sqrt (2.0);

        end
        else
        begin
          Teta:= 2* Pi+ Teta;
          DirectionIndex:= Floor (Teta/ Step);
          TetaX:= Teta- Floor (Teta/ Step)* Step;

          NewX:= GradiantVector.Len* Cos (TetaX);
          NewY:= GradiantVector.Len* Sin (TetaX);

          FeaturePtr [DirectionIndex]^:=
            NewX- NewY;
          FeaturePtr [ (DirectionIndex+ 1) mod 8]^:=
              NewY* Sqrt (2.0);
        
        end;

      end;

      Inc (NextRowPtr);
      Inc (LastRowPtr);
      Inc (NextRowPtr_1);
      Inc (NextRowPtr__1);
      Inc (CurRowPtr_1);
      Inc (CurRowPtr__1);
      Inc (LastRowPtr_1);
      Inc (LastRowPtr__1);

      GradiantVector.Free;

    end;

  end;
 (*$IFDEF DEBUG_MODE*)
  Closefile (TempOutput);

  Result.SaveToFile ('Image.Out');
 (*$ENDIF*)

end;

function TBaseImage.GetSampleGradiantFor8Dir: TSampleGradiantIn8Dir;
begin
  raise ENotImplemented.Create ('GetSampleGradiantFor8Dir');
  
end;

procedure TFMLImage.LoadFromFMLFile (var InputFile: TByteFile);
var
  r, c: Integer;
  b1, b2: Byte;
  Ptr: PInteger;

begin
  if FCenterOfMass<> nil then
    FCenterOfMass.Free;
  FCenterOfMass:= TPoint.Create (0, 0);

  BlackPixCount:= 0;

  FImageType:= itMonoChrome;
   
  Read (InputFile, b1, b2);
  FPattern:= b1+ b2 shl 8;
  Read (InputFile, b1, b2);
  Row:= b1+ b2 shl 8;
  Read (InputFile, b1, b2);
  Column:= b1+ b2 shl 8;

  for r:= 0 to FRow- 1 do
  begin
    Ptr:= ScanLine [r];

    for c:= 0 to FColumn- 1 do
    begin
      Read (InputFile, b1, b2);
      Ptr^:= b1+ b2 shl 8;

      if 1< Ptr^ then
        raise EInvalidImage.Create ('Pixel bigger than one!'+
                IntToStr (r)+ ':'+ IntToStr (c));


      if Ptr^= BLACK then
      begin
        Inc (BlackPixCount);
        FCenterOfMass.Move (r ,c);

      end;
      Inc (Ptr);

    end;

  end;

  Read (InputFile, b1, b2);
  if (b1<> 255) or (b2<> 255) then
    raise EInvalidImage.Create ('Last two byte must be 255.');

end;

procedure TFMLImage.SaveInFMLFile (var OutputFile: TByteFile);
var
  r, c: Integer;
  b1, b2: Byte;

begin
  b1:= Pattern mod 256;
  b2:=  Pattern div 256;

  System.Write (OutputFile, b1, b2);

  b1:= Row mod 256;
  b2:= Row div 256;
  System.Write (OutputFile, b1, b2);

  b1:= Column mod 256;
  b2:= Column div 256;
  System.Write (OutputFile, b1, b2);

  for r:= 0 to Row- 1 do
    for c:= 0 to Column- 1 do
    begin
      b1:= FBody [r][c] mod 256;
      b2:= FBody [r][c] div 256;
      System.Write (OutputFile, b1, b2);
      
    end;
    
  b1:= 255;
  b2:= 255;
  System.Write (OutputFile, b1, b2);

end;


procedure TFMLImage.LoadPhotoCopiedImage (FileName: string);
var
  JpegImage: TJPEGImage;
  BitmapImage: TBitmap;
  
begin
  if UpperCase (ExtractFileExt (FileName))= '.JPG' then
  begin
    JpegImage:= TJPEGImage.Create;
    JpegImage.LoadFromFile (FileName);
    BitmapImage:= TBitmap.Create;
    BitmapImage.Assign (JpegImage);
    
    LoadPhotoCopiedImage (BitmapImage);
 (*$ifdef DEBUG_MODE*)
    Self.SaveAsText ('C:\Temp.txt');
 (*$endif*)
    JpegImage.Free;
    BitmapImage.Free;

  end
  else if UpperCase (ExtractFileExt (FileName))= '.BMP' then
  begin
    BitmapImage:= TBitmap.Create;
    BitmapImage.LoadFromFile (FileName);
    LoadPhotoCopiedImage (BitmapImage);

    BitmapImage.Free;

  end
  else
    raise EInvalidImage.Create (FileName);

end;


procedure TFMLImage.LoadPhotoCopiedImage (Bitmap: TBitmap);
begin
  Row:= Bitmap.Height;
  Column:= Bitmap.Width;

  if Bitmap.PixelFormat<> pf8bit then
  begin
    raise EInvalidImage.Create ('Image must be 256 Colored');

  end
  else
  begin
    LoadBitMap (Bitmap);
    FImageType:= it8bit;
    
  end;

end;

procedure TFMLImage.DeleteHorizentalBlackLine (Percentage: Extended);
var
  Rows: array of Boolean;
  Count: Integer;
  r, c: Integer;
  CurPointer: PInteger;

begin
  SetLength (Rows, Row);
    
  for r:= 0 to Row- 1 do
  begin
    Count:= 0;
    CurPointer:= ScanLine [r];

    for c:= 0 to Column- 1 do
    begin
      Inc (Count, CurPointer^);
      Inc (CurPointer);

    end;

    if Column* Percentage< Count then
      Rows [r]:= True
    else
      Rows [r]:= False;

  end;

  for r:= Row- 1 downto 0 do
    if Rows [r] then
      DeleteRow (r);

  SetLength (Rows, 0);
  
end;

procedure TFMLImage.DeleteRow (Index: Integer);
var
  r, i, SumX, SumY: Integer;
  Ptr: PInteger;

begin
  SumX:= 0; SumY:= 0;
  Ptr:= ScanLine [Index];
  
  for i:= 0 to FColumn- 1 do
  begin

    if Ptr^= BLACK then
    begin
      Inc (SumX, i);
      Inc (SumY, Index);

    end;
    Inc (Ptr);
    
  end;

  FCenterOfMass.Move (-SumX, -SumY);

  for r:= Index+ 1 to FRow- 1 do
    FBody [r- 1]:= FBody [r];
    
  SetLength (FBody [FRow- 1], 0);
  SetLength (FBody, FRow- 1);
  Dec (FRow);

end;

procedure TFMLImage.DeleteRowsInRange (TopIndex, BotIndex: Integer);
var
  i: Integer;

begin
  if TopIndex<= BotIndex then
    for i:= TopIndex to BotIndex do
      DeleteRow (TopIndex);

end;

procedure TFMLImage. DeleteVerticalBlackLine (Percentage: Extended);
var
  Rows: array of Boolean;
  Count: Integer;
  r, c: Integer;
  CurPointer: PInteger;

begin
  SetLength (Rows, Row);
    
  for r:= 0 to Row- 1 do
  begin
    Count:= 0;
    CurPointer:= ScanLine [r];

    for c:= 0 to Column- 1 do
    begin
      Inc (Count, CurPointer^);
      Inc (CurPointer);

    end;

    if Column* Percentage< Count then
      Rows [r]:= True
    else
      Rows [r]:= False;

  end;

  for r:= Row- 1 downto 0 do
    if Rows [r] then
      FillChar (FBody [r][0],
      SizeOf (Integer)* Column, WHITE);

  SetLength (Rows, 0);
  
end;

procedure TFMLImage.ClearLine (r: Integer);
begin
  Body [r, 0];

  FillChar (FBody [r][0],
    SizeOf (Integer)* FColumn, WHITE);// can not used for BLACK
    
end;

procedure TFMLImage.ClearColumn (c: Integer);
var
  r: Integer;
  RowPtr: PInteger;
  
begin
  if ImageType= itMonoChrome then 
    for r:= 0 to Row- 1 do
    begin
      RowPtr:= ScanLine [r];
      Inc (RowPtr, c);
      RowPtr^:= WHITE;
     
    end
  else
    raise EInvalidImage.Create ('Not Monochrome!');

end;

function TFMLImage.BlackPixelCountInRows: TBlackPixelCountInRows;
var
  r, c: Integer;
  Count: Integer;
  RowPtr: PInteger;
  ResPtr: PInteger;
  BlacknessThr: Integer;

begin
  SetLength (FBlackPixelCountInRows, Row);
  FillChar (FBlackPixelCountInRows [0],
      SizeOf (Integer)* Row, 0);

  {if ImageType= it8bit then
    BlacknessThr:= (Self as TColoredImage).GrayThreshold
  else ??!!}
  if ImageType= itMonoChrome then
    BlacknessThr:= BLACK
  else
    raise EInvalidImage.Create ('');

  Result:= FBlackPixelCountInRows;
  ResPtr:= @Result [0];
  
  for r:= 0 to Row- 1 do
  begin
    Count:= 0;
    RowPtr:= ScanLine [r];
    
    for c:= 0 to Column- 1 do
    begin
      if ImageType= itMonoChrome then
        Inc (Count, RowPtr^)
      else if RowPtr^< BlacknessThr then
        Inc (Count);

      Inc (RowPtr);
      
    end;
    ResPtr^:= Count;
      
    Inc (ResPtr);
    
  end;

  Result:= FBlackPixelCountInRows;
  
end;

function TFMLImage.BlackPixelCountInColumns: TBlackPixelCountInColumns;
var
  r, c: Integer;
  RowPtr: PInteger;
  ResPtr: PInteger;
  BlacknessThr: Integer;

begin
  SetLength (FBlackPixelCountInColumns, Column);
  FillChar (FBlackPixelCountInColumns [0],
      SizeOf (Integer)* Column, 0);

{  if ImageType= it8bit then
    BlacknessThr:= (Self as TColoredImage).GrayThreshold
  else ??!!}
  if ImageType= itMonoChrome then
    BlacknessThr:= 1
  else
    raise EInvalidImage.Create ('');

  Result:= FBlackPixelCountInColumns;

  for r:= 0 to Row- 1 do
  begin
    ResPtr:= @Result [0];
    RowPtr:= ScanLine [r];
    
    for c:= 0 to Column- 1 do
    begin
      if ImageType= itMonoChrome then
        Inc (ResPtr^, RowPtr^)
      else if RowPtr^< BlacknessThr then
        Inc (ResPtr^);

      Inc (RowPtr);
      Inc (ResPtr);

    end;

  end;

  Result:= FBlackPixelCountInColumns;

end;

procedure TFMLImage.DeleteColumnsInRange (TopIndex, BotIndex: Integer);
var
  r, c: Integer;
  SourcePtr, DestPtr: PInteger;

begin
  if TopIndex<= BotIndex then
  begin
    for r:= 0 to Row- 1 do
    begin
      SourcePtr:= ScanLine [r];
      DestPtr:= SourcePtr;
    
      Inc (SourcePtr, BotIndex+ 1);
      Inc (DestPtr, TopIndex);
    
      for c:= BotIndex+ 1 to FColumn- 1 do
      begin
        DestPtr^:= SourcePtr^;
        Inc (SourcePtr);
        Inc (DestPtr);
      
      end;

    end;

    Column:= FColumn- BotIndex+ TopIndex- 1;
    
  end;
  
end;

function TFMLImage.CopyRows (TopRowIndex, BottomRowIndex: Integer): TFMLImage;
var
  r, c: Integer;
  SourcePtr, DestPtr: PInteger;
  
begin
  Result:= TFMLImage.Create;
  Result.FImageType:= Self.ImageType;
  Result.Column:= Column;
  Result.Row:= BottomRowIndex- TopRowIndex+ 1;

  for r:= TopRowIndex to BottomRowIndex do
  begin
     SourcePtr:= ScanLine [r];
     DestPtr:= Result.ScanLine [r- TopRowIndex];
     
     for c:= 0 to Column- 1 do
     begin
       DestPtr^:= SourcePtr^;
       Inc (DestPtr);
       Inc (SourcePtr);
       
     end;

  end;

end;

function TFMLImage.Rotate (AngleInDeg: Integer): TFMLImage;
var
  TempInt,
  r, c: Integer;
  SinA, CosA: Extended;
  DeltaR, DeltaC: Extended;
  MaxR, MinR, MaxC, MinC: Integer;
  NewR, NewC: Extended;
  NewIntR, NewIntC: Integer;
  Trans,
  TempArr: array of array of Extended;
  MaxTrans,
  TValue: Extended;
  BiasR, BiasC,
  i, j: Integer;
  PixPtr, SrcPixPtr: PInteger;
  TempArrPtr, TranPtr: PExtended;
  ZeroArray: array of Integer;

begin
  if ImageType<> it8bit then
    raise EInvalidImage.Create ('ImageType<> it8bit!');

  Result:= TFMLImage.Create;
  Result.FImageType:= it8bit;
  SinA:= Sin (AngleInDeg* Pi/ 180);
  BiasC:= 0; BiasR:= 0;

  if SinA< 0 then
    BiasC:= 1- Round (Row* SinA)
  else if 0< SinA then
    BiasR:= Round (Column* SinA)+ 1;
  CosA:= 0;
  if CosA< 0 then
    raise ENotImplemented.Create ('This procedure is not tested for the case in which Cos (RotateAngle)< 0');


  TempInt:= Row+ Column+ 2;
  SetLength (Trans, TempInt);
  SetLength (TempArr, TempInt);
  for r:= 0 to TempInt- 1 do
  begin
    SetLength (Trans [r], TempInt);
    SetLength (TempArr [r], TempInt);
    FillChar (Trans [r][0], SizeOf (Integer)* TempInt, 0);
    FillChar (TempArr [r][0], SizeOf (Integer)* TempInt, 0);

  end;

  MaxR:= 0; MinR:= BiasR; MaxC:=0 ;MinC:= BiasC;
  for r:= 0 to Row- 1 do
  begin
    PixPtr:= ScanLine [r];
//c_New= r Sin+ c Cos        [x, y] |Cos  -Sin|
//r_New= r Cos- c Sin               |+Sin  Cos|

    NewC:= BiasC+ r* SinA;
    NewR:= BiasR+ r* CosA;
    DeltaC:= CosA;
    DeltaR:= -SinA;

    MaxR:= 0; MinR:= BiasR; MaxC:=0 ;MinC:= BiasC;

    for c:= 0 to Column- 1 do
    begin
      NewIntC:= Trunc (NewC);
      NewIntR:= Trunc (NewR);

      if (NewIntR< 0) or (NewIntR+ 1>= Row+ Column) or
         (NewIntC< 0) or (NewIntc+ 1>= Row+ Column) then
        raise ERangeCheckError.Create ('TFMLImage.Rotate!'
         + IntToStr (NewIntR+ NewIntC+ c));

      if MaxR< NewIntR+ 1 then
        MaxR:= NewIntR+ 1
      else if NewIntR< MinR then
        MinR:= NewIntR;

      if MaxC< NewIntC+ 1 then
        MaxC:= NewIntC+ 1
      else if NewIntC< MinC then
        MinC:= NewIntC;

      TValue:= (NewIntR+ 1- NewR)* (NewIntC+ 1- NewC);
      TempArr [NewIntR, NewIntC]:=
        TempArr [NewIntR, NewIntC]+
          TValue* PixPtr^;
      Trans [NewIntR, NewIntC]:=
         Trans [NewIntR, NewIntC]+ TValue;

      TValue:= (NewR- NewIntR)* (NewIntC+ 1- NewC);
      TempArr [NewIntR+ 1, NewIntC]:=
        TempArr [NewIntR+ 1, NewIntC]+
        TValue* PixPtr^;
      Trans [NewIntR+ 1, NewIntC]:=
        Trans [NewIntR+ 1, NewIntC]+ TValue;

      TValue:= (NewIntR+ 1- NewR)* (NewC- NewIntC);;
      TempArr [NewIntR, NewIntC+ 1]:=
        TempArr [NewIntR, NewIntC+ 1]+
        TValue* PixPtr^;
      Trans [NewIntR, NewIntC+ 1]:=
        Trans [NewIntR, NewIntC+ 1]+ TValue;

      TValue:= (NewR- NewIntR)* (NewC- NewIntC);
      TempArr [NewIntR+ 1, NewIntC+ 1]:=
        TempArr [NewIntR+ 1, NewIntC+ 1]+
        TValue* PixPtr^;
      Trans [NewIntR+ 1, NewIntC+ 1]:=
        Trans [NewIntR+ 1, NewIntC+ 1]+ TValue;

      NewR:= NewR+ DeltaR;
      NewC:= NewC+ DeltaC;
      Inc (PixPtr);
      
    end;

  end;

  MaxTrans:= -1;
  for r:= MinR to MaxR- 1 do
  begin
    TranPtr:= @Trans [r][0];

    for c:= MinC to MaxC- 1 do
    begin
      if MaxTrans< TranPtr^ then
        MaxTrans:= TranPtr^;
        
      Inc (TranPtr);

    end;

  end;

  Result.Column:= MaxC- MinC;
  Result.Row:= MaxR- MinR;

  SetLength (ZeroArray, MaxC- MinC+ 1);
  for r:= 0 to MaxC- MinC do
    ZeroArray [r]:= 255;

  for r:= MinR to MaxR- 1 do
  begin
    PixPtr:= Result.ScanLine [r- MinR];
    SrcPixPtr:= @ZeroArray [0];
{
    if (r< 0) or (Row<= r) then
      SrcPixPtr:= @ZeroArray [0]
    else
      SrcPixPtr:= ScanLine [r];
}      
    TempArrPtr:= @TempArr [r][0];
//    TranPtr:= @Trans [r][0];

    for C:= MinC to MaxC- 1 do
    begin
      TValue:= TempArrPtr^;{ (TempArrPtr^* TranPtr^+
        (MaxTrans- TranPtr^)* SrcPixPtr^)/ MaxTrans;}
      PixPtr^:= Round (TValue);
{
      Inc (SrcPixPtr);
      Inc (TempArrPtr);
      Inc (TranPtr);
      Inc (PixPtr);
}      
    end;

  end;

  SetLength (ZeroArray, 0);
  for r:= 0 to High (Trans) do
  begin
    SetLength (Trans [r], 0);
    SetLength (TempArr [r], 0);

  end;
  
  SetLength (Trans, 0);
  SetLength (TempArr, 0);


end;

{
//??!!! TO BE DONE
}
function TFMLImage.ExtractFreemanFeature: TFreemanFeature;
  
  procedure FindFreePixFromTop;
  var
    r, c: Integer;
    PixPtr: PInteger;
    ActiveRowPtr: ^TObject;
    ActiveRow: TARowOfFreemanFeature;
    LastRowPropPtr,
    CurRowPropPtr: PLongWord;

  begin
    PixPtr:= ScanLine [0];
    ActiveRowPtr:= Result.GetPointerToFirst;
    ActiveRow:= TARowOfFreemanFeature (ActiveRowPtr^);
    CurRowPropPtr:= ActiveRow.GetPointerToFirst;

    for c:= 0 to FColumn- 1 do
    begin
      if PixPtr^<> BLACK then
        PLongWord (CurRowPropPtr^)^:= Ord (iclUp);

      Inc (PixPtr);
      Inc (CurRowPropPtr);

    end;

    for r:= 1 to FRow- 1 do
    begin
      LastRowPropPtr:= ActiveRow.GetPointerToFirst;
      Inc (ActiveRowPtr);
      ActiveRow:= TARowOfFreemanFeature (ActiveRowPtr^);
      CurRowPropPtr:= ActiveRow.GetPointerToFirst;
      PixPtr:= ScanLine [r];

      for c:= 0 to FColumn- 1 do
      begin
        if PixPtr^<> BLACK then
          PLongWord (CurRowPropPtr^)^:= PLongWord (LastRowPropPtr^)^;
        Inc (LastRowPropPtr);
        Inc (CurRowPropPtr);
        Inc (PixPtr);
        
      end;

    end;

  end;

  procedure FindFreePixFromBot;
  var
    r, c: Integer;
    PixPtr: PInteger;
    ActiveRowPtr: ^TObject;
    ActiveRow: TARowOfFreemanFeature;
    LastRowPropPtr,
    CurRowPropPtr: PLongWord;

  begin
    PixPtr:= ScanLine [FRow- 1];
    ActiveRowPtr:= Result.GetPointerToFirst;
    Inc (ActiveRowPtr, FRow- 1);
    
    ActiveRow:= TARowOfFreemanFeature (ActiveRowPtr^);
    CurRowPropPtr:= ActiveRow.GetPointerToFirst;
    
    for c:= 0 to FColumn- 1 do
    begin
      if PixPtr^<> BLACK then
        PLongWord (CurRowPropPtr^)^:= PLongWord (CurRowPropPtr^)^ or LongWord (iclDown);

      Inc (PixPtr);
      Inc (CurRowPropPtr);

    end;

    for r:= FRow- 2 downto 0 do
    begin
      LastRowPropPtr:= ActiveRow.GetPointerToFirst;
      Dec (ActiveRowPtr);
      ActiveRow:= TARowOfFreemanFeature (ActiveRowPtr^);
      CurRowPropPtr:= ActiveRow.GetPointerToFirst;
      PixPtr:= ScanLine [r];

      for c:= 0 to FColumn- 1 do
      begin
        if PixPtr^<> BLACK then
          PLongWord (CurRowPropPtr^)^:= PLongWord (CurRowPropPtr^)^
            or (PLongWord (LastRowPropPtr^)^ and Ord (iclDown));

        Inc (LastRowPropPtr);
        Inc (CurRowPropPtr);
        Inc (PixPtr);
        
      end;

    end;

  end;

  procedure FindFreePixFromRight;
  var
    LastColLabelPtr,
    ColsPixPtr,
    TargetLabelPtr: array of PInteger;
    r, c: Integer;
    PixPtr: ^PInteger;
    ActiveRowPtr: ^TObject;
    LastColPropPtr,
    CurColPropPtr: ^PLongWord;

  begin
    SetLength (LastColLabelPtr, FRow);
    SetLength (ColsPixPtr, FRow);
    SetLength (TargetLabelPtr, FRow);

    ActiveRowPtr:= Result.GetPointerToFirst;
    for r:= 0 to FRow- 1 do
    begin
      ColsPixPtr [r]:= ScanLine [r];
      Inc (ColsPixPtr [r], FColumn- 1);

      TargetLabelPtr [r]:= TARowOfFreemanFeature (ActiveRowPtr^).
          GetPointerToFirst;
      Inc (TargetLabelPtr [r], FColumn- 1);
      LastColLabelPtr [r]:= TargetLabelPtr [r];

      Inc (ActiveRowPtr);

    end;

    PixPtr:= @ColsPixPtr [0];
    CurColPropPtr:= @TargetLabelPtr [0];
    for r:= 0 to FRow- 1 do
    begin
      if PixPtr^^<> BLACK then
        PLongWord (CurColPropPtr^^)^:= PLongWord (CurColPropPtr^^)^ or LongWord (iclRight);

      Dec (PixPtr^);
      Inc (PixPtr);
      Dec (CurColPropPtr^);
      Inc (CurColPropPtr);

    end;

    for c:= FColumn- 2 downto 0 do
    begin
      PixPtr:= @ColsPixPtr [0];
      CurColPropPtr:= @TargetLabelPtr [0];
      LastColPropPtr:= @LastColLabelPtr [0]; 
    
      for r:= 0 to FRow- 1 do
      begin
        if PixPtr^^<> BLACK then
          PLongWord (CurColPropPtr^^)^:= PLongWord (CurColPropPtr^^)^
            or (PLongWord (LastColPropPtr^^)^ and Ord (iclRight));

        Dec (PixPtr^);
        Inc (PixPtr);
        Dec (CurColPropPtr^);
        Inc (CurColPropPtr);
        Dec (LastColPropPtr^);
        Inc (LastColPropPtr);

      end;
      
    end;

    SetLength (ColsPixPtr, 0);
    SetLength (TargetLabelPtr, 0);
    SetLength (LastColLabelPtr, 0);

  end;
  
  procedure FindFreePixFromLeft;
  var
    LastColLabelPtr,
    ColsPixPtr,
    TargetLabelPtr: array of PInteger;
    r, c: Integer;
    PixPtr: ^PInteger;
    ActiveRowPtr: ^TObject;
    LastColPropPtr,
    CurColPropPtr: ^PLongWord;

  begin
    SetLength (LastColLabelPtr, FRow);
    SetLength (ColsPixPtr, FRow);
    SetLength (TargetLabelPtr, FRow);

    ActiveRowPtr:= Result.GetPointerToFirst;
    for r:= 0 to FRow- 1 do
    begin
      ColsPixPtr [r]:= ScanLine [r];

      TargetLabelPtr [r]:= TARowOfFreemanFeature (ActiveRowPtr^).
          GetPointerToFirst;
      LastColLabelPtr [r]:= TargetLabelPtr [r];

      Inc (ActiveRowPtr);

    end;

    PixPtr:= @ColsPixPtr [0];
    CurColPropPtr:= @TargetLabelPtr [0];
    for r:= 0 to FRow- 1 do
    begin
      if PixPtr^^<> BLACK then
        PLongWord (CurColPropPtr^^)^:= PLongWord (CurColPropPtr^^)^ or LongWord (iclLeft);

      Inc (PixPtr^);
      Inc (PixPtr);
      Inc (CurColPropPtr^);
      Inc (CurColPropPtr);

    end;

    for c:= 1 to FColumn- 1 do
    begin
      PixPtr:= @ColsPixPtr [0];
      CurColPropPtr:= @TargetLabelPtr [0];
      LastColPropPtr:= @LastColLabelPtr [0]; 
    
      for r:= 0 to FRow- 1 do
      begin
        if PixPtr^^<> BLACK then
          PLongWord (CurColPropPtr^^)^:= PLongWord (CurColPropPtr^^)^
            or (PLongWord (LastColPropPtr^^)^ and Ord (iclLEFT));

        Inc (PixPtr^);
        Inc (PixPtr);
        Inc (CurColPropPtr^);
        Inc (CurColPropPtr);
        Inc (LastColPropPtr^);
        Inc (LastColPropPtr);

      end;
      
    end;

    SetLength (ColsPixPtr, 0);
    SetLength (TargetLabelPtr, 0);
    SetLength (LastColLabelPtr, 0);

  end;

  procedure FindBlackPixels;
  var
    r, c: Integer;
    PixPtr: PInteger;
    ActiveRowPtr: ^TObject;
    ActiveRow: TARowOfFreemanFeature;
    CurRowPropPtr: PLongWord;

  begin
    PixPtr:= ScanLine [0];
    ActiveRowPtr:= Result.GetPointerToFirst;
    ActiveRow:= TARowOfFreemanFeature (ActiveRowPtr^);
    CurRowPropPtr:= ActiveRow.GetPointerToFirst;

    for c:= 0 to FColumn- 1 do
    begin
      if PixPtr^= BLACK then
        PLongWord (CurRowPropPtr^)^:= Ord (iclBlackPixInFreeman);

      Inc (PixPtr);
      Inc (CurRowPropPtr);

    end;

    for r:= 1 to FRow- 1 do
    begin
      Inc (ActiveRowPtr);
      ActiveRow:= TARowOfFreemanFeature (ActiveRowPtr^);
      CurRowPropPtr:= ActiveRow.GetPointerToFirst;
      PixPtr:= ScanLine [r];

      for c:= 0 to FColumn- 1 do
      begin
        if PixPtr^= BLACK then
          PLongWord (CurRowPropPtr^)^:= Ord (iclBlackPixInFreeman);
          
//        Inc (LastRowPropPtr);
        Inc (CurRowPropPtr);
        Inc (PixPtr);

      end;

    end;
    
  end;

var
  r: Integer;
  ARowOfFreemanFeature: TARowOfFreemanFeature;

begin
  Result:= TFreemanFeature.Create (FRow, FColumn, (100* BlackPixCount) div (FRow* FColumn));

  Result.Allocate (FRow);
  for r:= 0 to FRow- 1 do
  begin
    ARowOfFreemanFeature:= TARowOfFreemanFeature.Create;
    ARowOfFreemanFeature.FillWithZero (FColumn);
      
    Result.Member [r]:= ARowOfFreemanFeature;
      
  end;

  Result.ARowofFreemanFeature [0].MemberAt [0];

  FindFreePixFromTop;// It should call first because it tries to set the value for black pixels
  FindFreePixFromBot;
  FindFreePixFromRight;
  FindFreePixFromLeft;

  FindBlackPixels;
//  FindHoles;
  
end;

function TFMLImage.MixImage (ImagePixels: TComponent): TFMLImage;
var
  MaxPoint: TPoint;
  NewRow,
  NewCol: Integer;
  Pixel: TMyPixel;
  i: Integer;
begin
  MaxPoint:= ImagePixels.GetMaximum;

  if Row< MaxPoint.r+ 1 then
    NewRow:= MaxPoint.r+ 1
  else
    NewRow:= FRow;

  if Column< MaxPoint.c+ 1 then
    NewCol:= MaxPoint.c+ 1
  else
    NewCol:= FColumn;
  MaxPoint.Free;

  Row:= NewRow;
  Column:= NewCol;

  for i:= 0 to ImagePixels.Count- 1 do
  begin
    Pixel:= ImagePixels.GetPixel (i);
    FBody [Pixel.Location.r, Pixel.Location.c]:=
          BLACK;
    FCenterOfMass.Move (Pixel.Location);

  end;
  
  Result:= Self;

end;

procedure TImageCollection.LoadFromBMLFile(FileName: string);
var
  i: Integer;
  b1, b2, b3: Byte;
  ImageNo: Integer;
  NewImage: TFMLImage;
  InputStream: TFileStream;
  Ptr: PObject;

begin
  if not FileExists (FileName) then
    raise EFileNotFound.Create (FileName);

  InputStream:= TFileStream.Create (FileName, fmOpenRead);
  InputStream.Read (b1, 1);
  InputStream.Read (b2, 1);
  InputStream.Read (b3, 1);
  ImageNo:= b3+ 100* b2+ 10000* b1;
  Allocate (ImageNo);

  Ptr:= GetPointerToFirst;
  for i:= 1 to ImageNo do
  begin
    NewImage:= TFMLImage.Create;
    NewImage.LoadFromBMLStream (InputStream);
    Ptr^:= NewImage;
    Inc (Ptr);

  end;

  InputStream.Free;

end;

procedure TImageCollection.AddImageCollection(
  ImageCollection: TimageCollection);
var
  SourcePtr, TargetPtr: PObject;
  LastSize, i: Integer;

begin
  LastSize:= Size; 
  Allocate (Size+ ImageCollection.Size);
  
  SourcePtr:= ImageCollection.GetPointerToFirst;
  TargetPtr:= GetPointerToFirst;
  Inc (TargetPtr, LastSize);
  
  for i:= 1 to ImageCollection.Size do
  begin
    TargetPtr^:= SourcePtr^;
    Inc (SourcePtr);
    Inc (TargetPtr);
    
  end;

end;

{ TComponentCollection }

procedure TComponentCollection.Add (NewComponent: TComponent);
begin
  inherited Add (NewComponent);

  if MaxR< NewComponent.MaxR then
    MaxR:= NewComponent.MaxR;

  if MaxC< NewComponent.MaxC then
    MaxC:= NewComponent.MaxC;

  if NewComponent.MinR< MinR then
    MinR:= NewComponent.MinR;

  if NewComponent.MinC< MinC then
    MinC:= NewComponent.MinC;

end;

procedure TComponentCollection.AddComponent (Component: TComponent);
begin
  inherited Add (Component);

  if MaxR< Component.MaxR then
    MaxR:= Component.MaxR;

  if MaxC< Component.MaxC then
    MaxC:= Component.MaxC;

  if Component.MinR< MinR then
    MinR:= Component.MinR;

  if Component.MinC< MinC then
    MinC:= Component.MinC;

end;

procedure TComponentCollection.Clear;
begin
  inherited;

  MinR:= MaxInt; MinC:= MaxInt;
  MaxR:= -1; MaxC:= -1;

end;

constructor TComponentCollection.Create;
begin
  inherited;

  MinR:= MaxInt; MinC:= MaxInt;
  MaxR:= -1; MaxC:= -1;

end;

procedure TComponentCollection.Delete (Index: Integer);
begin
  inherited Delete (Index);

  FindMaxPoint;
  FindMinPoint;
  
end;

destructor TComponentCollection.Destroy;
begin
 
  inherited;

end;

procedure TComponentCollection.FindMaxPoint;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  MaxR:= -1; MaxC:= -1;

  for i:= 1 to Size do
  begin
    if MaxR< TComponent (Ptr^).MaxR then
      MaxR:= TComponent (Ptr^).MaxR;
    if MaxC< TComponent (Ptr^).MaxC then
      MaxC:= TComponent (Ptr^).MaxC;

    Inc (Ptr);

  end;

end;

procedure TComponentCollection.FindMinPoint;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  MinR:= MaxInt; MinC:= MaxInt;

  for i:= 1 to Size do
  begin
    if TComponent (Ptr^).MinR< MinR then
      MinR:= TComponent (Ptr^).MinR;
    if TComponent (Ptr^).MinC< MinC then
      MinC:= TComponent (Ptr^).MinC;

    Inc (Ptr);

  end;

end;

function TComponentCollection.GetComponent (Index: Integer): TComponent;
begin
  Result:= FMembers [Index] as TComponent;
  
end;

function TComponentCollection.GetMaxPoint: TPoint;
begin
  if 0< MaxR then
    Result:= TPoint.Create (MaxR, MaxC)
  else
  begin
    FindMaxPoint;
    Result:= TPoint.Create (MaxR, MaxC);

  end;

end;

function TComponentCollection.GetMinPoint: TPoint;
begin
  if 0< MaxR then
    Result:= TPoint.Create (MinR, MinC)
  else
  begin
    FindMinPoint;
    Result:= TPoint.Create (MinR, MinC);

  end;
  
end;

function TComponentCollection.IsExists (r, c: Integer): Boolean;
var
  i: Integer;

begin
  for i:= 0 to Size- 1 do
    if Component [i].IsExists (r, c) then
    begin
      Result:= True;
      Exit;
      
    end;
    
  Result:= False;
  
end;

procedure TComponentCollection.RemoveInvalidComponents;
const
  WidthThr: Integer= 5;
  HeightThr: Integer= 5;
  
var
  MinPoint,
  MaxPoint: TPoint;
  ComponentsPixels: TComponent;
  i, Index: Integer;
  
begin

  for i:= 0 to Size- 1 do
  begin
    ComponentsPixels:= Component [i];

    MinPoint:= ComponentsPixels.GetMinimum;
    MaxPoint:= ComponentsPixels.GetMaximum;

    if (MaxPoint.c- MinPoint.c< WidthThr) or (MaxPoint.r- MinPoint.r< HeightThr) then
    begin
      ComponentsPixels.Free;
      Component [i]:= nil;
      
    end;
    
    MinPoint.Free;
    MaxPoint.Free;
    
  end;

  Index:= 0;
  for i:= 0 to Size- 1 do
    if Component [i]<> nil then
    begin
      Component [Index]:= Component [i];
      Inc (Index);
    end;

  for i:= Size- 1 downto Index+ 1 do
    Delete (i);

end;

procedure TFMLImage.SaveInFMLFile (Filename: String);
var
  FMLImageCollection: TImageCollection;

begin
  FMLImageCollection:= TImageCollection.Create;
  FMLImageCollection.AddImage (Self);
  FMLImageCollection.SaveToFile (Filename);
  FMLImageCollection.Clear;
  FMLImageCollection.Free;

end;

function TFMLImage.NewResize (NewRow, NewColumn: Integer;
  SaveAspectRatio: Boolean): TFMLImage;
{
  This function is tested just  
  for enlargements, but not for extraction.
}
var
  StartR, EndR,
  StartC, EndC: Extended;
  IntStartR, IntEndR,
  IntStartC, IntEndC: Integer;
  
  xc, yc: Integer;
  r, c: Integer;

  Dummy: Integer;
  RowPtr: PInteger;
  ExPtr: PDouble;
  
  CommonArea, CommonWidth,
  CommonHeight: Double;

  RowCoef, ColCoef: Double;
  NewBody: array of array of Double;
  BlankRowsCount, BlankColsCount: Integer;
  MainNewRow, MainNewCol: Integer;
  
begin
  BlankRowsCount:= 0;
  BlankColsCount:= 0;

  MainNewRow:= NewRow;
  MainNewCol:= NewColumn;
  
  if SaveAspectRatio then
  begin
    if (NewRow/ FRow)* FColumn< NewColumn then
    begin
      BlankColsCount:= NewColumn-
         Round ((NewRow* FColumn)/ FRow);
      BlankRowsCount:= 0;
      NewColumn:= Round ((NewRow* FColumn)/ FRow)

    end
    else
    begin
      BlankRowsCount:= NewRow-
         Round ((NewColumn* FRow)/ FColumn);
      BlankColsCount:= 0;
      NewRow:= Round ((NewColumn* FRow)/ FColumn);

    end;

  end;

  SetLength (NewBody, MainNewRow);
  for r:= 0 to MainNewRow- 1 do
  begin
    SetLength (NewBody [r], MainNewCol);
    FillChar (NewBody [r, 0], MainNewCol* SizeOf (Double), 0);

  end;

  RowCoef:= (MainNewRow- BlankRowsCount)/ FRow;
  ColCoef:= (MainNewCol- BlankColsCount)/ FColumn;
  StartR:= -RowCoef+ (BlankRowsCount+ 1) div 2;
  EndR:= +(BlankRowsCount+ 1) div 2;

  for r:= 0 to FRow- 1 do
  begin
    RowPtr:= Self.ScanLine [r];

    StartR:= StartR+ RowCoef;
    EndR:= EndR+ RowCoef;
    IntStartR:= Trunc (StartR);
    IntEndR:= Trunc (EndR- 1e-10);

    StartC:= (BlankColsCount+ 1) div 2;
    EndC:= ColCoef+ StartC;
    
    IntStartC:= Trunc (StartC);
    IntEndC:= Trunc (EndC- 1e-10);

    for c:= 0 to FColumn- 1 do
    begin
      if c= -1 then
       Break;

      Dummy:= RowPtr^;

      if Dummy<> 0 then
      begin
        for yc:= IntStartR to IntEndR do
          for xc:= IntStartC to IntEndC do
            NewBody [yc, xc]:= NewBody [yc, xc]+ 1;// i.e. CommonArea* Dummy;

{
}

{Remove the 1,2 and 3's Areas from the Newbodies (Some parts of these areas)}
        CommonHeight:= IntStartR- StartR;//CommonHeight is always <= 0
        yc:= IntStartR;

        for xc:= IntStartC to IntEndC do
{1,2,3}          NewBody [yc, xc]:= NewBody [yc, xc]+ CommonHeight* 1;

  {Remove the 7,8 and 9's Areas from the Newbodies (Some parts of these areas)}
        CommonHeight:= EndR- (IntEndR+ 1);//CommonHeight is always <= 0
        CommonArea:= CommonHeight;{* 1}

        yc:= IntEndR;
        for xc:= IntStartC to IntEndC do
{7,8,9}          NewBody [yc, xc]:= NewBody [yc, xc]+
            CommonArea;

  {Remove the 1, 4, 7's Areas from the Newbodies (Some other parts of these areas)}
        CommonWidth:= IntStartC- StartC;//CommonHeight is always <= 0
        CommonArea:= CommonWidth;{* 1}
        xc:= IntStartC;

        for yc:= IntStartR to IntEndR do
{1,4,7}          NewBody [yc, xc]:= NewBody [yc, xc]+
              CommonArea;


  {Remove the 3, 6, 9's Areas from the Newbodies (Some other parts of these areas)}
        CommonWidth:= EndC- (IntEndC+ 1);//CommonWidth is always <= 0
        CommonArea:= CommonWidth;{* 1}
        xc:= IntEndC;

        for yc:= IntStartR to IntEndR do
{3,6,9}   NewBody [yc, xc]:= NewBody [yc, xc]+
            CommonArea;

//---------------------------//
        NewBody [IntStartR, IntStartC]:= NewBody [IntStartR, IntStartC]+
          (StartR- IntStartR)* (StartC- IntStartC);

        NewBody [IntEndR, IntEndC]:= NewBody [IntEndR, IntEndC]+
          (IntEndR+ 1- EndR)* (IntEndC+ 1- EndC);

        NewBody [IntEndR, IntStartC]:= NewBody [IntEndR, IntStartC]+
          (IntEndR+ 1- EndR)* (StartC- IntStartC);

        NewBody [IntStartR, IntEndC]:= NewBody [IntStartR, IntEndC]+
          (StartR- IntStartR)* (IntEndC+ 1- EndC);

      end;

      StartC:= StartC+ ColCoef;
      EndC:= EndC+ ColCoef;
      IntStartC:= Trunc (StartC);
      IntEndC:= Trunc (EndC- 1e-10);

      Inc (RowPtr);

    end;

  end;

  Result:= TFMLImage.Create;
  Result.FImageKind:= FImageKind;
  Result.FImageType:= FImageType;
  Result.Column:= MainNewCol;
  Result.Row:= MainNewRow;

  for r:= 0 to MainNewRow- 1 do
  begin
    ExPtr:= @NewBody [r, 0];
    RowPtr:= Result.ScanLine [r];

    for c:= 0 to MainNewCol- 1 do
    begin
      RowPtr^:= Round (ExPtr^);
      if not (RowPtr^ in [0, 1]) then
        raise Exception.Create ('1+ 1e-10< ExPtr^');

      Inc (RowPtr);
      Inc (ExPtr);

    end;
    SetLength (NewBody [r], 0);

  end;
  SetLength (NewBody, 0);
  
end;

function TFMLImage.GetComponentByOnePoint (APoint: TPoint): TComponent;
const
{
  These values will be added successively.
}
  IncAdjancedRow: array [0..3] of Integer= (+1, -2, 2, 0);
  IncAdjancedColumn: array [0..3] of Integer= (0, 0, +1, -2);
  
  procedure DFS (r, c: Integer);
  var
    i: Integer;
    
  begin
    Result.Add (r, c);

    for i:= 0 to 3 do
    begin
      Inc (r, IncAdjancedRow [i]);
      Inc (c, IncAdjancedColumn [i]);
      
      if (r< FRow) and  (0<= r) and
        (c< FColumn) and  (0<= c)then
        if FBody [r, c]= BLACK then
          if not Result.IsExists (r, c) then
            DFS (r, c);

    end;
    
  end;

begin
  Result:= TComponent.Create;
  DFS (APoint.r, APoint.c);
  
end;

constructor TFMLImage.Create (BlackPoints: TPointCollection);
var
  r, c: Integer;
  i: Integer;
  Ptr: PObject;
  IntPtr: PInteger;
  MinData,
  TempP: TPoint;

begin
  inherited Create;

  FImageType:= itMonoChrome;
  HistogramIsCalced:= False;
  FRow:= -1; FColumn:= -1; FPattern:= -1;
  FIsBlank:= mbUnSet;

  Column:= BlackPoints.Max.c- BlackPoints.Min.c+ 1;
  Row:= BlackPoints.Max.r- BlackPoints.Min.r+ 1;
  MinData:= BlackPoints.Min;


  for r:= 0 to Row- 1 do
  begin
    IntPtr:= @FBody [r, 0];

    for c:= 0 to Column- 1 do
    begin
      IntPtr^:= WHITE;
      Inc (IntPtr);

    end;

  end;

  Ptr:= BlackPoints.GetPointerToFirst;

  FCenterOfMass:= TPoint.Create (0, 0);
  for i:= 1 to BlackPoints.Size do
  begin
    TempP:= TPoint (Ptr^);
    FBody [TempP.r- MinData.r, TempP.c- MinData.c]:= BLACK;
    FCenterOfMass.Move (TempP.r- MinData.r, TempP.c- MinData.c);
    Inc (Ptr);
     
  end;

end;

procedure TFMLImage.LoadFromFMLStream (InputStream: TFileStream);
var
  r, c: Integer;
  b1, b2: Byte;
  Ptr: PInteger;
  b: array of Byte;
  bPtr: PByte;

begin
  if FCenterOfMass<> nil then
    FCenterOfMass.Free;
  FCenterOfMass:= TPoint.Create (0, 0);

  BlackPixCount:= 0;

  FImageType:= itMonoChrome;
   
  InputStream.Read (b1, 1);
  InputStream.Read (b2, 1);
  FPattern:= b1+ b2 shl 8;

  InputStream.Read (b1, 1);
  InputStream.Read (b2, 1);
  Row:= b1+ b2 shl 8;

  InputStream.Read (b1, 1);
  InputStream.Read (b2, 1);
  Column:= b1+ b2 shl 8;

  SetLength (b, 2* FColumn);
  
  for r:= 0 to FRow- 1 do
  begin
    Ptr:= ScanLine [r];

    InputStream.Read (b [0], 2* FColumn);
    bPtr:= @b [0];

    for c:= 0 to FColumn- 1 do
    begin
      b1:= bPtr^;
      Inc (bPtr);
      b2:= bPtr^;
      Inc (bPtr);
      Ptr^:= b1+ b2 shl 8;

      if 1< Ptr^ then
        raise EInvalidImage.Create ('Pixel bigger than one!'+
                IntToStr (r)+ ':'+ IntToStr (c));

      if Ptr^= BLACK then
      begin
        Inc (BlackPixCount);
        FCenterOfMass.Move (r ,c);

      end;
      Inc (Ptr);

    end;

  end;

  InputStream.Read (b1, 1);
  InputStream.Read (b2, 1);
  if (b1<> 255) or (b2<> 255) then
    raise EInvalidImage.Create ('Last two byte must be 255.');
  SetLength (b, 0);

end;

procedure TFMLImage.SaveInFMLStream (OutputStream: TFileStream);
var
  r, c: Integer;
  b1, b2: Byte;

begin
  b1:= Pattern mod 256;
  b2:=  Pattern div 256;

  OutputStream.Write (b1,  1);
  OutputStream.Write (b2, 1);

  b1:= Row mod 256;
  b2:= Row div 256;
  OutputStream.Write (b1,  1);
  OutputStream.Write (b2, 1);

  b1:= Column mod 256;
  b2:= Column div 256;
  OutputStream.Write (b1,  1);
  OutputStream.Write (b2, 1);

  for r:= 0 to Row- 1 do
    for c:= 0 to Column- 1 do
    begin
      b1:= FBody [r][c] mod 256;
      b2:= FBody [r][c] div 256;
      OutputStream.Write (b1,  1);
      OutputStream.Write (b2, 1);
      
    end;
    
  b1:= 255;
  b2:= 255;
  OutputStream.Write (b1,  1);
  OutputStream.Write (b2, 1);

end;


function TFMLImage.Crop: TFMLImage;
var
  r, c: Integer;
  IntPtr: PInteger;
  StartR, StartC,
  FinR, FinC: Integer;
  
begin
  Result:= Self;
  BlackPixelCountInRows;
  BlackPixelCountInColumns;

  IntPtr:= @FBlackPixelCountInRows [0];
  StartR:= 0;
  for r:= 0 to Row- 1 do
  begin
    if IntPtr^<> 0 then
    begin
      StartR:= r;
      Break;

    end;

    Inc (IntPtr);

  end;

  IntPtr:= @FBlackPixelCountInRows [Row- 1];

  FinR:= FRow;
  for r:= Row- 1 downto 0 do
  begin
    if IntPtr^<> 0 then
    begin
      FinR:= r;
      Break;
      
    end;
      
    Dec (IntPtr);

  end;

  DeleteRowsInRange (FinR+ 1, Row- 1);
  DeleteRowsInRange (0, StartR- 1);

  IntPtr:= @FBlackPixelCountInColumns [0];
  StartC:= 0;
  for c:= 0 to Column- 1 do
  begin
    if IntPtr^<> 0 then
    begin
      StartC:= c;
      Break;

    end;

    Inc (IntPtr);

  end;

  IntPtr:= @FBlackPixelCountInColumns [Column- 1];
  FinC:= Column- 1;
  for c:= Column- 1 downto 0 do
  begin
    if IntPtr^<> 0 then
    begin
      FinC:= c;
      Break;
      
    end;

    Dec (IntPtr);
     
  end;
  
  DeleteColumnsInRange (FinC+ 1, Column- 1);
  DeleteColumnsInRange (0, StartC- 1);

end;

function TFMLImage.HitAndMiss(Mask: TArrayofArrayofInt): TFMLImage;
//http://www.cee.hw.ac.uk/hipr/html/hitmiss.html
//Dont Care is -1.

var
  NewBody: TArrayofArrayofInt;
  BodyPtr: PInteger;
  r, c, BiasR, BiasC,
  ir, ic,
  S, NonDontCareCells,
  MaskRow, MaskCol: Integer;

begin
  Result:= Self;
//  Result.SaveAsText ('C:\Main.txt');
  
  if IsBlank= mbTrue then
    Exit;
    
  MaskRow:= Length (Mask);
  MaskCol:= Length (Mask [0]);
  NonDontCareCells:= 0;
  
  for r:= 0 to MaskRow- 1 do
    for c:= 0 to MaskCol- 1 do
      if Mask [r, c]<> -1 then
        Inc (NonDontCareCells);

  SetLength (NewBody, FRow+ MaskRow- 1);
  BiasR:= (MaskRow- 1) div 2;
  BiasC:= (MaskCol- 1) div 2;

  for r:= 0 to FRow+ MaskRow- 2 do
  begin
    SetLength (NewBody [r], Column+ MaskCol- 1);
    FillChar (NewBody [r, 0], SizeOf (NewBody [r]), 0);
    
  end;

  for r:= 0 to FRow- 1 do
    Move (FBody [r, 0], NewBody [r+ BiasR, BiasC], SizeOf (Integer)* Column);

  for r:= BiasR to FRow+ BiasR- 1 do
  begin
    BodyPtr:= ScanLine [r- BiasR];
    
    for c:= BiasC to Column+ BiasC- 1 do
    begin
      S:= 0;
      if (Mask [BiasR, BiasC]<> -1) and
       (Mask [BiasR, BiasC]= BodyPtr^) then
        for ir:= -BiasR to BiasR do
        begin
          for ic:= -BiasC to BiasC do
            if (Mask [BiasR+ ir, BiasC+ ic]= NewBody [r+ ir, c+ ic]) then//          {NewBody [r+ BiasR+ ir,c+ BiasR+ ic];}
              Inc (S)
            else if Mask [BiasR+ ir, BiasC+ ic]<> -1 then
              Break;


        end;
      
      if S<> NonDontCareCells then
        BodyPtr^:= WHITE
      else
        BodyPtr^:= BLACK;        

      Inc (BodyPtr);
    end;

  end;

  for r:= 0 to FRow+ MaskRow- 2 do
    SetLength (NewBody [r], 0);
  SetLength (NewBody, 0);

//  Result.SaveAsText ('C:\HitnMisses.txt');
  
end;

function TFMLImage.Closing (Mask: TArrayofArrayofInt): TFMLImage;
begin
  Result:= Self.Erode (Mask).Dilate (Mask);
    
end;

function TFMLImage.Opening(Mask: TArrayofArrayofInt): TFMLImage;
begin
  Result:= Self.Dilate (Mask).Erode (Mask);

end;

function TFMLImage.ImageThickness: Integer;
(*$J+*)
const
  ThicknessCount: array of Integer= nil;

(*$J-*)

procedure Allocate;
var
  M: Integer;

begin
  M:= Row;
  if M< Column then
    M:= Column;
  Inc (M);
  
  if ThicknessCount= nil then
    SetLength (ThicknessCount, M)
  else if Length (ThicknessCount)< M then
    SetLength (ThicknessCount, M);

  FillChar (ThicknessCount [0], M* SizeOf (Integer), 0);

end;

var
  i, r, c: Integer;
  Ptr: PInteger;
  Width,
  Max: Integer;

begin
  Allocate;

  //Rows
  for r:= 0 to Row- 1 do
  begin
    Ptr:= ScanLine [r];

    c:= 0;     
    Width:= 0;
    
    while c< Column do
    begin

      if Ptr^= BLACK then
      begin
        while c< Column do
        begin
          if Ptr^<> BLACK then
            Break;

          Inc (Width);
          Inc (c);
          Inc (Ptr);

        end;
        Inc (ThicknessCount [Width]);
        Width:= 0;

      end;

      Inc (c);
      Inc (Ptr);

    end;

  end;

  //Columns
  for c:= 0 to Column- 1 do
  begin
    r:= 0;     
    Width:= 0;
    
    while r< Row do
    begin

      if FBody [r][c]= BLACK then
      begin

        while r< Row do
        begin
          if FBody [r][c]<> BLACK then
          begin
            Break;

          end;

          Inc (Width);
          Inc (r);

        end;
        Inc (ThicknessCount [Width]);
        Width:= 0;

      end;

      Inc (r);

    end;

  end;

  Result:= 0;
  Max:= -1;

  for i:= 0 to Math.Max (Row, Column) do
  begin
    if Max< ThicknessCount [i] then
    begin
      Max:= ThicknessCount [i];
      Result:= i;

    end

  end;

end;

function TFMLImage.ThickTheImage: TFMLImage;
(*$J+*)
const
  Masks: array of TArrayofArrayofInt= nil;
  TL: TPoint= nil;
  BR: TPoint= nil;
(*$J+*)

  procedure Init;
  var
    i, j: Integer;

  begin
    SetLength (Masks, 8);
    for i:= 0 to 7 do
    begin
      SetLength (Masks [i], 3);
      for j:= 0 to 2 do
        SetLength (Masks [i][j], 3);

    end;

  //B{1}=[-1 -1 -1; 0 1 0 ; 1 1 1];
    Masks [0][0, 0]:= +1; Masks [0][0, 1]:= +1;Masks [0][0, 2]:= +1;
    Masks [0][1, 0]:= -1; Masks [0][1, 1]:=  0;Masks [0][1, 2]:= -1;
    Masks [0][2, 0]:=  0; Masks [0][2, 1]:=  0;Masks [0][2, 2]:=  0;
  
  //B{2}=[0 -1 -1 ;1 1 -1; 1 1 0];
    Masks [1][0, 0]:= -1; Masks [1][0, 1]:= +1;Masks [1][0, 2]:= +1;
    Masks [1][1, 0]:=  0; Masks [1][1, 1]:=  0;Masks [1][1, 2]:= +1;
    Masks [1][2, 0]:=  0; Masks [1][2, 1]:=  0;Masks [1][2, 2]:= -1;

  {B3=[1 0 -1;
          1 1 -1;
          1 0 -1];
          }
    Masks [2][0, 0]:=  0; Masks [2][0, 1]:= -1;Masks [2][0, 2]:= +1;
    Masks [2][1, 0]:=  0; Masks [2][1, 1]:=  0;Masks [2][1, 2]:= +1;
    Masks [2][2, 0]:=  0; Masks [2][2, 1]:= -1;Masks [2][2, 2]:= +1;

  {B4=[1 1 0;
       1 1 -1;
      0 -1 -1]
  }
    Masks [3][0, 0]:=  0; Masks [3][0, 1]:=  0;Masks [3][0, 2]:= -1;
    Masks [3][1, 0]:=  0; Masks [3][1, 1]:=  0;Masks [3][1, 2]:= +1;
    Masks [3][2, 0]:= -1; Masks [3][2, 1]:= +1;Masks [3][2, 2]:= +1;

  {B5=[1 1 1;
       0 1 0;
       -1 -1 -1];
    }
    Masks [4][0, 0]:=  0; Masks [4][0, 1]:=  0;Masks [4][0, 2]:=  0;
    Masks [4][1, 0]:= -1; Masks [4][1, 1]:=  0;Masks [4][1, 2]:= -1;
    Masks [4][2, 0]:= +1; Masks [4][2, 1]:= +1;Masks [4][2, 2]:= +1;

{  B6=[0 1 1;
      -1 1 1;
      -1 -1 0];
      }
      
    Masks [5][0, 0]:= -1; Masks [5][0, 1]:=  0;Masks [5][0, 2]:=  0;
    Masks [5][1, 0]:= +1; Masks [5][1, 1]:=  0;Masks [5][1, 2]:=  0;
    Masks [5][2, 0]:= +1; Masks [5][2, 1]:= +1;Masks [5][2, 2]:= -1;
{ B7=  [-1 0 1;
        -1 1 1;
        -1 0 1];
        }
    Masks [6][0, 0]:= +1; Masks [6][0, 1]:= -1;Masks [6][0, 2]:=  0;
    Masks [6][1, 0]:= +1; Masks [6][1, 1]:=  0;Masks [6][1, 2]:=  0;
    Masks [6][2, 0]:= +1; Masks [6][2, 1]:= -1;Masks [6][2, 2]:=  0;

    {B8=[-1 -1 0;
         -1 1 1;
          0 1 1];
            }
    Masks [7][0, 0]:= +1; Masks [7][0, 1]:=  0;Masks [7][0, 2]:= -1;
    Masks [7][1, 0]:= +1; Masks [7][1, 1]:=  0;Masks [7][1, 2]:=  0;
    Masks [7][2, 0]:= -1; Masks [7][2, 1]:=  0;Masks [7][2, 2]:=  0;

    TL:= TPoint.Create (0, 0);
    BR:= TPoint.Create (0, 0);
    
  end;

var
  i: Integer;
  MainImage: TFMLImage;
  MainPtr, BodyPtr: PInteger;
  r, c: Integer;
  Changed: Boolean;
  
begin
  if Masks= nil then
    Init;
  BR.r:= Self.Row;
  BR.c:= Self.Column;
  MainImage:= Self.Copy (TL, BR);
//  Self.SaveAsText ('C:\Main.txt');

  Result:= Self;
  Changed:= True;

  while Changed do
  begin
//    Changed:= False;
    for i:= 0 to 3 do
    begin
      MainImage.Free;
      MainImage:= Self.Copy (TL, BR);
      Self.HitAndMiss (Masks [2* i]);

      for r:= 0 to Row- 1 do
      begin
        MainPtr:= MainImage.ScanLine [r];
        BodyPtr:= ScanLine [r];

        for c:= 0 to Column- 1 do
        begin
          BodyPtr^:= (MainPtr^ or BodyPtr^);
          Inc (MainPtr);
          Inc (BodyPtr);

        end;

      end;

    end;
//    Result.SaveAsText ('C:\ThickedImage.txt');
    Break;
    
    for r:= 0 to Row- 1 do
    begin
      MainPtr:= MainImage.ScanLine [r];
      BodyPtr:= ScanLine [r];

      for c:= 0 to Column- 1 do
      begin
        if MainPtr^<> BodyPtr^ then
        begin
          if r* c= -1 then
            Break;

          Changed:= True;
          Break;

        end;
        Inc (MainPtr);
        Inc (BodyPtr);
        
      end;
      if Changed then
        Break;

    end;

  end;
  
  MainImage.Free;

end;

function TFMLImage.ThinTheImage: TFMLImage;
(*$J+*)
const
  Masks: array of TArrayofArrayofInt= nil;
  TL: TPoint= nil;
  BR: TPoint= nil;
(*$J+*)

  procedure Init;
  var
    i, j: Integer;

  begin
    SetLength (Masks, 8);
    for i:= 0 to 7 do
    begin
      SetLength (Masks [i], 3);
      for j:= 0 to 2 do
        SetLength (Masks [i][j], 3);

    end;

  //B{1}=[-1 -1 -1; 0 1 0 ; 1 1 1];
    Masks [0][0, 0]:=  0; Masks [0][0, 1]:=  0;Masks [0][0, 2]:=  0;
    Masks [0][1, 0]:= -1; Masks [0][1, 1]:= +1;Masks [0][1, 2]:= -1;
    Masks [0][2, 0]:= +1; Masks [0][2, 1]:= +1;Masks [0][2, 2]:= +1;

  //B{2}=[0 -1 -1 ;1 1 -1; 1 1 0];
    Masks [1][0, 0]:= -1; Masks [1][0, 1]:=  0;Masks [1][0, 2]:=  0;
    Masks [1][1, 0]:= +1; Masks [1][1, 1]:= +1;Masks [1][1, 2]:=  0;
    Masks [1][2, 0]:= +1; Masks [1][2, 1]:= +1;Masks [1][2, 2]:= -1;

  {B3=[1 0 -1;
          1 1 -1;
          1 0 -1];
          }
    Masks [2][0, 0]:= +1; Masks [2][0, 1]:= -1;Masks [2][0, 2]:=  0;
    Masks [2][1, 0]:= +1; Masks [2][1, 1]:= +1;Masks [2][1, 2]:=  0;
    Masks [2][2, 0]:= +1; Masks [2][2, 1]:= -1;Masks [2][2, 2]:=  0;

  {B4=[1 1 0;
       1 1 -1;
      0 -1 -1]
  }
    Masks [3][0, 0]:= +1; Masks [3][0, 1]:= +1;Masks [3][0, 2]:= -1;
    Masks [3][1, 0]:= +1; Masks [3][1, 1]:= +1;Masks [3][1, 2]:=  0;
    Masks [3][2, 0]:= -1; Masks [3][2, 1]:=  0;Masks [3][2, 2]:=  0;

  {B5=[1 1 1;
       0 1 0;
       -1 -1 -1];
    }
    Masks [4][0, 0]:= +1; Masks [4][0, 1]:= +1;Masks [4][0, 2]:= +1;
    Masks [4][1, 0]:= -1; Masks [4][1, 1]:= +1;Masks [4][1, 2]:= -1;
    Masks [4][2, 0]:=  0; Masks [4][2, 1]:=  0;Masks [4][2, 2]:=  0;

{  B6=[0 1 1;
      -1 1 1;
      -1 -1 0];
      }
      
    Masks [5][0, 0]:= -1; Masks [5][0, 1]:= +1;Masks [5][0, 2]:= +1;
    Masks [5][1, 0]:=  0; Masks [5][1, 1]:= +1;Masks [5][1, 2]:= +1;
    Masks [5][2, 0]:=  0; Masks [5][2, 1]:=  0;Masks [5][2, 2]:= -1;
{ B7=  [-1 0 1;
        -1 1 1;
        -1 0 1];
        }
    Masks [6][0, 0]:=  0; Masks [6][0, 1]:= -1;Masks [6][0, 2]:= +1;
    Masks [6][1, 0]:=  0; Masks [6][1, 1]:= +1;Masks [6][1, 2]:= +1;
    Masks [6][2, 0]:=  0; Masks [6][2, 1]:= -1;Masks [6][2, 2]:= +1;

{B8=[-1 -1 0;
     -1 1 1;
      0 1 1];
        }
    Masks [7][0, 0]:=  0; Masks [7][0, 1]:= +1;Masks [7][0, 2]:= -1;
    Masks [7][1, 0]:=  0; Masks [7][1, 1]:= +1;Masks [7][1, 2]:= +1;
    Masks [7][2, 0]:= -1; Masks [7][2, 1]:= +1;Masks [7][2, 2]:= +1;

    TL:= TPoint.Create (0, 0);
    BR:= TPoint.Create (0, 0);
    
  end;

var
  i: Integer;
  MainImage: TFMLImage;
  MainPtr, BodyPtr: PInteger;
  r, c: Integer;
  
begin
  if Masks= nil then
    Init;
  BR.r:= Self.Row;
  BR.c:= Self.Column;
  MainImage:= Self.Copy (TL, BR);
//  Self.SaveAsText ('C:\Main.txt');

  Result:= Self;
  for i:= 0 to 3 do
  begin
    MainImage.Free;
    MainImage:= Self.Copy (TL, BR);
    Self.HitAndMiss (Masks [2* i]);

    for r:= 0 to Row- 1 do
    begin
      MainPtr:= MainImage.ScanLine [r];
      BodyPtr:= ScanLine [r];

      for c:= 0 to Column- 1 do
      begin
        if BodyPtr^= 1 then
          BodyPtr^:= 0
        else
          BodyPtr^:= MainPtr^;
          
        Inc (MainPtr);
        Inc (BodyPtr);

      end;

    end;

  end;
//  Result.SaveAsText ('C:\ThinedImage.txt');

  MainImage.Free;

end;

function TFMLImage.ReverseColor: TFMLImage;
var
  Ptr: PInteger;
  r, c: Integer;

begin
  for r:= 0 to FRow- 1 do
  begin
    Ptr:= ScanLine [r];
    
    for c:= 0 to FColumn- 1 do
    begin
      Ptr^:= 1 xor Ptr^;
      Inc (Ptr);
      
    end;

  end;

  Result:= Self;
  
end;

procedure TFMLImage.LoadFromBMLFile (var InputFile: TByteFile);
begin
   raise ENotImplemented.Create ('LoadFromBMLFile (var InputFile: TByteFile)');
   
end;

procedure TFMLImage.LoadFromBMLStream (InputStream: TFileStream);
var
  r, c: Integer;
  b1: Byte;
  Ptr: PInteger;
  Bytes: array of Byte;
  Index: Integer;
  ActiveBytes: PByte;
  
begin
  if FCenterOfMass<> nil then
    FCenterOfMass.Free;
  FCenterOfMass:= TPoint.Create (0, 0);

  BlackPixCount:= 0;

  FImageType:= itMonoChrome;
   
  InputStream.Read (b1, 1);
  FPattern:= b1;

  InputStream.Read (b1, 1);
  Row:= b1;
  InputStream.Read (b1, 1);
  Column:= b1;
  SetLength (Bytes, (Row* Column+ 7) div 8);
  InputStream.Read (Bytes [0], (Row* Column+ 7) div 8);
  ActiveBytes:= @Bytes [0];
  Index:= 0;

  for r:= 0 to FRow- 1 do
  begin
    Ptr:= ScanLine [r];
    for c:= 0 to FColumn- 1 do
    begin
      Ptr^:= (ActiveBytes^ and (1 shl (Index mod 8)))
        shr (Index mod 8);

      if Ptr^= BLACK then
      begin
        Inc (BlackPixCount);
        FCenterOfMass.Move (r ,c);

      end;

      Inc (Index);
      Inc (Ptr);

      if Index mod 8= 0 then
        Inc (ActiveBytes);

    end;
    
  end;

end;

procedure TComponentCollection.SetComponent(Index: Integer;
  const Value: TComponent);
begin
//  Member [Index];// For Range Checking

  FMembers [Index]:= Value;
  FindMaxPoint;
  FindMinPoint;
  
end;

{ TVector }

constructor TVector.Create;
begin
  inherited;

  Fx:= 0; Fy:= 0;
end;

function TVector.Add (AnotherVector: TVector): TVector;
begin
  Result:= TVector.Create (x+ AnotherVector.x,
        y+ AnotherVector.y);

end;

constructor TVector.Create (r, Teta: Extended);
begin
  inherited Create;

  FLength:= r;
  FTeta:= Teta;
  Fx:= 0; Fy:= 0;

end;

constructor TVector.CreateXY (x, y: Extended);
begin
  inherited Create;
  
  Fx:= x;
  Fy:= y;

  FLength:= Sqrt (Sqr (Fx)+ Sqr (Fy));
  FTeta:= MyArcTan (Fy, Fx);
    
end;

function TVector.Getx: Extended;
begin
  if Abs (Fx)< 1e-10  then
    Fx:= FLength* Cos (FTeta/ 180* Pi);

  Result:= Fx;

end;

function TVector.Gety: Extended;
begin
  if Abs (Fy)< 1e-10  then
    Fy:= FLength* Sin (FTeta/ 180* Pi);
    
  Result:= Fx;

end;

function TVector.Multiply(ASize: Extended): TVector;
begin
  Result:= TVector.Create (x+ ASize, y+ ASize);

end;

function TFMLImage.RemoveColors (AColor: Integer): TFMLImage;
begin
  raise ENotImplementedYet.Create ('TFMLImage.RemoveColors');
  
end;

{ TColoredImage }
(*
*)
procedure TColoredImage.CalculateHistogram;
const
  Matrix: array [0..2] of Extended= (0.299, 0.587, 0.114);

var
  r, c: Integer;
  IntPtr: PInteger;
  Red, Green, Blue, Temp: Integer;
  
begin
  if ImageType= it8bit then
  begin

    FillChar (FHistogram, SizeOf (FHistogram), 0);

    for r:= 0 to FRow- 1 do
    begin
      IntPtr:= @FBody [r, 0];

      for c:= 0 to FColumn- 1 do
      begin
        Inc (FHistogram [IntPtr^]);
        Inc (IntPtr);

      end;

    end;
  end
  else if ImageType= it24Bit then
  begin
    for r:= 0 to Row- 1 do
    begin
      IntPtr:= ScanLine [r];
      
      for c:= 0 to Column- 1 do
      begin
        Red:= (IntPtr^) and $FF;
        Green:= (IntPtr^ and $FF00) shr 8;
        Blue:= (IntPtr^ and $FF0000) shr 16;

        Temp:= Round (Matrix [0]* Red+ Matrix [1]* Green+ Matrix [2]* Blue);

        Inc (FHistogram [Temp]);

      end;

    end;

  end;
  HistogramIsCalced:= True;

end;

function TColoredImage.ConvertToBinary: TFMLImage;
var
  Threshold: Integer;
  r, c: Integer;
  SourcePtr, TargetPtr: PInteger;
  TempImage: TColoredImage;
  BLACK, WHITE: Integer; 
  
begin
  if ImageType= it24bit then
  begin
    TempImage:= ConvertToGrayScale;
    Result:= TempImage.ConvertToBinary;
    TempImage.Free;

  end
  else if ImageType= it8bit then
  begin
    Threshold:= GrayThreshold;
    Result:= TFMLImage.Create;
    Result.FImageType:= itMonoChrome;
    Result.Column:= Column;
    Result.Row:= Row;
    BLACK:= Result.GetBlackColor;
    WHITE:= Result.GetWhiteColor;

    for r:= 0 to Row- 1 do
    begin
      SourcePtr:= ScanLine [r];
      TargetPtr:= Result.ScanLine [r];

      for c:= 0 to Column- 1 do
      begin
        if SourcePtr^< Threshold then
          TargetPtr^:= BLACK
        else
          TargetPtr^:= WHITE;

        Inc (SourcePtr);
        Inc (TargetPtr);

      end;

    end;

  end
  else
    raise EInvalidImage.Create ('In ConvertToBinary!');

end;

function TColoredImage.ConvertToGrayScale: TColoredImage;
const
  RedCoef= 0.299;
  GreenCoef= 0.587;
  BlueCoef= 0.114;

var
  r, c,
  Temp,
  Red, Blue, Green: Integer;
  PixPtr, ResPtr: PInteger;
  
begin
  if ImageType<> it24bit then
    raise Exception.Create ('Invalid Bitmap File!');

  Result:= TColoredImage.Create (it8bit);

  Result.Column:= Column;
  Result.Row:= Row;
  Result.FImageType:= it8bit;

  for r:= 0 to Row- 1 do
  begin
    PixPtr:= ScanLine [r];
    ResPtr:= Result.ScanLine [r];

    for c:= 0 to Column- 1 do
    begin

      Red:= PixPtr^ and $FF;
      Green:= (PixPtr^ and $FF00) shr 8;
      Blue:= (PixPtr^ and $FF0000) shr 16;

      Temp:= Round (RedCoef* Red+ GreenCoef* Green+ BlueCoef* Blue);
      ResPtr^:= Temp;

      Inc (ResPtr);
      Inc (PixPtr);
      
    end;
    
  end;

end;

function TColoredImage.Copy (TL, BR: TPoint): TColoredImage;
begin
  Result:= inherited CopyPixels (TL, BR) as TColoredImage;

end;

constructor TColoredImage.Create (ImageType: TImageType);
begin
  inherited;
  
end;

function TColoredImage.GetAsBitmap: TBitmap;
var
  r, c: Integer;
  PixPtr: PInteger;
  RGBTipleRowPtr: ^TRGBTriple;
  RowPtr: PByte;
  
begin
  Result:= TBitmap.Create;
  
  case ImageType of
    it8Bit:
    begin
      Result.PixelFormat:= pf24bit;
      Result.Height:= FRow;
      Result.Width:= FColumn;

      for r:= 0 to Row- 1 do
      begin
        RGBTipleRowPtr:= Result.ScanLine [r];
        PixPtr:= ScanLine [r];

        for c:= 0 to FColumn- 1 do
        begin
//          Result.Canvas.Pixels [c, r]:= PixPtr^* $10101;
          RGBTipleRowPtr^.rgbtBlue:= PixPtr^;
          RGBTipleRowPtr^.rgbtGreen:= PixPtr^;
          RGBTipleRowPtr^.rgbtRed:= PixPtr^;

          Inc (RGBTipleRowPtr);
          Inc (PixPtr);

        end;

      end;

    end;

    it24Bit:
    begin
      Result.PixelFormat:= pf24bit;
      Result.Height:= FRow;
      Result.Width:= FColumn;

      for r:= 0 to Row- 1 do
      begin
        RGBTipleRowPtr:= Result.ScanLine [r];
        PixPtr:= ScanLine [r];

        for c:= 0 to FColumn- 1 do
        begin
          RGBTipleRowPtr^.rgbtBlue:= (PixPtr^ and $FF0000) shr 16;
          RGBTipleRowPtr^.rgbtGreen:= (PixPtr^ and $00FF00) shr 8;
          RGBTipleRowPtr^.rgbtRed:= (PixPtr^ and $0000FF);
          Inc (RGBTipleRowPtr);
          Inc (PixPtr);

        end;

      end;

    end;

  end;

end;

function TColoredImage.GetBlackColor: Integer;
begin
  Result:= 0;
  
end;

function TColoredImage.GetHistogram (Color: Integer): Integer;
begin
  if FImageType= it8bit then
  begin
    if HistogramIsCalced then
      Result:= FHistogram [Color]
      
    else
    begin
      CalculateHistogram;
      Result:= FHistogram [Color]

    end;

  end
  else
  begin
    raise EInvalidImage.Create ('Image is an not it8bit image!');
    Result:= -1;
    
  end;
end;


function TColoredImage.GetWhiteColor: Integer;
begin
  if FImageType= it8bit then
    Result:= $FF
  else if FImageType= it24bit then
    Result:= $FFFFFF
  else
    raise EInvalidImage.Create ('ImageType must be it8bit and it24bit');
    

end;

function TColoredImage.GrayThreshold: Integer;
var
  k: Integer;
  Tot: Integer;
  n,
  Index: Integer;
  Max,
  mu_t: Extended;
  cum, mu, HistNorm,
  Sigma_b_Squard: array [0..255] of Extended;

begin
  if FImageType<> it8bit then
    raise EInvalidImage.Create ('');

  // Sum of Histogram
  Tot:= FRow* FColumn;

  if not HistogramIsCalced then
    CalculateHistogram;

  // Normal Histogram (0,1)
  for k:= 0 to 255 do
    HistNorm [k]:= Histogram [k]/ Tot;

  // Cumulative Normal Histogram & Cumulaive sum of (Normal Histogram * k)
  cum [0]:= HistNorm[0];
  mu [0]:= HistNorm[0];
  for k:= 1 to 255 do
  begin
    cum [k]:= cum [k- 1]+ HistNorm [k];
    mu [k]:= mu [k- 1]+ k* HistNorm [k];
    
  end;
  mu_t:= mu [255];

  // sigma_b_squared
  for k:= 0 to 255 do
    if cum [k]* (1- cum [k])< 1e-10 then
      sigma_b_squard [k]:= 1e10* (mu_t* cum [k]- mu [k])* (mu_t* cum [k]- mu [k])
    else
      sigma_b_squard [k]:= (mu_t* cum [k]- mu [k])* (mu_t* cum [k]- mu [k])
       / (cum [k]* (1- cum [k]));

  // Find mean of indexes that values is max
  Index:= 0;
  Max:= 0;
  n:= 0;
  for k:= 0 to 255 do
    if Max< Sigma_b_Squard [k]- 1e-10 then
    begin
      Max:= sigma_b_squard [k];
      Index:= k;
      n:= 1;

    end
    else if Abs (Max- Sigma_b_Squard [k])< 1e-10 then
    begin
      Inc (index, k);
      Inc (n);

    end;

  Index:= Index div n;

  Result:= Index;

end;

procedure TColoredImage.LoadBitmap (Bitmap: TBitmap; Pattern: Integer= 9999);
var
  r, c: Integer;
  PixPtr: PInteger;
  RowPtr: PByte;
//  RowPtrFor24bit: ^TRGBTriple;
//  Color: TColor;

begin
  Column:= Bitmap.Width;
  Row:= Bitmap.Height;

  if Bitmap.PixelFormat= pf24bit then
  begin
    if FImageType<> it24bit then
    begin
      WHITE:= GetWhiteColor;
      BLACK:= GetBlackColor;

    end;

    FImageType:= it24bit;
    for r:= 0 to FRow- 1 do
    begin
      RowPtr:= Bitmap.ScanLine [r];
      PixPtr:= ScanLine [r];

      for c:= 0 to FColumn- 1 do
      begin
        PixPtr^:= RowPtr^;
        Inc (RowPtr);
        PixPtr^:= PixPtr^ shl 8+ RowPtr^;
        Inc (RowPtr);
        PixPtr^:= PixPtr^ shl 8+ RowPtr^;

        Inc (RowPtr);
        Inc (PixPtr);

      end;

    end;

  end
  else if Bitmap.PixelFormat= pf8bit then
  begin
    if FImageType<> it8bit then
    begin
      WHITE:= GetWhiteColor;
      BLACK:= GetBlackColor;
      
    end;

    FImageType:= it8bit;
    
    for r:= 0 to FRow- 1 do
    begin
      RowPtr:= Bitmap.ScanLine [r];
      PixPtr:= ScanLine [r];

      for c:= 0 to FColumn- 1 do
      begin
        PixPtr^:= RowPtr^;

        Inc (RowPtr);
        Inc (PixPtr);

      end;

    end;

  end

end;

{ TBaseImage }

function TBaseImage.BlackPixCountInImage (ReCalc: Boolean): Integer;
var
  r, c: Integer;
  IntPtr: PInteger;
  Black: Integer;

begin
  Black:= GetBlackColor;
  
  Result:= 0;
  if ReCalc then
    Result:= 0;

  for r:= 0 to Row- 1 do
  begin
    IntPtr:= Self.ScanLine [r];
    
    for c:= 1 to Column do
    begin
      if IntPtr^= Black then
        Inc (Result);
      Inc (IntPtr);

    end;

  end;


end;

constructor TBaseImage.Create (ImageType: TImageType);
begin
  inherited Create;

  HistogramIsCalced:= False;
  FRow:= -1; FColumn:= -1; FPattern:= -1;
  FIsBlank:= mbUnSet;
  FCenterOfMass:= TPoint.Create;
  FImageType:= ImageType;
  
  WHITE:= GetWhiteColor;
  BLACK:= GetBlackColor;

end;

destructor TBaseImage.Destroy;
var
  i: Integer;

begin

  for i:= 0 to High (FBody) do
    SetLength (FBody [i], 0);
  SetLength (FBody, 0);

  if FCenterOfMass<> nil then
    FCenterOfMass.Free;

  inherited;
  
end;

function TBaseImage.GetBodyArray: TArrayofArrayofInt;
begin
  Result:= FBody;
  
end;

function TBaseImage.GetBodyColor (r, c: Integer): Integer;
begin
  if (r< FRow) and (0<= r) and
      (c< FColumn) and (0<= c) then
     Result:= FBody [r, c]

  else
    raise ERangeCheckError.Create ('Index out of range!');

end;

function TBaseImage.GetCenterOfMass(Recalc: Boolean): TPoint;
var
  r, c, BlackCount: Integer;
  x, y: Double;
  LinePtr: PInteger;

begin
  Result:= TPoint.Create (0, 0);
  x := 0; y := 0; BlackCount := 0;
  for r:= 0 to Row - 1 do
  begin
    LinePtr:= ScanLine [r];

    for c:= 0 to FColumn- 1 do
    begin
      if LinePtr^= BLACK then
      begin
        x:= x+ c+ 0.5;
        y:= y+ r+ 0.5;
        Inc (BlackCount);
        
      end;
      Inc (LinePtr);

    end;
    
  end;
  x:= x / BlackCount;
  y:= y / BlackCount;
  Result.c:= Round (x);
  Result.r:= Round (y);

end;

function TBaseImage.GetPattern: Integer;
begin
  if FPattern>= 0 then
    Result:= FPattern
  else
    raise EFMLImageNotInitialized.Create (IntToStr (FPattern));

end;

function TBaseImage.GetScanLine (RowIndex: Integer): PInteger;
begin
  if Row<= RowIndex then
    raise ERangeCheckError.Create ('GetScanLine'+ IntToStr (Row)+ ':'+ IntToStr (RowIndex));

  Result:= @FBody [RowIndex, 0];

end;

procedure TBaseImage.LoadBitMap (FileName: String);
var
  Bitmap: TBitMap;
  
begin
  Bitmap:= TBitmap.Create;
  
  Bitmap.LoadFromFile (FileName);
  LoadBitMap (Bitmap);
  
  Bitmap.Free;
  
end;

procedure TBaseImage.SaveAsBitmap (FileName: string);
var
  TempImage: Graphics.TBitmap;
  
begin
  TempImage:= GetAsBitmap;
  TempImage.SaveToFile (FileName);
  TempImage.Free;
  
end;

procedure TBaseImage.SetColumn (const Value: Integer);
var
  r: Integer;
  
begin
  FColumn:= Value;

  if 0< FRow then
  begin
    SetLength (FBody, FRow);
    for r:= 0 to FRow- 1 do
      SetLength (FBody [r], FColumn);

  end;

end;

procedure TBaseImage.SetImageKind (const Value: TInputKind);
begin
  FImageKind:= Value;
  
end;

procedure TBaseImage.SetPattern (const Value: Integer);
begin
  FPattern := Value;

end;

procedure TBaseImage.SetRow (const Value: Integer);
var
  r: Integer;
  
begin
  FRow:= Value;
  
  if 0< FColumn then
  begin
    SetLength (FBody, FRow);
    for r:= 0 to FRow- 1 do
      SetLength (FBody [r], FColumn);
      
  end;

end;

function TFMLImage.GetBlackColor: Integer;
begin
  Result:= 1;
  
end;

function TFMLImage.GetWhiteColor: Integer;
begin
  Result:= 0;

end;

function TBaseImage.IsHorizentalLineBlack (RowIndex, LeftCol, RightCol,
  Heigth: Integer): Real;
var
  BlackPixCount,
  i, j: Integer;
  PixPtr: PInteger;

begin

  BlackPixCount:= 0;

  for i:= RowIndex to RowIndex+ Heigth- 1 do
  begin
    PixPtr:= ScanLine [i];

    Inc (PixPtr, LeftCol);
    for j:= LeftCol to RightCol do
    begin

      if PixPtr^= Black then
        Inc (BlackPixCount)
      else
        Inc (BlackPixCount, 0);
        
      Inc (PixPtr);
      
    end;

  end;

  Result:= BlackPixCount/ (Heigth* (RightCol- LeftCol+ 1));
  
end;

function TBaseImage.IsVerticalLineBlack(ColIndex, TopRow, BotRow,
  Width: Integer): Real;
var
  BlackPixCount,
  r, c: Integer;
  PixPtr: PInteger;

begin

  BlackPixCount:= 0;

  for r:= TopRow to BotRow do
  begin
    PixPtr:= ScanLine [r];

    Inc (PixPtr, ColIndex);
    for c:= ColIndex to ColIndex+ Width- 1 do
    begin

      if PixPtr^= Black then
        Inc (BlackPixCount);
        
      Inc (PixPtr);
      
    end;

  end;

  Result:= BlackPixCount/ (Width* (BotRow- TopRow+ 1));
  
end;

destructor TFMLImage.Destroy;
begin

  inherited;

end;

function TFMLImage.Copy(TL, BR: TPoint): TFMLImage;
begin
  Result:= inherited CopyPixels (TL, BR) as TFMLImage;

end;

function TBaseImage.CopyPixels (TL, BR: TPoint): TBaseImage;
var
  r: Integer;
  Newr: Integer;
  SourcePixPtr,
  TargetPixPtr: PInteger;
  Sr, Er, Sc, Ec: Integer;

begin

  Result:= NewInstance;
  Sr:= Max (0, TL.r);
  Er:= Min (BR.r, FRow- 1);
  Sc:= Max (0, TL.c);
  Ec:= Min (BR.c, FColumn- 1);

  Result.Row:= Er- Sr+ 1;
  Result.Column:= Ec- Sc+ 1;

  Newr:= 0;

  for r:= Sr to Er do
  begin
    SourcePixPtr:= ScanLine [r];
    TargetPixPtr:= Result.ScanLine [Newr];

    Inc (SourcePixPtr, Sc);
    Move (SourcePixPtr^, TargetPixPtr^, SizeOf (Integer)* (Result.FColumn));

{
    for c:= Sc to Ec do
    begin
      TargetPixPtr^:= SourcePixPtr^;
      Inc (SourcePixPtr);
      Inc (TargetPixPtr);

    end;
    }
    Inc (Newr);

  end;

end;

function TFMLImage.NewInstance: TBaseImage;
begin
  Result:= TFMLImage.Create (itMonoChrome);
  
end;

function TColoredImage.NewInstance: TBaseImage;
begin
  Result:= TColoredImage.Create (FImageType);
  
end;

end.
