unit FMLImage;
(*$DEFINE REVERSE_MODE*)
(*$DEFINE DEBUG_MODE*)

interface
uses
  {System.IO, }Graphics, HashUnit, SysUtils,
    FeatureUnit, CollectionUnit;
    
const BLACK: Integer= 1;//??!!
const WHITE: Integer= 0;//??!!
const UnknownPattern: Integer= 9999;
type
  EFileNotFound= class (Exception);

  TByteFile= file of Byte;
  TInputKind= (ikNumeral, ikAlphabet, ikCheckBox, ikPicture, ikHelpBar);
  TMyBoolean= (mbTrue, mbFalse, mbUnSet);

  THSI= record
    h, i: Integer;
    s: Extended;
  end;

  TRGB= record
    r, g, b: Byte;
    Color: TColor;
  end;

  TMyPixel= class
  private
    FLocation: TPoint;
    FRGBColor: TRGB;
  public
    function GetHSIColor: THSI;
    procedure SetLocation (const Value: TPoint);
    property Location: TPoint read FLocation write SetLocation;
    property RGBColor: TRGB read FRGBColor write FRGBColor;
    property HSIColor: THSI read GetHSIColor;

    function ToString: String;
    constructor Create;overload;
    constructor Create (Point: TPoint; Color: TRGB);overload;
    procedure Free;
  end;

  PComponent= ^TComponent;
  TComponent= class
  private
    Pixels: array of TMyPixel;
    FMaxX, FMaxY: Integer;
    FMinX, FMinY: Integer;

    SumOfR, SumOfG, SumOfB: Integer;
    FCollectionColor: TRGB;

    LeftestPoint, RightestPoint,
    TopestPoint, MostBottonPoint: Integer;
    FID: Integer;
    FCenterOfMass: TPoint;

    function GetIndex (x, y: Integer): Integer;
    function GetCount: Integer;
    procedure FindBoundraryPoints;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPercentage: Integer;

  public
    HashedData: THash;
    property ID: Integer read FID;
    property Count: Integer read GetCount;
    property CollectionColor: TRGB read FCollectionColor;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Percentage: Integer read GetPercentage;
    property MinX: Integer read FMinX;
    property MaxX: Integer read FMaxX;
    property MinY: Integer read FMinY;
    property MaxY: Integer read FMaxY;
    property CenterOfMass: TPoint read FCenterOfMass;

    constructor Create (ID: Integer= -1);
    procedure Free;

    function IsExists (x, y: Integer): Boolean;overload;
    function IsExists (Point: TPoint): Boolean;overload;
    procedure Add (x, y: Integer);overload;
    procedure Add (x, y: Integer; RGBColor: TRGB);overload;
    procedure Add (Point: TPoint; RGBColor: TRGB);overload;
    procedure Delete (x, y: Integer);overload;
    procedure Delete (Index: Integer);overload;
    function  GetPixel: TMyPixel;overload;
    function  GetPixel (Index: Integer): TMyPixel;overload;
    function  CountInSameLine: Extended;

    procedure SaveToFile (FileName: String);
    procedure AddToFile (var FileHandle: TextFile);

    function GetTopLeftPoint: TPoint;
    function GetTopRightPoint: TPoint;
    function GetDownLeftPoint: TPoint;
    function GetDownRightPoint: TPoint;

    function GetMinimum: TPoint;
    function GetMaximum: TPoint;
    function GetEffectiveLength: Integer;
    function GetEffectiveWidth: Integer;
    function GetArea: Integer;

    function ExtractContour: TComponent;
    procedure Merge (AnotherComponent: TComponent; MaybeTheSame: Boolean= False);

    function FindMinDistToComp (AnotherComponnt: TComponent): Extended;
    function FindMinDistToPoint (APoint: TPoint): Extended;

  end;

  TFMLImage= class;
  TImageCollection= class;
  
  TComponentCollection= class
  private
    FComponents: array of TComponent;

    function GetComponent (Index: Integer): TComponent;
    function GetCompCount: Integer;
    function GetMaxPoint: TPoint;
    function GetMinPoint: TPoint;
    function GetComponentPointer: PComponent;
  public
    property Count: Integer read GetCompCount;
    property Component [Index: Integer]: TComponent read GetComponent;
    property MinPoint: TPoint read GetMinPoint;
    property MaxPoint: TPoint read GetMaxPoint;
    property ComponentPointer: PComponent read GetComponentPointer;

    constructor Create;
    procedure Free (FreeObj: Boolean= True);

    procedure AddComponent (Component: TComponent);
    function IsExists (x, y: Integer): Boolean;
    procedure RemoveInvalidComponents;
    procedure DeleteComponent (Index: Integer; FreeIt: Boolean= False);
    
  end;
  TImageType= (itMonoChrome, it8bit, it24bit, it32Bit, itNone);

  TFMLImage= class
  private
    FHistogram: array [0..255] of Integer;
    FBody: array of array of Integer;
    FRow, FColumn, FPattern: Integer;
    FImageKind: TInputKind;
    FIsBlank: TMyBoolean;
    HistogramIsCalced: Boolean;
    FImageType: TImageType;

    procedure CalculateHistogram;
    function GetColumn: Integer;
    function GetHistogram (Color: Integer): Integer;
    function GetPattern: Integer;
    function GetRow: Integer;
    procedure SetColumn (const Value: Integer);
    procedure SetRow (const Value: Integer);
    function GetBodyColor (r, c: Integer): Integer;

    function DeleteImage (NoiseColor: TColor; NoiseThr: Integer): TFMLImage;
    function FindAllComponents: TComponentCollection;
    function GetScanLine(RowIndex: Integer): PInteger;
    function DoSmooth: TFMLImage;
    function DoSmooth1: TFMLImage;
    function DoSmooth2: TFMLImage;

    function ApplySobelAndGetGradiantIn8Dir: T8DirGradiantFeature;
    function GetSampleGradiantFor8Dir: TSampleGradiantIn8Dir;

    procedure SetImageKind (const Value: TInputKind);
    procedure LoadJpeg (FileName: string);
    
  public
    property ImageType: TImageType read FImageType;
    property Pattern: Integer read GetPattern ;
    property Row: Integer read GetRow write SetRow;
    property Column: Integer read GetColumn write SetColumn;
    property Histogram [Color: Integer]: Integer read GetHistogram;
    property Body [r, c: Integer]: Integer read GetBodyColor;
    property Kind: TInputKind read FImageKind write SetImageKind;
    property IsBlank: TMyBoolean read FIsBlank;
    property ScanLine [RowIndex: Integer]: PInteger read GetScanLine;

    constructor Create;overload;
    constructor Create (PixelCollection: TComponent);overload;
    constructor Create (ComponentCollection: TComponentCollection);overload;
    procedure Free;

    procedure SaveInFMLFile (var OutputFile: TByteFile);
    procedure SaveAsBitmap (FileName: string);
    procedure SaveAsText (FileName: String; PrintBody: Boolean= False);

    procedure LoadPhotoCopiedImage (FileName: string); overload;
    procedure LoadPhotoCopiedImage (Bitmap: TBitmap); overload;

    procedure LoadBitMap (FileName: string);overload;
    procedure LoadBitMap (Bitmap: TBitmap; Pattern: Integer= 9999);overload;
    procedure LoadRGB256Bitmap (Bitmap: TBitmap);
    procedure LoadFromFMLFile (var InputFile: TByteFile); overload;
//    procedure LoadFromFMLFile (InputFileName: String; Index: Integer= 0); overload;

    function GetAsBitmap (MonoChrome: Boolean= true): TBitmap;
    function ConvertToGrayScale (Bitmap: TBitmap): TBitmap;
    function GrayThreshold: Integer;
    function ConvertToBinary (Bitmap: TBitmap): TBitmap;
    function MixImage (ImagePixels: TComponent): TFMLImage;
    procedure SetPixelBlack (r, c: Integer); overload;
    procedure SetPixelBlack (Point: TPoint); overload;

    function FindAllComponentsInBox (TopLeft, BottomRight: TPoint;
          UseDialateBeforeExtraction: Boolean= True): TComponentCollection;
    function FindAllComponentsHavePointInBox (TopLeft, BottomRight: TPoint): TComponentCollection;

    function Dilate: TFMLImage;
    function Erode: TFMLImage;

    procedure Write (PrintToFile: Boolean= False; FileName: String= '');
    procedure Add (Component: TComponent);
    function Copy (TopLeft, BottomRight: TPoint): TFMLImage;

    function IsVerticalLineBlack (ColIndex: Integer; TopRow, BotRow: Integer;
      Width: Integer= 1): Real;
    function IsHorizentalLineBlack (RowIndex: Integer; LeftCol, RightCol: Integer;
      Heigth: Integer= 1): Real;

    function Resize (NewRow, NewColumn: Integer): TFMLImage;
    function Smooth (RepeatCount: Integer): TFMLImage;
    function ExtractFeatures (NewSize: Integer; SmoothDegree: Integer;
       NumberOfMasks: Integer= 5): TFeatureVectorBasedOnGradiant;
    procedure DeleteRow (Index: Integer);
    procedure DeleteRowsInRange (TopIndex, BotIndex: Integer);

    procedure DeleteVerticalBlackLine (Percentage: Extended= 1/2);
    procedure DeleteHorizentalBlackLine (Percentage: Extended= 1/2);

  end;

  TImageCollection= class (TObject)
    private
      MinImageProperties: TPoint;
      MaxImageProperties: TPoint;
      PointsInImage: TPoint;//x is for min and y is for max

      FImages: array of TFMLImage;

      function GetImageNumber: Integer;
      function GetImage (Index: Integer): TFMLImage;

    public
      property ImageNumber: Integer read GetImageNumber;
      property Size: Integer read GetImageNumber;
      property Image [Index: Integer]: TFMLImage read GetImage;

      procedure LoadFromFMLFile (FileName: string);
      procedure SaveToFile (FileName: string);
      procedure SaveFilesAsBitmap (BaseFileName: String);
      procedure AddImage (Image: TFMLImage);
      procedure Dilate;

      constructor Create; overload;
      procedure Free (FreeObj: Boolean= True);

      function ExtractAllImagesFeatures (NewSize, SmoothDegree: Integer): TFeatureVectorBasedOnGradiantCollection;
      function GetAllWithPattern (PatternIndex: Integer): TImageCollection;
      
  end;

implementation
uses
  FormInformationUnit,
  Math, ExceptionUnit, TypInfo, VectorUnit, Jpeg;
  
const
  BlackRGB: TRGB= (r: 0; g: 0; b: 0; Color: $000);
  
type
  EFMLImageNotInitialized= class (Exception);
  ERangeCheckError= class (Exception);

  TArrArrInt= array of array of Integer;
  TArrInt= array of Integer;

procedure TFMLImage.CalculateHistogram;
var
  i,
  r, c: Integer;
begin
  for i:= 0 to 255 do
    FHistogram [i]:= 0;
    
  for r:= 0 to Row- 1 do
    for c:= 0 to Column- 1 do
      Inc (FHistogram [Body [r, c]]);
      
  HistogramIsCalced:= True;
  
end;

constructor TFMLImage.Create;
begin
  inherited Create;
  
  HistogramIsCalced:= False;
  FRow:= -1; FColumn:= -1; FPattern:= -1;
  FIsBlank:= mbUnSet;

end;

function TFMLImage.GetAsBitmap (MonoChrome: Boolean): TBitmap;
var
  Temp: Byte;
  i, cIndex,
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

  Result.Height:= Row;
  Result.Width:= Column;
  Result.PixelFormat:= pf1bit;
  Result.Monochrome:= MonoChrome;


  for r:= 0 to Row- 1 do
  begin
    RowPtr:= Result.ScanLine [r];

    PixPtr:= @FBody [r, 0];
    Temp:= 0;

    CIndex:= 0;
    for c:= 0 to (Column+ 7) div 8- 1 do
    begin
      for i:= 0 to 7 do
      begin
        case FImageType of
          itMonoChrome:
            if (PixPtr^= WHITE) then
              Temp:= Temp or (1 shl (7- i));
              
          it8bit:
            if (127< PixPtr^) then
            else
              Temp:= Temp or (1 shl (7- i));

           else
             raise ENotImplemented.Create ('GetAsBitmap');
        end;

         Inc (cIndex);
         Inc (PixPtr);
         
         if FColumn<= cIndex then
           Break;

      end;

//     Result.Canvas.Pixels [r, c]:= $FF;
      RowPtr^:= Temp;
      Inc (RowPtr);
      Temp:= 0;
      
    end;

  end;

end;

function TFMLImage.GetBodyColor (r, c: Integer): Integer;
begin
  if (r< Row) and (r>= 0) and
      (c< Column) and (c>= 0) then
     Result:= FBody [r, c]

  else
    raise ERangeCheckError.Create ('Index out of range!');

end;

function TFMLImage.GetColumn: Integer;
begin
  if FColumn> 0 then
    Result:= FColumn
  else
    raise EFMLImageNotInitialized.Create ('');
end;

function TFMLImage.GetHistogram (Color: Integer): Integer;
begin
  if HistogramIsCalced then
    Result:= FHistogram [Color]
    
  else
  begin
    CalculateHistogram;
    Result:= FHistogram [Color]

  end;

end;

function TFMLImage.GetPattern: Integer;
begin
  if FPattern>= 0 then
    Result:= FPattern
  else        
    raise EFMLImageNotInitialized.Create (IntToStr (FPattern));
    
end;

function TFMLImage.GetRow: Integer;
begin
  if FRow> 0 then
    Result:= FRow
  else
    raise EFMLImageNotInitialized.Create ('');
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
 
procedure TFMLImage.LoadBitMap (FileName: string);
var
  Bitmap: TBitMap;
begin
  Bitmap:= TBitmap.Create;
  
  Bitmap.LoadFromFile (FileName);
  LoadBitMap (Bitmap);
  
  Bitmap.Free;
end;

procedure TFMLImage.LoadBitMap (Bitmap: TBitmap; Pattern: Integer);
var
  r, c, i: Integer;
  RowPtr: PByte;
  cIndex: Integer;
  
begin
  Row:= Bitmap.Height;
  Column:= Bitmap.Width;
  
  if (Bitmap.PixelFormat= pf24bit) or (Bitmap.PixelFormat= pfDevice) then
  begin
    raise ENotImplemented.Create ('');
{    TempBitmap1:= Self.ConvertToGrayScale (Bitmap);
    TempBitmap2:= Self.ConvertToBinary (TempBitmap1);
    TempBitmap1.Free;
    }
  end
  else if Bitmap.PixelFormat= pf8bit then
  begin
    FImageType:= it8bit;
    
    for r:= 0 to FRow- 1 do
    begin
      RowPtr:= Bitmap.ScanLine [r];

      for c:= 0 to FColumn- 1 do
      begin
        FBody [r, c]:= RowPtr^;
        Inc (RowPtr);

      end;
      
    end;

  end
  else if Bitmap.PixelFormat= pf1bit then
  begin
    FImageType:= itMonoChrome;
    
    for r:= 0 to FRow- 1 do
    begin
      RowPtr:= Bitmap.ScanLine [r];

      cIndex:= 0;
      for c:= 0 to (FColumn- 1) div 8 do
      begin
        for i:= 0 to 7 do
        begin

(*$IFDEF REVERSE_MODE*)          
          if (RowPtr^ shr (7- i)) and 1= 1 then
            FBody [r, cIndex]:= WHITE
          else
            FBody [r, cIndex]:= BLACK;
(*$ELSE*)
          if (RowPtr^ shr (7- i)) and 1= 1 then
            FBody [r, cIndex]:= BLACK
          else
            FBody [r, cIndex]:= WHITE;
(*$ENDIF*)

          Inc (cIndex);
          if cIndex= FColumn then
            Break;

        end;

        Inc (RowPtr);

      end;
      
    end;
  end
  else
    raise Exception.Create ('Invalid filetype!');

{  raise Exception.Create ('Invalid filetype!');
  for r:= 0 to FRow- 1 do
    for c:= 0 to FColumn- 1 do
      if TempBitmap2.Canvas.Pixels [c, r]= clWhite then
        FBody [r][c]:= WHITE
      else
        FBody [r][c]:= BLACK;

  TempBitmap2.Free;
  }
  FPattern:= Pattern;
  
end;

procedure TFMLImage.SaveAsBitmap (FileName: string);
var
  TempImage: TBitmap;
  
begin
  TempImage:= GetAsBitmap (True);
  TempImage.SaveToFile (FileName);
  TempImage.Free;
  
end;

procedure TFMLImage.SetColumn (const Value: Integer);
var
  r, c: Integer;
  LastValue: Integer;

begin
  LastValue:= FColumn;
  FColumn:= Value;
  
  if FRow>0 then
  begin
    SetLength (FBody, FRow);
    for r:= 0 to FRow- 1 do
    begin
    
      SetLength (FBody [r], FColumn);
      for c:= LastValue to FColumn- 1 do
        FBody [r, c]:= WHITE;
        
    end;
      
  end;
  
end;

procedure TFMLImage.SetImageKind (const Value: TInputKind);
begin
  FImageKind:= Value;
  
end;

procedure TFMLImage.SetRow (const Value: Integer);
var
  r, c: Integer;
  
begin
  FRow:= Value;
  if FColumn>0 then
  begin
    SetLength (FBody, FRow);
    for r:= 0 to FRow- 1 do
    begin
      SetLength (FBody [r], FColumn);
      for c:= 0 to FColumn- 1 do
        FBody [r, c]:= WHITE;
      
    end;
    
  end;
  
end;

function TFMLImage.ConvertToGrayScale (Bitmap: TBitmap): TBitmap;
const
  Matrix: array [0..2] of Extended= (0.299, 0.587, 0.114);
  
var
  i, j, 
  Temp,
  Red, Blue, Green,
  Width, Height: Integer;
  
begin
  if (Bitmap.PixelFormat<> pf24bit) and ( (Bitmap.PixelFormat<> pfDevice)) then
    raise Exception.Create ('Invalid Bitmap File!');

  Result:= TBitmap.Create ();

  Result.Width:= Bitmap.Width;
  Result.Height:= Bitmap.Height;
  Result.PixelFormat:= pf8bit;

  Width:= Bitmap.Width;
  Height:= Bitmap.Height;

  for i:= 0 to 255 do
    FHistogram [i]:= 0;

  for i:= 0 to Width- 1 do
    for j:= 0 to Height- 1 do
    begin
      Red:= Bitmap.Canvas.Pixels [i, j] and $FF;
      Green:= (Bitmap.Canvas.Pixels [i, j] and $FF00) shr 8;
      Blue:= (Bitmap.Canvas.Pixels [i, j] and $FF0000) shr 16;

      Temp:= Round (Matrix [0]* Red+ Matrix [1]* Green+ Matrix [2]* Blue);
          
      Inc (FHistogram [Temp]);
      Result.Canvas.Pixels [i, j]:= Temp* $10101;
    end;
//    Result.SaveToFile ('a.bmp');
end;

function TFMLImage.GrayThreshold: Integer;
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

function TFMLImage.ConvertToBinary (Bitmap: TBitmap): TBitmap;
var
  IsCreated: Boolean;
  NewBitmap: TBitmap;
  i, j: Integer;
  GrayThresh: Integer;
  
begin
  IsCreated:= False;

  if Bitmap.PixelFormat= pf24bit then
  begin
    IsCreated:= True;
    NewBitmap:= ConvertToGrayScale (Bitmap);
    
  end
  else
    NewBitmap:= Bitmap;

  Result:= TBitmap.Create;
  Result.Monochrome:= true;

  Result.Width:= NewBitmap.Width;
  Result.Height:= NewBitmap.Height;

  GrayThresh:= GrayThreshold* $10101;

  for i:= 0 to Result.Width- 1 do
    for j:= 0 to Result.Height- 1 do
    begin
        if NewBitmap.Canvas.Pixels [i, j]< GrayThresh then
          Result.Canvas.Pixels [i, j]:= clBlack
        else
          Result.Canvas.Pixels [i, j]:= clWhite;
          
    end;
    
  if IsCreated then
    NewBitmap.Free;
    
end;

{ TImageCollection }

procedure TImageCollection.AddImage (Image: TFMLImage);
begin
  SetLength (FImages, Length (FImages)+ 1);
  FImages [High (FImages)]:= Image;
  
end;

constructor TImageCollection.Create;
begin
  inherited;
  SetLength (FImages, 0);
  
end;

procedure TImageCollection.Dilate;
var
  i: Integer;
  
begin
  for i:= 0 to ImageNumber- 1 do
    FImages [i].Dilate;

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

procedure TImageCollection.Free (FreeObj: Boolean= True);
var
  i: Integer;
  
begin
  if FreeObj then
    for i:= 0 to ImageNumber- 1 do
      FImages [i].Free;
      
  SetLength (FImages, 0);

  inherited Free; 
  
end;

function TImageCollection.GetImage (Index: Integer): TFMLImage;
begin
  if (0<= Index) and (Index< Length (FImages)) then
    Result:= FImages [Index]
  else
    raise ERangeCheckError.Create ('Invalid Index for Image!');
    
end;

function TImageCollection.GetImageNumber: Integer;
begin
  Result:= Length (FImages);
  
end;

procedure TImageCollection.SaveToFile (FileName: string);
var
  i: Integer;
  OutputFile: TByteFile;
  b1, b2: Byte;
//  NewImage: TFMLImage;
  
begin
  AssignFile (OutputFile, FileName);
  Rewrite (OutputFile);
  b1:= ImageNumber and 255;
  b2:= ImageNumber shr 8;
  Write (OutputFile, b1, b2);
  
  for i:= 0 to ImageNumber- 1 do
    Image [i].SaveInFMLFile (OutputFile);

  CloseFile (OutputFile);

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

function TFMLImage.Dilate: TFMLImage;
const
  AdjRow: array [0..8] of Integer= (-1, -1, -1, 0, 0, 0, +1, +1, +1);
  AdjCol: array [0..8] of Integer= (+1,  0, -1, +1, 0, -1, +1, 0, -1);
var
//  AllWhite: array of Integer;
  NewBody: array of array of Integer;
  NewBodyPtr: PInteger;
  r, c,
  i: Integer;
  
begin
  Result:= Self;
  if FImageType<> itMonoChrome then
    raise EInvalidImage.Create ('');
    

  if IsBlank= mbTrue then
    Exit;
    
  SetLength (NewBody, Row+ 2);
  for r:= 0 to Row+ 1 do
  begin
    SetLength (NewBody [r], Column+ 2);
    NewBodyPtr:= @NewBody [r, 0];
    
    for c:= 0 to FColumn+ 1 do
    begin
      NewBodyPtr^:= WHITE;
      Inc (NewBodyPtr);
      
    end;

  end;

  for r:= 0 to Row- 1 do
    for c:= 0 to Column- 1 do
      if Body [r, c]=  BLACK then
      begin
        for i:= 0 to 8 do
          NewBody [r+ 1+ AdjRow [i]][c+ 1+ AdjCol [i]]:= BLACK;
      end;

  SetLength (FBody, Row+ 2);
  for r:= 0 to Row+ 1 do
    SetLength (FBody [r], Column+ 2);

  Inc (FRow, 2);
  Inc (FColumn, 2);

  for r:= 0 to Row- 1 do
    for c:= 0 to Column- 1 do
      FBody [r, c]:= NewBody [r][c];
      
  for r:= 0 to Row- 1 do
    SetLength (NewBody [r], 0);
    
  SetLength (NewBody, 0);

end;

function TFMLImage.Erode: TFMLImage;
const
  AdjRow: array [0..8] of Integer= (-1, -1, -1, 0, 0, 0, +1, +1, +1);
  AdjCol: array [0..8] of Integer= (+1,  0, -1, +1, 0, -1, +1, 0, -1);
  
var
  NewBody: array of array of Integer;
  S, r, c,
  i, j: Integer;

begin
  SetLength (NewBody, Row- 2);

  for i:= 0 to Row- 3 do
  begin
    SetLength (NewBody [i], Column- 2);
    for j:= 0 to Column- 3 do
      NewBody [i][j]:= White;
  end;

  for r:= 1 to Row- 2 do
    for c:= 1 to Column- 2 do
    begin
      S:= 0;
      for i:= 0 to 8 do
        if (Body [r+ AdjRow [i], c+ AdjCol [i]]= BLACK) then
          Inc (S);
      if S= 9 then
        NewBody [r- 1][c- 1]:= BLACK
      else
        NewBody [r- 1][c- 1]:= WHITE;
    end;


  for r:= 0 to Row- 1 do
    SetLength (FBody [r], 0);
  SetLength (FBody, 0);

  Dec (FRow, 2);
  Dec (FColumn, 2);
  SetLength (FBody, Row);
  for r:= 0 to Row- 1 do
  begin
    SetLength (FBody [r], Column);
    for c:= 0 to Column- 1 do
      FBody [r][c]:= NewBody [r][c];
  end;

  Result:= Self;

  for r:= 0 to Row- 1 do 
    SetLength (NewBody [r], 0);
  SetLength (NewBody, 0);
  
end;

function TFMLImage.DeleteImage (NoiseColor: TColor; NoiseThr: Integer): TFMLImage;
begin
  raise ENotImplemented.Create ('TFMLImage.DeleteImage');
  
end;

constructor TFMLImage.Create (PixelCollection: TComponent);
var
  r, c,
  i: Integer;
  MinPoint, MaxPoint: TPoint;
  Pixel: TMyPixel;
begin
  inherited Create;
  
  HistogramIsCalced:= False;
  FRow:= -1; FColumn:= -1; FPattern:= -1;
  FIsBlank:= mbUnSet;

  MinPoint:= PixelCollection.GetMinimum;
  MaxPoint:= PixelCollection.GetMaximum;

  Row:= MaxPoint.y- MinPoint.y+ 1;
  Column:= MaxPoint.x- MinPoint.x+ 1;

  for r:= 0 to Row- 1 do
    for c:= 0 to Column- 1 do
      FBody [r, c]:= WHITE;

  for i:= 0 to PixelCollection.Count- 1 do
  begin
    Pixel:= PixelCollection.GetPixel (i);
    FBody [Pixel.FLocation.y- MinPoint.y, Pixel.Location.x- MinPoint.x]:= BLACK;
    
  end;
  
  MinPoint.Free; MaxPoint.Free;
  
end;

constructor TFMLImage.Create (ComponentCollection: TComponentCollection);
var
  PixelCollection: TComponent;
  i, j: Integer;
  MinPoint, MaxPoint: TPoint;
  Pixel: TMyPixel;
begin
  inherited Create;

  HistogramIsCalced:= False;
  FRow:= 0; FColumn:= 0; FPattern:= -1;

  if ComponentCollection.Count= 0 then
  begin
    FIsBlank:= mbTrue;
    Exit;
  end
  else
    FIsBlank:= mbFalse;

  MinPoint:= ComponentCollection.GetMinPoint;
  MaxPoint:= ComponentCollection.GetMaxPoint;

  Row:= MaxPoint.y- MinPoint.y+ 1;
  Column:= MaxPoint.x- MinPoint.x+ 1;
  MaxPoint.Free;

  for i:= 0 to ComponentCollection.Count- 1 do
  begin
    PixelCollection:= ComponentCollection.Component [i];

    for j:= 0 to PixelCollection.Count- 1 do
    begin
      Pixel:= PixelCollection.GetPixel (j);
      FBody [Pixel.Location.y- MinPoint.y, Pixel.FLocation.x- MinPoint.x]:= BLACK;
//          Pixel.RGBColor.Color;
    end;
  end;
  MinPoint.Free; 
end;

procedure TImageCollection.SaveFilesAsBitmap (BaseFileName: String);
var
  i: Integer;
  
begin
  for i:= 0 to ImageNumber- 1 do
    FImages [i].SaveAsBitmap (BaseFileName+ IntToStr (i)+ '.bmp');
    
end;

procedure TImageCollection.LoadFromFMLFile (FileName: string);
var
  i: Integer;
  InputFile: TByteFile;
  b1, b2: Byte;
  ImageNo: Integer;
  NewImage: TFMLImage;
  
begin
  AssignFile (InputFile, FileName);
  Reset (InputFile);
  Read (InputFile, b1, b2);
  ImageNo:= b1+ 256* b2;
  SetLength (FImages, 0);
  
  for i:= 1 to ImageNo do
  begin
    NewImage:= TFMLImage.Create;
    NewImage.LoadFromFMLFile (InputFile);

    AddImage (NewImage);
    
  end;


  CloseFile (InputFile);

end;

function TImageCollection.GetAllWithPattern(
  PatternIndex: Integer): TImageCollection;
var
  i: Integer;

begin
  Result:= TImageCollection.Create;

  for i:= 0 to Size- 1 do
    if Image [i].Pattern= PatternIndex then
      Result.AddImage (Image [i]);

end;

{ TMyPixel }

constructor TMyPixel.Create;
begin
  inherited;
  
  Location:= nil;
end;

constructor TMyPixel.Create (Point: TPoint; Color: TRGB);
begin
  inherited Create;
  
  Location:= Point;
  RGBColor:= Color;

end;

procedure TMyPixel.Free;
begin
  FLocation.Free;

  inherited;
  
end;

function TMyPixel.GetHSIColor: THSI;
begin
  raise ENotImplemented.Create ('');
//  Result:= Conver
end;

procedure TMyPixel.SetLocation (const Value: TPoint);
begin
  FLocation:= Value;
end;

{ TPixelCollection }

procedure TComponent.Add (x, y: Integer; RGBColor: TRGB);
begin
  if IsExists (x, y) then
    Exit;
    
  SetLength (Pixels, Length (Pixels)+ 1);
  Pixels [High (Pixels)]:= TMyPixel.Create (TPoint.Create (x, y), RGBColor);
  
  FCenterOfMass.x:= FCenterOfMass.x+ x;
  FCenterOfMass.y:= FCenterOfMass.y+ y;

  HashedData.Insert (x, y);

  if x< MinX then
    FMinX:= x;
  if MaxX< x then
    FMaxX:= x;

  if y< Miny then
    FMinY:= y;
  if MaxY< y then
    FMaxY:= y;
    
  Inc (SumOfR, RGBColor.r);
  Inc (SumOfG, RGBColor.g);
  Inc (SumOfB, RGBColor.b);
  
  FCollectionColor.r:= SumOfR div Count;
  FCollectionColor.g:= SumOfG div Count;
  FCollectionColor.b:= SumOfB div Count;
  
end;

procedure TComponent.Add (Point: TPoint; RGBColor: TRGB);
begin
  Add (Point.x, Point.y, RGBColor);
  
end;

procedure TComponent.Add (x, y: Integer);
begin
  if IsExists (x, y) then
    Exit;
    
  SetLength (Pixels, Length (Pixels)+ 1);
  Pixels [High (Pixels)]:= TMyPixel.Create (TPoint.Create (x, y), BlackRGB);
  
  FCenterOfMass.x:= FCenterOfMass.x+ x;
  FCenterOfMass.y:= FCenterOfMass.y+ y;

  HashedData.Insert (x, y);

  if x< MinX then
    FMinX:= x;
  if MaxX< x then
    FMaxX:= x;

  if y< Miny then
    FMinY:= y;
  if MaxY< y then
    FMaxY:= y;
    
end;

procedure TComponent.AddToFile (var FileHandle: TextFile);
var
  i: Integer;
  
begin
  WriteLn (FileHandle, CollectionColor.r, ' ',
      CollectionColor.g, ' ',
      CollectionColor.b, ' ');
       
  for i:= 0 to Count- 1 do
    WriteLn (FileHandle, Pixels [i].ToString);
  
end;

function TComponent.CountInSameLine: Extended;
var
  SameLineCount,
  i: Integer;
begin
  SameLineCount:= 0;

  for i:= 0 to Count- 2 do
  begin
    if (Pixels [i].FLocation.x- Pixels [i+ 1].FLocation.x= 0) or
     (Pixels [i].FLocation.y- Pixels [i+ 1].FLocation.y= 0) then
      Inc (SameLineCount);
  end;

  if (Pixels [Count- 1].FLocation.x- Pixels [0].FLocation.x= 0) or
   (Pixels [Count- 1].FLocation.y- Pixels [0].FLocation.y= 0) then
    Inc (SameLineCount);

  Result:= SameLineCount/ Count; 
end;

constructor TComponent.Create (ID: Integer= -1);
begin
  inherited Create;
  
  SetLength (Pixels, 0);
  FMaxX:= -1; FMaxY:= -1;
  FMinX:= 100000; FMinY:= 1000000;
  SumOfR:= 0;  SumOfG:= 0; SumOfB:= 0;

  TopestPoint:= -1;
  LeftestPoint:= -1;
  RightestPoint:= -1;
  MostBottonPoint:= -1;
  
  HashedData:= THash.Create;
  FID:= ID;
  
  FCenterOfMass:= TPoint.Create (0, 0);

{  DifferentX:= THash.Create;
  DifferentY:= THash.Create;
  }
end;

procedure TComponent.Delete (x, y: Integer);
var
  Index, i: Integer;
begin
  Index:= GetIndex (x, y);
  if Index<> -1 then
  begin
    Pixels [Index].Free;
    for i:= Index+ 1 to Length (Pixels)- 1 do
      Pixels [i- 1]:= Pixels [i];
    SetLength (Pixels, Length (Pixels)- 1);
  end;
end;

procedure TComponent.Delete (Index: Integer);
var
  i: Integer;
begin
  if Index<> -1 then
  begin
    Pixels [Index].Free;
    for i:= Index+ 1 to Length (Pixels)- 1 do
      Pixels [i- 1]:= Pixels [i];
    SetLength (Pixels, Length (Pixels)- 1);
  end;
end;

function TComponent.ExtractContour: TComponent;
const
  AdjancedPixelY: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
  AdjancedPixelX: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
var
  RGBColor: TRGB;
  x, y: Integer;
  StartDir: TDirection;
  i, j: Integer;
  
begin
  try
    x:= MinX; y:= MinY;
    
    Result:= TComponent.Create;
    for j:= MinY to MaxY do
      for i:= MinX to MaxX do
        if HashedData.IsExist (i, j) then
        begin
          x:= i;
          y:= j;
          Break;
        end;


    RGBColor.r:= 0; RGBColor.g:= 0; RGBColor.b:= 0;

    StartDir:= dE;

    while not Result.HashedData.IsExist (x, y) do
    begin
      Result.Add (x, y, RGBColor);
    
      StartDir:= TDirection ( (Ord (StartDir)+ 5) mod 8);

      for i:= 0 to 7 do
      begin
        if HashedData.IsExist (x+ AdjancedPixelX [StartDir], y+   AdjancedPixelY [StartDir]) then
        begin
          Inc (x, AdjancedPixelX [StartDir]);
          Inc (y, AdjancedPixelY [StartDir]);

          Break;
        end;
      
        StartDir:= TDirection ( (Ord (StartDir)+ 1) mod 8);
      end;

    end;

  except
    raise Exception.Create ('Contour');
  end;
end;

procedure TComponent.FindBoundraryPoints;
var
  Temp,
  MaxFind: Integer;
  Max, Min: TPoint;
  x, y: Integer;
  
begin
  Max:= GetMaximum;
  Min:= GetMinimum;

  if TopestPoint= -1 then
  begin
    MaxFind:= -1;
    for y:= Min.y to Max.y do
    begin
      Temp:= 0;
      for x:= Min.x to Max.x do
        if HashedData.IsExist (x, y) then
          Inc (Temp);

      if MaxFind< Temp then
      begin
        MaxFind:= Temp;
        TopestPoint:= y;
      end;
      if 2* (Max.x- Min.x)< 3* MaxFind then
        Break;
    end;
  end;

  if MostBottonPoint= -1 then
  begin
    MaxFind:= -1;
    for y:= Max.y downto Min.y do
    begin
      Temp:= 0;
      for x:= Min.x to Max.x do
        if HashedData.IsExist (x, y) then
          Inc (Temp);

      if MaxFind< Temp then
      begin
        MaxFind:= Temp;
        MostBottonPoint:= y;
      end;
      if 2* (Max.x- Min.x)< 3* MaxFind then
        Break;
    end;
  end;

  if LeftestPoint= -1 then
  begin
    MaxFind:= -1;
    for x:= Min.x to Max.x do
    begin
      Temp:= 0;
      for y:= Min.y to Max.y do
        if HashedData.IsExist (x, y) then
          Inc (Temp);

      if MaxFind< Temp then
      begin
        MaxFind:= Temp;
        LeftestPoint:= x;
      end;
      if 2* (Max.y- Min.y)< 3* MaxFind then
        Break;
    end;
  end;

  if RightestPoint= -1 then
  begin
    MaxFind:= -1;
    for x:= Max.x to Min.x do
    begin
      Temp:= 0;
      for y:= Min.y to Max.y do
        if HashedData.IsExist (x, y) then
          Inc (Temp);

      if MaxFind< Temp then
      begin
        MaxFind:= Temp;
        RightestPoint:= x;
      end;
      if 2* (Max.y- Min.y)< 3* MaxFind then
        Break;
    end;
  end;

  Max.Free;
  Min.Free;
end;

procedure TComponent.Free;
var
  i: Integer;
begin

  for i:= 0 to Count- 1 do
    Pixels [i].Free;
    
  HashedData.Free;
  FCenterOfMass.Free;
  
  inherited;
  
end;

function TComponent.GetArea: Integer;
begin
  Result:= (MaxY- MinY+ 1)* (MaxX- MinX+ 1);
   
end;

function TComponent.GetCount: Integer;
begin
  Result:= Length (Pixels);
  
end;

function TComponent.GetDownLeftPoint: TPoint;
begin
  FindBoundraryPoints;
  Result:= TPoint.Create (LeftestPoint, MostBottonPoint);
end;

function TComponent.GetDownRightPoint: TPoint;
begin
  FindBoundraryPoints;
  Result:= TPoint.Create (RightestPoint, MostBottonPoint);
end;

function TComponent.GetEffectiveLength: Integer;
var
  DiffX: array of Integer;
  TotalTilNow,
  CountDiv2,
  i: Integer;
  Min,
  Max: TPoint;
begin
  Min:= GetMinimum;
  Max:= GetMaximum;
  SetLength (DiffX, Max.x- Min.x+ 1);

  for i:= 0 to Max.x- Min.x do
    DiffX [i]:= 0;
  for i:= 0 to Count- 1 do
    Inc (DiffX [Pixels [i].Location.x- Min.x]);

  TotalTilNow:= 0;
  CountDiv2:= Count div 2;
  Result:= -1;
  for i:= 0 to Max.x- Min.x do
  begin
    Inc (TotalTilNow, DiffX [i]);
    if CountDiv2< TotalTilNow then
    begin
      Result:= Diffx [i];
      Break;
    end;
  end;

  Max.Free;
  Min.Free;
  SetLength (Diffx, 0);
end;

function TComponent.GetEffectiveWidth: Integer;
var
  DiffY: array of Integer;
  TotalTilNow,
  CountDiv2,
  i: Integer;
  Min,
  Max: TPoint;
begin
  Min:= GetMinimum;
  Max:= GetMaximum;
  SetLength (DiffY, Max.y- Min.y+ 1);

  for i:= 0 to Max.y- Min.y do
    DiffY [i]:= 0;
  for i:= 0 to Count- 1 do
    Inc (DiffY [Pixels [i].Location.y- Min.y]);

  TotalTilNow:= 0;
  CountDiv2:= Count div 2;
  Result:= -1;
  for i:= 0 to Max.y- Min.y do
  begin
    Inc (TotalTilNow, DiffY [i]);
    if CountDiv2< TotalTilNow then
    begin
      Result:= Diffy [i];
      Break;
    end;
  end;

  Max.Free;
  Min.Free;
  SetLength (Diffy, 0);
end;

function TComponent.GetHeight: Integer;
begin
  Result:= MaxY- MinY+ 1;
  
end;

function TComponent.GetIndex (x, y: Integer): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Length (Pixels)- 1 do
    if (Pixels [i].Location.x= x) and
     (Pixels [i].Location.y= y) then
    begin
       Result:= i;
       Exit;
    end;
end;

function TComponent.GetMaximum: TPoint;
begin
  Result:= TPoint.Create (MaxX, MaxY);
  
end;

function TComponent.GetMinimum: TPoint;
begin
  Result:= TPoint.Create (MinX, MinY);
end;

function TComponent.GetPixel: TMyPixel;
begin
  Result:= Pixels [0];
end;

function TComponent.GetPixel (Index: Integer): TMyPixel;
begin
  if Index<> -1 then
    Result:= Pixels [Index]
 else
   raise ERangeCheckError.Create ('GetPixel');
   
end;

function TComponent.GetTopLeftPoint: TPoint;
begin
  FindBoundraryPoints;
  Result:= TPoint.Create (LeftestPoint, TopestPoint);
end;

function TComponent.GetTopRightPoint: TPoint;
begin
  FindBoundraryPoints;
  Result:= TPoint.Create (RightestPoint, TopestPoint);
end;

function TComponent.IsExists (x, y: Integer): Boolean;
begin
//  Result:= GetIndex (x, y)<> -1;
  Result:= HashedData.IsExist (x, y);
{
  if Result<> (GetIndex (x, y)<> -1) then
  begin
    Result:= HashedData.IsExists (x, y);
    Result:= GetIndex (x, y)<> -1;
    Result:= not Result;
  end;
}
end;

function TComponent.GetWidth: Integer;
begin
  Result:= MaxX- MinX+ 1;
  
end;

function TComponent.IsExists (Point: TPoint): Boolean;
begin
  Result:= IsExists (Point.x, Point.y);
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

  if Row< MaxPoint.y+ 1 then
    NewRow:= MaxPoint.y+ 1
  else
    NewRow:= FRow;

  if Column< MaxPoint.x+ 1 then
    NewCol:= MaxPoint.x+ 1
  else
    NewCol:= FColumn;

  Row:= NewRow;
  Column:= NewCol;

  for i:= 0 to ImagePixels.Count- 1 do
  begin
    Pixel:= ImagePixels.GetPixel (i);
    FBody [Pixel.Location.y, Pixel.Location.x]:=
          BLACK;
//        (Pixel.RGBColor.r+ Pixel.RGBColor.g+ Pixel.RGBColor.b) div 3;
  end;
  Result:= Self;
  MaxPoint.Free;
  
end;

function TMyPixel.ToString: String;
begin
  Result:= Location.ToString;//+...??!!
  
end;

{ TComponentCollection }

procedure TComponentCollection.AddComponent (Component: TComponent);
begin
  SetLength (FComponents, Length (FComponents)+ 1);
  FComponents [High (FComponents)]:= Component;
  
end;

constructor TComponentCollection.Create;
begin
  inherited;
  
  SetLength (FComponents, 0);
  
end;

procedure TComponentCollection.DeleteComponent (Index: Integer;  FreeIt: Boolean);
var
  i: Integer;
  
begin
  if (Index< 0) or (Count<= Index) then
    raise ERangeCheckError.Create ('Delete Component');
    

  if FreeIt then
    FComponents [Index].Free;

  for i:= Index+ 1 to Count- 1 do
    FComponents [i- 1]:= FComponents [i];

  SetLength (FComponents, Count- 1);

end;

procedure TComponentCollection.Free (FreeObj: Boolean= True);
var
  i: Integer;
  
begin
  if FreeObj then
    for i:= 0 to Count- 1 do
      FComponents [i].Free;
    
  SetLength (FComponents, 0);
  
  inherited Free;
  
end;

function TComponentCollection.GetCompCount: Integer;
begin
  Result:= Length (FComponents);
end;

function TComponentCollection.GetComponent (Index: Integer): TComponent;
begin
  if (Index< 0) or (Count<= Index) then
    raise ERangeCheckError.Create (IntToStr (Index));
    
  Result:= FComponents [Index];
  
end;

function TComponentCollection.GetComponentPointer: PComponent;
begin
  Result:= @FComponents [0];
  
end;

function TComponentCollection.GetMaxPoint: TPoint;
var
  TempPoint: TPoint;
  i: Integer;
begin
  Result:= Component [0].GetMaximum;
  for i:= 0 to Count- 1 do
  begin
    TempPoint:= Component [i].GetMaximum;
    
    if Result.x< TempPoint.x then
      Result.x:= TempPoint.x;

    if Result.y< TempPoint.y then
      Result.y:= TempPoint.y;
      
    TempPoint.Free;
  end;
end;

function TComponentCollection.GetMinPoint: TPoint;
var
  TempPoint: TPoint;
  i: Integer;
begin
  Result:= Component [0].GetMinimum;
  for i:= 0 to Count- 1 do
  begin
    TempPoint:= Component [i].GetMinimum;
    
    if TempPoint.x< Result.x then
      Result.x:= TempPoint.x;

    if TempPoint.y< Result.y then
      Result.y:= TempPoint.y;
      
    TempPoint.Free;
  end;
end;

function TComponentCollection.IsExists (x, y: Integer): Boolean;
var
  i: Integer;
begin
  for i:= 0 to Count- 1 do
    if FComponents [i].IsExists (x, y) then
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

  for i:= 0 to Count- 1 do
  begin
    ComponentsPixels:= FComponents [i];

    MinPoint:= ComponentsPixels.GetMinimum;
    MaxPoint:= ComponentsPixels.GetMaximum;

    if (MaxPoint.x- MinPoint.x< WidthThr) or (MaxPoint.y- MinPoint.y< HeightThr) then
    begin
      ComponentsPixels.Free;
      FComponents [i]:= nil;
    end;
    MinPoint.Free;
    MaxPoint.Free;
  end;

  Index:= 0;
  for i:= 0 to Count- 1 do
    if FComponents [i]<> nil then
    begin
      FComponents [Index]:= FComponents [i];
      Inc (Index);
    end;
  SetLength (FComponents, Index);

end;

procedure TComponent.Merge (AnotherComponent: TComponent; MaybeTheSame: Boolean);
var
  i, j: Integer;
  Hash: THash;
  Point: TPoint;
  
begin
  Hash:= AnotherComponent.HashedData;

  for i:= 0 to High (Hash.Data) do
  begin
    for j:= 0 to High (Hash.Data [i]) do
    begin
      Point:= Hash.Data [i][j];

      Self.Add (Point.x, Point.y);

    end;

  end;

end;

procedure TComponent.SaveToFile (FileName: String);
var
  FileHandle: TextFile;
begin
  AssignFile (Filehandle, FileName);
  Rewrite (FileHandle);
  
  AddToFile (FileHandle);
  
  CloseFile (FileHandle);
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

procedure TFMLImage.SetPixelBlack (r, c: Integer);
begin
  if (FRow<= r) or (r< 0) and
  (FColumn<= c) or (c< 0) then
    raise ERangeCheckError.Create ('SetPixelBlack');

  FBody [r, c]:= BLACK;

end;

procedure TFMLImage.SetPixelBlack (Point: TPoint);
begin
  Self.SetPixelBlack (Point.y, Point.x);  
end;


procedure TFMLImage.LoadRGB256Bitmap (Bitmap: TBitmap);
var
  r, c: Integer;
  BytePtr: ^Byte;

begin
  FImageType:= it8bit;
  
  if Bitmap.PixelFormat<> pf8bit then
    raise EInvalidImage.Create ('Invalid Pixel Format');

  Row:= Bitmap.Height;
  Column:= Bitmap.Width;
  
  for r:= 0 to FRow- 1 do
  begin
    BytePtr:= Bitmap.ScanLine [r];
    
    for c:= 0 to FColumn- 1 do
    begin
      FBody [r][c]:= BytePtr^;
      Inc (BytePtr);
    end;
    
  end;
  
end;

procedure TFMLImage.Free;
var
  i: Integer;

begin

  for i:= 0 to High (FBody) do
    SetLength (FBody [i], 0);
  SetLength (FBody, 0);

  inherited;
  
end;

function TFMLImage.FindAllComponentsInBox (TopLeft,BottomRight: TPoint;
          UseDialateBeforeExtraction: Boolean): TComponentCollection;
var
  NewImage: TFMLImage;

begin
  NewImage:= Self.Copy (TopLeft, BottomRight);
  
(*$IFDEF DEBIG_MODE*)
  NewImage.SaveAsBitmap ('C:\TempNormal.bmp');
(*$ENDIF*)

  if UseDialateBeforeExtraction then
  begin
    NewImage.Dilate;

(*$IFDEF DEBIG_MODE*)
    NewImage.SaveAsBitmap ('C:\TempDilate.bmp');
(*$ENDIF*)

  end;

  Result:= NewImage.FindAllComponents;
  
  NewImage.Free;
  
end;

function TFMLImage.Copy (TopLeft, BottomRight: TPoint): TFMLImage;
var
  r, c: Integer;
  Newr: Integer;
  SourcePixPtr,
  TargetPixPtr: PInteger;

begin
  Result:= TFMLImage.Create;
  Result.FImageType:= FImageType;
  Result.Row:= BottomRight.y- TopLeft.y+ 1;
  Result.Column:= BottomRight.x- TopLeft.x+ 1;

  Newr:= 0;

  for r:= TopLeft.y to BottomRight.y do
  begin
    SourcePixPtr:= ScanLine [r];
    TargetPixPtr:= Result.ScanLine [Newr];

    Inc (SourcePixPtr, TopLeft.x);

    for c:= TopLeft.x to BottomRight.x do
    begin
      TargetPixPtr^:= SourcePixPtr^;
      Inc (SourcePixPtr);
      Inc (TargetPixPtr);
      
    end;
    Inc (Newr);

  end;

end;

function TFMLImage.FindAllComponents: TComponentCollection;
var
  i, Temp,
  r, c: Integer;
  PixPtr, LeftPixPtr,
  UpPixPtr,
  LastRowPtr, CurRowPtr,
  CurLeftPtr
  : PInteger;
  ActiveComponent: TComponent;
  LastRow, CurRow: array [0..10000] of Integer;
  IsCopied: array of Boolean;
  TempCollection: TComponentCollection;

begin

  Result:= TComponentCollection.Create;
  TempCollection:= TComponentCollection.Create;

  CurRowPtr:= @CurRow [0];
  for c:= 0 to FColumn- 1 do
  begin
    CurRowPtr^:= 0;
    Inc (CurRowPtr);
    
  end;

  PixPtr:= @FBody [0, 0];
  LeftPixPtr:= @Temp;
  LeftPixPtr^:= WHITE;
  ActiveComponent:= nil;

  for c:= 0 to FColumn- 1 do
  begin
    if c<> 0 then
    begin
      LeftPixPtr:= PixPtr;
      Inc (PixPtr);
    end;

    if PixPtr^= BLACK then
    begin
      if LeftPixPtr^= BLACK then
      begin
        ActiveComponent.Add (c, 0);
        CurRow [c]:= CurRow [c- 1];

      end
      else
      begin
        ActiveComponent:= TComponent.Create (TempCollection.Count);
        ActiveComponent.Add (c, 0);

        TempCollection.AddComponent (ActiveComponent);
        CurRow [c]:= TempCollection.Count- 1;

      end;

    end
    else
     CurRow [c]:= -1;

  end;

  for r:= 1 to FRow- 1 do
  begin
    LastRow:= CurRow;

    PixPtr:= ScanLine [r];
    UpPixPtr:= ScanLine [r- 1];
    CurRowPtr:= @CurRow [0];
    CurLeftPtr:= @CurRow [0];
    LastRowPtr:= @LastRow [0];

    for c:= 1 to FColumn- 1 do
    begin
      LeftPixPtr:= PixPtr;
      Inc (PixPtr);
      Inc (UpPixPtr);
      
      Inc (CurRowPtr);
      Inc (LastRowPtr);

      if PixPtr^= BLACK then
      begin

        if (LeftPixPtr^= BLACK) and (c<> 1) and (LastRow [c]<> CurRow [c- 1]) then
        begin
          TempCollection.FComponents [CurLeftPtr^].Add (c, r);
          CurRowPtr^:= CurLeftPtr^;

          if UpPixPtr^= BLACK then
            if TempCollection.Component [LastRowPtr^]<> TempCollection.Component [CurRowPtr^] then
            begin

              if TempCollection.FComponents [LastRowPtr^].Count< TempCollection.FComponents [CurRowPtr^].Count then
              begin
                TempCollection.FComponents [CurRowPtr^].Merge (TempCollection.FComponents [LastRowPtr^], True);

                for i:= 0 to TempCollection.Count- 1  do
                  if (i<> LastRowPtr^) and
                    (TempCollection.FComponents [i]= TempCollection.FComponents [LastRowPtr^]) then
                    TempCollection.FComponents [i]:= TempCollection.FComponents [CurRowPtr^];

                TempCollection.FComponents [LastRowPtr^].Free;
                TempCollection.FComponents [LastRowPtr^]:= TempCollection.FComponents [CurRowPtr^];

              end
              else
              begin
                TempCollection.FComponents [LastRowPtr^].Merge (TempCollection.FComponents [CurRowPtr^], True);
//                WriteLn (TempCollection.FComponents [LastRow [c]].ID, ' Merged with ',  TempCollection.FComponents [CurRow [c]].ID);

                for i:= 0 to TempCollection.Count- 1  do
                  if (i<> CurRowPtr^) and
                    (TempCollection.FComponents [i]= TempCollection.FComponents [CurRowPtr^]) then
                    TempCollection.FComponents [i]:= TempCollection.FComponents [LastRowPtr^];

                TempCollection.FComponents [CurRowPtr^].Free;
                TempCollection.FComponents [CurRowPtr^]:= nil;
                TempCollection.FComponents [CurRowPtr^]:=
                            TempCollection.FComponents [LastRowPtr^];

              end

            end;

        end

        else if UpPixPtr^= BLACK then
        begin
          TempCollection.FComponents [LastRowPtr^].Add (c, r);
          CurRowPtr^:= LastRowPtr^;
                        
        end
        else 
        begin
          ActiveComponent:= TComponent.Create (TempCollection.Count);
          ActiveComponent.Add (c, r);

          TempCollection.AddComponent (ActiveComponent);
          CurRowPtr^:= TempCollection.Count- 1;

        end;

      end
      else
        CurRowPtr^:= -1;

      Inc (CurLeftPtr);
       
    end;

  end;


  SetLength (IsCopied, TempCollection.Count+ 1);
  for i:= 0 to TempCollection.Count- 1 do
    IsCopied [i]:= False;

  for i:= 0 to TempCollection.Count- 1 do
    if not IsCopied [TempCollection.FComponents [i].ID] then
    begin
      Result.AddComponent (TempCollection.FComponents [i]);
//      WriteLn (i, ' Saved: ', TempCollection.FComponents [i].ID);
      IsCopied [TempCollection.FComponents [i].ID]:= True;

    end;
  
  TempCollection.Free (False);
  SetLength (IsCopied, 0);
  
end;

procedure TFMLImage.SaveAsText (FileName: String; PrintBody: Boolean);
var
  r, c: Integer;
  OutputFile: TextFile;

begin
  AssignFile (OutputFile, FileName);
  Rewrite (OutputFile);

  for r:= 0 to FRow- 1 do
  begin
    for c:= 0 to FColumn- 1 do
      if FBody [r, c]= WHITE then
        if PrintBody then
          System.Write (OutputFile, '(', FBody [r, c], ')')
        else
          System.Write (OutputFile, 'W')
      else
        if PrintBody then
          System.Write (OutputFile, '(', FBody [r, c], ')')
        else
          System.Write (OutputFile, 'B');

    Writeln (OutputFile);
    
  end;
  CloseFile (OutputFile);
  
end;

function TComponent.GetPercentage: Integer;
begin
  Result:= 100* Count div (Width* Height);
  
end;

function TFMLImage.FindAllComponentsHavePointInBox (TopLeft, BottomRight: TPoint):
  TComponentCollection;
begin
  raise ENotImplemented.Create ('FindAllComponentsHavePointInBox (');

end;

function TFMLImage.GetScanLine (RowIndex: Integer): PInteger;
begin
  if Row<= RowIndex then
    raise ERangeCheckError.Create ('GetScanLine'+ IntToStr (Row)+ ':'+ IntToStr (RowIndex));

  Result:= @FBody [RowIndex, 0];

end;

function TFMLImage.IsVerticalLineBlack (ColIndex, TopRow, BotRow,
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

function TFMLImage.IsHorizentalLineBlack (RowIndex, LeftCol, RightCol,
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

function TComponent.FindMinDistToComp (AnotherComponnt: TComponent): Extended;
var
  i: Integer;
  Temp: Extended;

begin
  Result:= 1e10;
  
  if AnotherComponnt.Count= 0 then
    Result:= 0
  else
    for i:= 0 to AnotherComponnt.Count- 1 do
    begin
      Temp:= FindMinDistToPoint (AnotherComponnt.Pixels [i].FLocation);
      
      if Temp< Result then
        Result:= Temp;

    end;
    
end;

function TComponent.FindMinDistToPoint (APoint: TPoint): Extended;
var
  i: Integer;
  Temp: Extended;

begin
  Result:= 1e10;

  if Self.Count= 0 then
    Result:= 0
  else
    for i:= 0 to Self.Count- 1 do
    begin
      Temp:= APoint.CalcDistance (Self.Pixels [i].Location);

      if Temp< Result then
        Result:= Temp;

    end;
    
end;

function TFMLImage.ExtractFeatures (NewSize: Integer; SmoothDegree: Integer;
       NumberOfMasks: Integer): TFeatureVectorBasedOnGradiant;
var
  ResizedImage,
  SmoothedImage: TFMLImage;
  GradiantIn8Dir: T8DirGradiantFeature;
  
begin
  ResizedImage:= Self.Resize (NewSize, NewSize);
(*$IFDEF DEBUG_MODE*)
  ResizedImage.SaveAsBitmap ('Resized.00.bmp');
(*$ENDIF DEBUG_MODE*)
  
  SmoothedImage:= ResizedImage.Smooth (SmoothDegree);
(*$IFDEF DEBUG_MODE*)
  SmoothedImage.SaveAsBitmap ('Smoothed.00.bmp');
(*$ENDIF DEBUG_MODE*)

  GradiantIn8Dir:= SmoothedImage.ApplySobelAndGetGradiantIn8Dir;
  Result:= GradiantIn8Dir.SampleGradiant (NumberOfMasks);

  GradiantIn8Dir.Free;
  SmoothedImage.Free;
  ResizedImage.Free;

end;

function TFMLImage.Smooth (RepeatCount: Integer): TFMLImage;
var
  TempImage: TFMLImage;
  i: Integer;

begin
  if 0< RepeatCount then
  begin
    TempImage:= Self.DoSmooth;
    
    Result:= TempImage;
    
    for i:= 2 to RepeatCount do
    begin
      TempImage.SaveAsBitmap ('R'+ IntToStr (i- 1)+ '.bmp');
      TempImage:= Result.DoSmooth;
      Result.Free;
      Result:= TempImage;
    
    end;
  end
  else
    Result:= Self.Copy (TPoint.Create (0, 0),
        TPoint.Create (Row- 1, Column-1));

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
        Inc (m10, (c+ 1)* dummy);
        Inc (m01, (r+ 1)* dummy);
        
        Inc (RowPtr);
      
      end;
      
    end;

    xc:= m10/ m00;
    yc:= m01/ m00;
    xprimc:= NewColumn/ 2.0;
    yprimc:= NewRow/ 2.0;
    
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
        ix:= Round ((c- xprimc)/ Alpha+ xc);
        iy:= Round ((r- yprimc)/ Beta+ yc);

        if (ix< Column) and (0<= ix ) and (iy< Row) and (0<= iy) then
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

function TFMLImage.DoSmooth: TFMLImage;
var
  r, c: Integer;
  SourcePtr,//

  TargetPtr,//Pixel [r, c]
  LastPixTPtr,//Pixel [r, c- 1]
  LastRowTPtr,//Pixel [r- 1, c]
  LastRowCelTPtr//Pixel [r- 1, c- 1]
  : PInteger;
  TempResult: TFMLImage;
  TempIntArray: array of Integer;// This temp variable is used to unify the process and to avoid some if-then-else strcuture.
  
begin
  if Self.ImageType<> it8bit then
  begin
    for r:= 0 to FRow- 1 do
    begin
      SourcePtr:= Self.ScanLine [r];
      for c:= 0 to FColumn- 1 do
      begin
        SourcePtr^:= SourcePtr^* 255;
        Inc (SourcePtr);
        
      end;

    end;

  end;

  Result:= TFMLImage.Create;
  Result.FImageType:= it8bit;
  TempResult:= TFMLImage.Create;
  TempResult.FImageType:= it8bit;
  Result.Column:= Column;
  Result.Row:= Row;
  TempResult.Row:= Row+ 2;
  TempResult.Column:= Column+ 2;
(*$IFDEF DEBUG_MODE*)
  Self.SaveAsText ('BeforeSmooth.txt', True);
(*$ENDIF*)
  
  SetLength (TempIntArray, Column+ 5);
  Fillchar (TempIntArray [0], (Column+ 5)* SizeOf (Integer), 0);
  
{Step 1: Calculating TempResult's Values }

  for r:= 0 to Row- 1 do
  begin
    
    if r<> 0 then
    begin
      LastRowTPtr:= TempResult.ScanLine [r];
      Inc (LastRowTPtr);
      LastRowCelTPtr:= LastRowTPtr;
      Dec (LastRowCelTPtr);

    end
    else
    begin
      LastRowTPtr:= @TempIntArray [0];
      LastRowCelTPtr:= @TempIntArray [0];

    end;
      
    TargetPtr:= TempResult.ScanLine [r+ 1];
    Inc (TargetPtr);
    SourcePtr:= ScanLine [r];
      
    TargetPtr^:= SourcePtr^+ LastRowTPtr^;

    LastPixTPtr:= TargetPtr;
    Inc (TargetPtr);
    Inc (SourcePtr);
    Inc (LastRowTPtr);
    Inc (LastRowCelTPtr);


    for c:= 1 to Column- 1 do
    begin
      TargetPtr^:= SourcePtr^+
         LastPixTPtr^+ LastRowTPtr^- LastRowCelTPtr^;

      Inc (TargetPtr);
      Inc (SourcePtr);
      Inc (LastRowTPtr);
      Inc (LastPixTPtr);
      Inc (LastRowCelTPtr);
        
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

(*$IFDEF DEBUG_MODE*)  
  TempResult.SaveAsText ('TempResult.txt', True);
(*$ENDIF*)

{Step 2: Calculating Results Value }
  r:= 0;
  TargetPtr:= Result.ScanLine [r];

  TargetPtr^:= TempResult.FBody [r+ 2, 2];
  TargetPtr^:= Round (TargetPtr^/ 4);

  Inc (TargetPtr);

  for c:= 1 to Column- 2 do
  begin
    TargetPtr^:= TempResult.FBody [r+ 2, c+ 2]
       - TempResult.FBody [r+ 2, c- 1];

    TargetPtr^:= Round (TargetPtr^/ 6);

    Inc (TargetPtr);

  end;

  c:= Column- 1;
  TargetPtr^:= TempResult.FBody [r+ 2, c+ 2]
     - TempResult.FBody [r+ 2, c- 1];
  TargetPtr^:= Round (TargetPtr^/ 4);

  for r:= 1 to Row- 2 do
  begin
    TargetPtr:= Result.ScanLine [r];

    TargetPtr^:= TempResult.FBody [r+ 2, 2]
       - TempResult.FBody [r- 1, 2];

    TargetPtr^:= Round (TargetPtr^/ 6);

    Inc (TargetPtr);

    for c:= 1 to Column- 2 do
    begin
      TargetPtr^:= TempResult.FBody [r+ 2, c+ 2]+
         TempResult.FBody [r- 1, c- 1]- TempResult.FBody [r- 1, c+ 2]
         - TempResult.FBody [r+ 2, c- 1];

      TargetPtr^:= Round (TargetPtr^/ 9);

      Inc (TargetPtr);

    end;

    c:= Column- 1;
    TargetPtr^:= TempResult.FBody [r+ 2, c+ 2]+
       TempResult.FBody [r- 1, c- 1]- TempResult.FBody [r- 1, c+ 2]
       - TempResult.FBody [r+ 2, c- 1];

    TargetPtr^:= Round (TargetPtr^/ 6);
    
  end;
    
  r:= Row- 1;
  TargetPtr:= Result.ScanLine [r];

  TargetPtr^:= TempResult.FBody [r+ 2, 2]
     - TempResult.FBody [r- 1, 2];

  TargetPtr^:= Round (TargetPtr^/ 4);

  Inc (TargetPtr);

  for c:= 1 to Column- 2 do
  begin
    TargetPtr^:= TempResult.FBody [r+ 2, c+ 2]+
       TempResult.FBody [r- 1, c- 1]- TempResult.FBody [r- 1, c+ 2]
       - TempResult.FBody [r+ 2, c- 1];

    TargetPtr^:= Round (TargetPtr^/ 6);

    Inc (TargetPtr);

  end;

  c:= Column- 1;
  TargetPtr^:= TempResult.FBody [r+ 2, c+ 2]+
     TempResult.FBody [r- 1, c- 1]- TempResult.FBody [r- 1, c+ 2]
     - TempResult.FBody [r+ 2, c- 1];

  TargetPtr^:= Round (TargetPtr^/ 4);


  TempResult.Free;
  SetLength (TempIntArray, 0);
(*$IFDEF DEBUG_MODE*)  
  Result.SaveAsText ('Result.txt', True);
(*$ENDIF*)

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
  TempIntArray: array of Integer;// This temp variable is used to unify the process and to avoid some if-then-else strcuture.
  
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
  SetLength (TempIntArray, 0);

end;

function TFMLImage.ApplySobelAndGetGradiantIn8Dir: T8DirGradiantFeature;
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
//  TargetPtr: PExtended;
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
    Inc (CurRowPtr);
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
          FeaturePtr [(DirectionIndex+ 1) mod 8]^:=
              NewY* Sqrt (2.0);
        
        end;

      end;

      Inc (NextRowPtr);
      Inc (CurRowPtr);
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

function TFMLImage.GetSampleGradiantFor8Dir: TSampleGradiantIn8Dir;
begin
  raise ENotImplemented.Create ('GetSampleGradiantFor8Dir');
  
end;

procedure TFMLImage.LoadFromFMLFile (var InputFile: TByteFile);
var
  r, c: Integer;
  b1, b2: Byte;
  Ptr: PInteger;

begin
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

procedure TFMLImage.LoadJpeg (FileName: string);
var
  JpegImage: TJpegImage;
  BitmapImage: TBitmap;

begin
  JpegImage:= TJPEGImage.Create;
  BitmapImage:= TBitmap.Create;
  
  JpegImage.LoadFromFile (FileName);
  BitmapImage.Assign (JpegImage);
  Self.LoadBitMap (BitmapImage);
  
  BitmapImage.Free;
  JpegImage.Free;

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
var
  r, c, i: Integer;
  RowPtr: PByte;
  cIndex: Integer;
  Threshold: Integer;
  NewImage: TFMLImage;


begin
  Row:= Bitmap.Height;
  Column:= Bitmap.Width;

  if Bitmap.PixelFormat<> pf8bit then
  begin
    raise EInvalidImage.Create ('Image must be 256 Colored');

  end
  else
  begin
    NewImage:= TFMLImage.Create;
    NewImage.LoadBitMap (Bitmap);
    Threshold:= NewImage.GrayThreshold* 2 div 3;//??!!

    for r:= 0 to FRow- 1 do
    begin
      RowPtr:= Bitmap.ScanLine [r];

      cIndex:= 0;
      for c:= 0 to FColumn- 1 do
      begin
        if RowPtr^< Threshold then
          FBody [r, c]:= BLACK
        else
          FBody [r, c]:= WHITE;

        Inc (RowPtr);

      end;

    end;

    NewImage.Free;
    
  end;

end;

procedure TFMLImage.DeleteRow (Index: Integer);
var
  r: Integer;

begin
  Body [Index, 0];

  for r:= Index+ 1 to FRow- 1 do
    FBody [r- 1]:= FBody [r];
  SetLength (FBody [FRow- 1], 0);
  SetLength (FBody, FRow- 1);
  Dec (FRow);

end;

procedure TFMLImage.DeleteRowsInRange (TopIndex, BotIndex: Integer);
var
  r, s: Integer;

begin
  if TopIndex<= BotIndex then
  begin
    s:= TopIndex;

    for r:= BotIndex+ 1 to Row- 1 do
    begin
      FBody [s]:= FBody [r];
      Inc (s);

    end;

    for r:= TopIndex to BotIndex do
      SetLength (FBody [r], 0);

    SetLength (FBody, Row- BotIndex+ TopIndex- 1);
    
  end;

end;

procedure TFMLImage.DeleteHorizentalBlackLine (Percentage: Extended);
begin
  raise ENotImplemented.Create ('DeleteHorizentalBlackLine ');
  
end;

procedure TFMLImage.DeleteVerticalBlackLine (Percentage: Extended);
var
  Rows: array of Boolean;
  Count: Integer;
  r, c, MaxIndex: Integer;
  CurPointer: PInteger;

begin
  MaxIndex:= 0;
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

    if Column< Count/ Percentage then
      Rows [r]:= True
    else
      Rows [r]:= False;

  end;

  for r:= Row- 1 downto 0 do
    if Rows [r] then
      DeleteRow (r);

  SetLength (Rows, 0);
  
end;

end.
