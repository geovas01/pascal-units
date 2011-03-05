unit FMLImage;
// (*$Define General_Debug*)
 (*$DEFINE REVERSE_MODE*)
// (*$DEFINE DEBUG_MODE*)

interface
uses
  SysUtils,
  FeatureUnit, CollectionUnit, ComponentsUnit,
  ICLFeatureUnit, Graphics, GeometryUnit;
    
const
  BLACK: Integer= 1;
  WHITE: Integer= 0;
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

  TComponentCollection= class (TObject)
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
  TArrayofArrayofInt= array of array of Integer;
  
  TFMLImage= class (TObject)
  private
    FHistogram: array [0..255] of Integer;
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

    procedure CalculateHistogram;
    function GetColumn: Integer;
    function GetHistogram (Color: Integer): Integer;
    function GetPattern: Integer;
    procedure SetPattern (const Value: Integer);
    function GetRow: Integer;
    procedure SetColumn (const Value: Integer);
    procedure SetRow (const Value: Integer);
    function GetBodyColor (r, c: Integer): Integer;

    function DeleteImage (NoiseColor: TColor; NoiseThr: Integer): TFMLImage;
    function FindAllComponents: TComponentCollection;
    function GetScanLine (RowIndex: Integer): PInteger;
    function DoSmooth: TFMLImage;
    function DoSmooth1: TFMLImage;
    function DoSmooth2: TFMLImage;

    function ApplySobelAndGetGradiantIn8Dir: T8DirGradiantFeature;
    function GetSampleGradiantFor8Dir: TSampleGradiantIn8Dir;

    procedure SetImageKind (const Value: TInputKind);
    
  protected
    procedure LoadPhotoCopiedImage (Bitmap: TBitmap); overload;

  public
    (*ImageType determines how the image is stored in the FBody array*)
    property ImageType: TImageType read FImageType;
    property Pattern: Integer read GetPattern write SetPattern;
    property Row: Integer read GetRow write SetRow;
    property Column: Integer read GetColumn write SetColumn;
    (*Histogram returns the number of occurence of each color. It only works when ImageType= it8bit*)
    property Histogram [Color: Integer]: Integer read GetHistogram;
    (*returns the value of pixel located in r and c*)
    property Body [r, c: Integer]: Integer read GetBodyColor;
    property Kind: TInputKind read FImageKind write SetImageKind;
    property IsBlank: TMyBoolean read FIsBlank;
    (*returns a pointer to the first integer in the rowindex'th row*)
    property ScanLine [RowIndex: Integer]: PInteger read GetScanLine;

    constructor Create;overload;
    (*Create an image whose black pixel are stored in PixelCollection.
    Note!! this constructor crops the image*)
    constructor Create (PixelCollection: TComponent);overload;
    (*Create an image whose black pixel are stored in ComponentCollection (which is a collection of PixelCollection)*)
    constructor Create (ComponentCollection: TComponentCollection);overload;
    constructor Create (BlackPoints: TPointCollection);overload;

    procedure Free;

    (*Saves the image in the file whose handle in OutputFile by FML format*)
    procedure SaveInFMLFile (var OutputFile: TByteFile); overload;
    (*Saves the image in the filename by FML Format*)
    procedure SaveInFMLFile (Filename: String); overload;
    procedure SaveAsBitmap (FileName: string);
    (*Saves the image as text file.
      If the PrintBodyValue is false then it prints out the characters W and B.
      Otherwise, it prints the value of FBody.
      !!Note that PrintBodyValue= False  only works for images whose imagetype is itMonochrome*)
    procedure SaveAsText (FileName: String; PrintBodyValue: Boolean= False);

    (*Load an image which is filtered by photoshop "Photocopy" fileter.
      The pixels of transformation of an image under this filter will be
        0 or 255.*)
    procedure LoadPhotoCopiedImage (FileName: string); overload;

    procedure LoadBitMap (FileName: String);overload;
    procedure LoadBitMap (Bitmap: TBitmap; Pattern: Integer= 9999);overload;
    procedure LoadRGB256Bitmap (Bitmap: TBitmap);
    procedure LoadFromFMLFile (var InputFile: TByteFile); overload;

    function GetAsBitmap (MonoChrome: Boolean= True): TBitmap;
    (*This function returns a new bitmap in Monochrome style, but doesn't change
     the properties of itself*)
    function ConvertToGrayScale (Bitmap: TBitmap): TBitmap;
    (*Find a threshold for convertToBinary. It works for the images whose ImageType is it8Bit, only*)
    function GrayThreshold: Integer;
    function ConvertToBinary (Bitmap: TBitmap): TBitmap; overload;
    function ConvertToBinary: TFMLImage; overload;

    (*Adds the pixels in ImaePixels Component to the image (Self) and
      returns the self*)    
    function MixImage (ImagePixels: TComponent): TFMLImage;
    (*Sets the pixel in location r and c to black
      NOTE: This procedure works when the ImageType is itMonochrome*)
    procedure SetPixelBlack (r, c: Integer); overload;
    procedure SetPixelBlack (Point: TPoint); overload;

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
    function FindAllComponentsInBox (TopLeft, BottomRight: TPoint;
              UseDialateBeforeExtraction: Boolean= True): TComponentCollection;
    (*
      NOTE:: This procedure is not implemented Yet!!
    *)
    function FindAllComponentsHavePointInBox (TopLeft, BottomRight: TPoint): TComponentCollection;

    (*Dialtes the image and return Self*)
    function Dilate: TFMLImage;
    (*Erodes the image and return Self*)
    function Erode: TFMLImage;

    (*
      NOTE:: This procedure is not implemented Yet!!
    *)
    procedure Write (PrintToFile: Boolean= False; FileName: String= '');

    (*
      NOTE:: This procedure is not implemented Yet!!
    *)
    procedure Add (Component: TComponent);

    (*
      Copies a rectange from the image.
    *)
    function Copy (TopLeft, BottomRight: TPoint): TFMLImage;
    (*
      Copies the rows in the range TopRowIndex and BottomRowIndex, inclusivly.
    *)
    function CopyRows (TopRowIndex, BottomRowIndex: Integer): TFMLImage;
    (*
      Rotates the image, and return the rotated image. 
      NOTE:: This function only works with it8bit images.
    *)
    function Rotate (AngleInDeg: Integer): TFMLImage;

    (*Returns the ration of the black pixel in the column ColIndex by the BotRow- TopRow+ 1*)
    function IsVerticalLineBlack (ColIndex: Integer; TopRow, BotRow: Integer;
      Width: Integer= 1): Real;
    function IsHorizentalLineBlack (RowIndex: Integer; LeftCol, RightCol: Integer;
      Heigth: Integer= 1): Real;

    (*Resizes the image
    NOTE:: These functions are <B>not suitable</B> when one wants to shrink the image *)
    function Resize (NewRow, NewColumn: Integer): TFMLImage;
    (*Resizes the image*)
    function NewResize (NewRow, NewColumn: Integer; SaveAspectRatio: Boolean= False): TFMLImage;

    (*Smoothes the image*)
    function Smooth (RepeatCount: Integer): TFMLImage;


    
    function ExtractFeatures (NewSize: Integer; SmoothDegree: Integer;
       NumberOfMasks: Integer= 5): TFeatureVectorBasedOnGradiant;
    function ExtractFreemanFeature: TFreemanFeature;

    (*Deletes a row from image*)
    procedure DeleteRow (Index: Integer);
    procedure DeleteRowsInRange (TopIndex, BotIndex: Integer);
    (*NOTE:: This function does not update the center of mass*)
    procedure DeleteColumnsInRange (TopIndex, BotIndex: Integer);

    (*Deletes the rows who have more than Percentage black pixel in them*)
    procedure DeleteVerticalBlackLine (Percentage: Extended= 1/2);
    procedure DeleteHorizentalBlackLine (Percentage: Extended= 1/2);

    (*Note than, FCenterofMass holds the sum of x and y of black pixels,
    not the centerofmass. Use this function to get COM.
    The ReCalc can be used to force the function to recalculates the
    COM*)
    function GetCenterOfMass (Recalc: Boolean= False):  TPoint;

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
  {Borland.Vcl.Controls, System.Xml.XPath, }
  
  Math, ExceptionUnit, TypInfo, VectorUnit, JPeg;
  
type
  EFMLImageNotInitialized= class (Exception);
  ERangeCheckError= class (Exception);

  TArrArrInt= array of array of Integer;
  TArrInt= array of Integer;

procedure TFMLImage.CalculateHistogram;
var
  r, c: Integer;
  IntPtr: PInteger;
  
begin
  if ImageType<> it8bit then
    raise EInvalidImage.Create ('Histogram is not defined for this Type of image');

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

  HistogramIsCalced:= True;
  
end;

constructor TFMLImage.Create;
begin
  inherited Create;

  FImageType:= itMonoChrome;
  HistogramIsCalced:= False;
  FRow:= -1; FColumn:= -1; FPattern:= -1;
  FIsBlank:= mbUnSet;
  FCenterOfMass:= TPoint.Create;
  
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

  if MonoChrome then
    case ImageType of
      itMonoChrome:
      begin

        Result.PixelFormat:= pf1bit;
        Result.Monochrome:= True;
        Result.Height:= FRow;
        Result.Width:= FColumn;

        for r:= 0 to FRow- 1 do
        begin
          RowPtr:= Result.ScanLine [r];

          PixPtr:= ScanLine [r];
          Temp:= 0;

          CIndex:= 0;
          for c:= 0 to (FColumn+ 7) div 8- 1 do
          begin
            for i:= 0 to 7 do
            begin
              case FImageType of
                itMonoChrome:
                  if (PixPtr^= WHITE) then
                    Temp:= Temp or (1 shl (7- i));

                it8bit:
                  if (127< PixPtr^) then
                    Temp:= Temp or (1 shl (7- i));

                 else
                   raise ENotImplemented.Create ('GetAsBitmap');
              end;

               Inc (cIndex);
               Inc (PixPtr);

               if FColumn<= cIndex then
                 Break;

            end;

            RowPtr^:= Temp;
            Inc (RowPtr);
            Temp:= 0;

          end;
        end;

      end;
      it8Bit:
      begin
        Result.PixelFormat:= pf24bit;
        Result.Height:= FRow;
        Result.Width:= FColumn;

        for r:= 0 to Row- 1 do
        begin
          RowPtr:= Result.ScanLine [r];
          PixPtr:= ScanLine [r];

          for c:= 0 to FColumn- 1 do
          begin
            RowPtr^:= PixPtr^;
            Inc (RowPtr);
            RowPtr^:= PixPtr^;
            Inc (RowPtr);
            RowPtr^:= PixPtr^;
            Inc (RowPtr);

            Inc (PixPtr);
            
          end;

        end;
      
      end;
    
    end
  else
  begin
    case ImageType of
      itMonoChrome:
      begin
        Result.PixelFormat:= pf8bit;
        Result.Monochrome:= MonoChrome;
        Result.Height:= FRow;
        Result.Width:= FColumn;


        for r:= 0 to Row- 1 do
        begin
          RowPtr:= Result.ScanLine [r];
          PixPtr:= @FBody [r, 0];

          for c:= 0 to FColumn- 1 do
          begin
            case FImageType of
              itMonoChrome:
                RowPtr^:= $FF* (1- PixPtr^);

              it8bit:
                RowPtr^:= PixPtr^;

               else
                 raise ENotImplemented.Create ('GetAsBitmap');
            end;

            Inc (PixPtr);
            Inc (RowPtr);

            end;

        end;

      end;
      
    end

  end;

end;

function TFMLImage.GetBodyColor (r, c: Integer): Integer;
begin
  if (r< FRow) and (r>= 0) and
      (c< FColumn) and (c>= 0) then
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
  if FImageType<> it8bit then
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
  Bitmap: TBitmap;
  
begin
  Bitmap:= TBitmap.Create;
  
  Bitmap.LoadFromFile (FileName);
  LoadBitMap (Bitmap);
  
  Bitmap.Free;
  
end;

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
//??!!
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
  else if Bitmap.PixelFormat= pf1bit then
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
          if (RowPtr^ shr (7- i)) and 1= 1 then
//            FBody [r, cIndex]:= WHITE
            PixPtr^:= WHITE
          else
          begin
//            FBody [r, cIndex]:= BLACK;
            PixPtr^:= BLACK;
            FCenterOfMass.Move (r, cIndex);
            Inc (BlackPixCount);

          end;
 (*$ENDIF*)

          Inc (cIndex);
          Inc (PixPtr);

        end;
        Inc (RowPtr);

      end;
      
      for i:= 0 to (FColumn- 1) mod 8 do
      begin

 (*$IFDEF REVERSE_MODE*)
        if (RowPtr^ shr (7- i)) and 1= 1 then
//          FBody [r, cIndex]:= WHITE
          PixPtr^:= WHITE
        else
        begin
//          FBody [r, cIndex]:= BLACK;
          PixPtr^:= BLACK;
          FCenterOfMass.Move (r, cIndex);
          Inc (BlackPixCount);

        end;
 (*$ELSE*)
        if (RowPtr^ shr (7- i)) and 1= 1 then
        begin
          PixPtr^:= BLACK;
//          FBody [r, cIndex]:= BLACK;
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
  Self.SaveAsText ('C:\CheckBitmap.txt');
  
end;

procedure TFMLImage.SaveAsBitmap (FileName: string);
var
  TempImage: Graphics.TBitmap;
  
begin
  TempImage:= GetAsBitmap;
  TempImage.SaveToFile (FileName);
  TempImage.Free;
  
end;

procedure TFMLImage.SetColumn (const Value: Integer);
var
  r: Integer;
begin
  FColumn:= Value;
  
  if FRow>0 then
  begin
    SetLength (FBody, FRow);
    for r:= 0 to FRow- 1 do
      SetLength (FBody [r], FColumn);
      
  end;
  
end;

procedure TFMLImage.SetImageKind (const Value: TInputKind);
begin
  FImageKind:= Value;
  
end;

procedure TFMLImage.SetRow (const Value: Integer);
var
  r: Integer;
begin
  FRow:= Value;
  
  if FColumn>0 then
  begin
    SetLength (FBody, FRow);
    for r:= 0 to FRow- 1 do
      SetLength (FBody [r], FColumn);
      
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
  NewBody: TArrayofArrayofInt;
  BodyPtr,
  NewBodyPtr: PInteger;
  r, c,
  i: Integer;
  
begin
  Result:= Self;
  
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
  begin
    BodyPtr:= ScanLine [r];
    
    for c:= 0 to Column- 1 do
    begin
      if BodyPtr^=  BLACK then
        for i:= 0 to 8 do
          NewBody [r+ 1+ AdjRow [i]][c+ 1+ AdjCol [i]]:= BLACK;

      Inc (BodyPtr);

    end;

  end;

  for r:= 0 to FRow- 1 do
    SetLength (FBody [r], 0);
  SetLength (FBody, 0);

  FBody:= NewBody;

end;

function TFMLImage.Erode: TFMLImage;
const
  AdjRow: array [0..8] of Integer= (-1, -1, -1, 0, 0, 0, +1, +1, +1);
  AdjCol: array [0..8] of Integer= (+1,  0, -1, +1, 0, -1, +1, 0, -1);

var
  NewBodyPtr: PInteger;
  NewBody: TArrayofArrayofInt;
  S, r, c,
  i, j: Integer;

begin
  SetLength (NewBody, Row- 2);

  for i:= 0 to Row- 3 do
  begin
    SetLength (NewBody [i], Column- 2);
    NewBodyPtr:= @NewBody [i, 0];
    
    for j:= 0 to Column- 3 do
    begin
      NewBodyPtr^:= White;
      Inc (NewBodyPtr);
      
    end;
      
  end;

  for r:= 1 to Row- 2 do
  begin
    NewBodyPtr:= @NewBody [r, 0];
    
    for c:= 1 to Column- 2 do
    begin
      S:= 0;
      for i:= 0 to 8 do
        if (Body [r+ AdjRow [i], c+ AdjCol [i]]= BLACK) then
          Inc (S);

      if S= 9 then
        NewBodyPtr^:= BLACK
      else
        NewBodyPtr^:= WHITE;
        
      Inc (NewBodyPtr);

    end;

  end;

  for r:= 0 to Row- 1 do
    SetLength (FBody [r], 0);
  SetLength (FBody, 0);

  FBody:= NewBody;
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

  FCenterOfMass:= TPoint.Create (0, 0);
  for i:= 0 to PixelCollection.Count- 1 do
  begin
    Pixel:= PixelCollection.GetPixel (i);
    FBody [Pixel.Location.r- MinR, Pixel.Location.c- MinC]:= BLACK;
    FCenterOfMass.Move (Pixel.Location);
    FCenterOfMass.Move (-MinR, -MinC);


  end;

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
  FRow:= 0; FColumn:= 0; FPattern:= UnImportantPattern;

  if ComponentCollection.Count= 0 then
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
  MaxPoint.Free;

  FCenterOfMass:= TPoint.Create (0, 0);
  
  for i:= 0 to ComponentCollection.Count- 1 do
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

procedure TFMLImage.SetPixelBlack (r, c: Integer);
begin
  if (FRow<= r) or (r< 0) and
  (FColumn<= c) or (c< 0) then
    raise ERangeCheckError.Create ('SetPixelBlack');

  FBody [r, c]:= BLACK;

end;

procedure TFMLImage.SetPixelBlack (Point: TPoint);
begin
  Self.SetPixelBlack (Point.r, Point.c);
    
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

  FCenterOfMass.Free;

  inherited;
  
end;

function TFMLImage.FindAllComponentsInBox (TopLeft, BottomRight: TPoint;
            UseDialateBeforeExtraction: Boolean): TComponentCollection;
var
  NewImage: TFMLImage;
  P1, P2: TPoint;

begin
  P1:= TPoint.Create (Min (Row- 1, TopLeft.r),
      Min (Column- 1, TopLeft.c));
  P2:= TPoint.Create (Min (Row- 1, BottomRight.r),
      Min (Column- 1, BottomRight.c));
      
  NewImage:= Self.Copy (P1, P2);
  P1.Free;
  P2.Free;

  
(*$IFDEF DEBUG_MODE*)
   NewImage.SaveAsText ('C:\TempNormal.txt');
(*$ENDIF*)

  if UseDialateBeforeExtraction then
  begin
    NewImage.Dilate;

 (*$IFDEF DEBIG_MODE*)
    NewImage.SaveAsText ('C:\TempDilate.txt');
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
  Sr, Er, Sc, Ec: Integer;

begin
  Sr:= Max (0, TopLeft.r);
  Er:= Min (BottomRight.r, FRow- 1);
  Sc:= Max (0, TopLeft.c);
  Ec:= Min (BottomRight.c, FColumn- 1);

  Result:= TFMLImage.Create;
  Result.FImageType:= FImageType;
  Result.Row:= Er- Sr+ 1;
  Result.Column:= Ec- Sc+ 1;

  Newr:= 0;

  for r:= Sr to Er do
  begin
    SourcePixPtr:= ScanLine [r];
    TargetPixPtr:= Result.ScanLine [Newr];

    Inc (SourcePixPtr, Sc);

    for c:= Sc to Ec do
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
//  Self.SaveAsBitmap ('C:\Find.bmp');
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
        ActiveComponent.Add (0, c);

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

    if PixPtr^= BLACK then
    begin
      if UpPixPtr^= Black then
        TempCollection.FComponents [LastRowPtr^].Add (r, 0)
      else
      begin
        ActiveComponent:= TComponent.Create (TempCollection.Count);
        ActiveComponent.Add (r, 0);

        TempCollection.AddComponent (ActiveComponent);
        CurRowPtr^:= TempCollection.Count- 1;
        
      end;

    end;

    for c:= 1 to FColumn- 1 do
    begin
      LeftPixPtr:= PixPtr;
      Inc (PixPtr);
      Inc (UpPixPtr);
      
      Inc (CurRowPtr);
      Inc (LastRowPtr);

      if PixPtr^= BLACK then
      begin

        if (LeftPixPtr^= BLACK) and (LastRow [c]<> CurRow [c- 1]) then
        begin
          TempCollection.FComponents [CurLeftPtr^].Add (r, c);
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
          TempCollection.FComponents [LastRowPtr^].Add (r, c);
          CurRowPtr^:= LastRowPtr^;
                        
        end
        else 
        begin
          ActiveComponent:= TComponent.Create (TempCollection.Count);
          ActiveComponent.Add (r, c);

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

procedure TFMLImage.SaveAsText (FileName: String; PrintBodyValue: Boolean);
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
    
  end;
  CloseFile (OutputFile);
  
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

function TFMLImage.ExtractFeatures (NewSize: Integer; SmoothDegree: Integer;
       NumberOfMasks: Integer): TFeatureVectorBasedOnGradiant;
var
  ResizedImage,
  SmoothedImage: TFMLImage;
  GradiantIn8Dir: T8DirGradiantFeature;

begin
  ResizedImage:= Self.NewResize (NewSize, NewSize);
 (*$IFDEF DEBUG_MODE*)
  ResizedImage.SaveAsText ('Resized.00.txt');
 (*$ENDIF DEBUG_MODE*)
  
  SmoothedImage:= ResizedImage.Smooth (SmoothDegree);
 (*$IFDEF DEBUG_MODE*)
  SmoothedImage.SaveAsText ('Smoothed.00.txt');
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
      (*$IFDEF DEBUG_MODE*)
        TempImage.SaveAsText ('R'+ IntToStr (i- 1)+ '.txt');
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
        SourcePtr^:= (1- SourcePtr^)* 255;
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


procedure TFMLImage.SetPattern (const Value: Integer);
begin
  FPattern := Value;
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
//    Self.SaveAsText ('C:\Temp.txt');
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

  if ImageType= it8bit then
    BlacknessThr:= GrayThreshold
  else if ImageType= itMonoChrome then
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

  if ImageType= it8bit then
    BlacknessThr:= GrayThreshold
  else if ImageType= itMonoChrome then
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
  DisPlacement: Integer;
  SourcePtr, DestPtr: PInteger;

begin
  DisPlacement:= BotIndex- TopIndex;
  
  for r:= 0 to Row- 1 do
  begin
    SourcePtr:= ScanLine [r];
    DestPtr:= ScanLine [r];
    
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
  CosA:= Cos (AngleInDeg* Pi/ 180);
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
    TranPtr:= @Trans [r][0];

    for C:= MinC to MaxC- 1 do
    begin
      TValue:= TempArrPtr^;{ (TempArrPtr^* TranPtr^+
        (MaxTrans- TranPtr^)* SrcPixPtr^)/ MaxTrans;}
      PixPtr^:= Round (TValue);

      Inc (SrcPixPtr);
      Inc (TempArrPtr);
      Inc (TranPtr);
      Inc (PixPtr);
      
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

function TFMLImage.ConvertToBinary: TFMLImage;
var
  Threshold: Integer;
  r, c: Integer;
  SourcePtr, TargetPtr: PInteger;
begin
  Threshold:= GrayThreshold;
  Result:= TFMLImage.Create;
  Result.FImageType:= itMonoChrome;
  Result.Column:= Column;
  Result.Row:= Row;

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
        PLongWord (CurRowPropPtr^)^:= PLongWord (CurRowPropPtr^)^ or Ord (iclDown);

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
        PLongWord (CurColPropPtr^^)^:= PLongWord (CurColPropPtr^^)^ or ord (iclRight);

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
        PLongWord (CurColPropPtr^^)^:= PLongWord (CurColPropPtr^^)^ or Ord (iclLeft);

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
    LastRowPropPtr,
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
      LastRowPropPtr:= ActiveRow.GetPointerToFirst;
      Inc (ActiveRowPtr);
      ActiveRow:= TARowOfFreemanFeature (ActiveRowPtr^);
      CurRowPropPtr:= ActiveRow.GetPointerToFirst;
      PixPtr:= ScanLine [r];

      for c:= 0 to FColumn- 1 do
      begin
        if PixPtr^= BLACK then
          PLongWord (CurRowPropPtr^)^:= Ord (iclBlackPixInFreeman);
          
        Inc (LastRowPropPtr);
        Inc (CurRowPropPtr);
        Inc (PixPtr);

      end;

    end;
    
  end;

var
  i: Integer;
  r, c: Integer;
  ARowOfFreemanFeature: TARowOfFreemanFeature;

begin
  Result:= TFreemanFeature.Create (FRow, FColumn, (100* BlackPixCount) div (FRow* FColumn));

  for r:= 0 to FRow- 1 do
  begin
    ARowOfFreemanFeature:= TARowOfFreemanFeature.Create;
    ARowOfFreemanFeature.FillWithZero (FColumn);
      
    Result.AddNextRowOfFreemanFeature (ARowOfFreemanFeature);
      
  end;

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
  if (Index< 0) or (Length (FComponents)<= Index) then
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
    
    if Result.c< TempPoint.c then
      Result.c:= TempPoint.c;

    if Result.r< TempPoint.r then
      Result.r:= TempPoint.r;

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
    
    if TempPoint.c< Result.c then
      Result.c:= TempPoint.c;

    if TempPoint.r< Result.r then
      Result.r:= TempPoint.r;
      
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

    if (MaxPoint.c- MinPoint.c< WidthThr) or (MaxPoint.r- MinPoint.r< HeightThr) then
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

function TFMLImage.GetCenterOfMass (Recalc: Boolean= False): TPoint;
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

procedure TFMLImage.SaveInFMLFile (Filename: String);
var
  FMLImageCollection: TImageCollection;

begin
  FMLImageCollection:= TImageCollection.Create;
  FMLImageCollection.AddImage (Self);
  FMLImageCollection.SaveToFile (Filename);
  FMLImageCollection.Free (False);

end;

function TFMLImage.NewResize (NewRow, NewColumn: Integer;
  SaveAspectRatio: Boolean): TFMLImage;
{
  This correctness of this function is tested
    just for enlargements, but not for extraction.
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
  Temp: Double;
  
begin
  if SaveAspectRatio then
  begin
    if (NewRow/ FRow)* FColumn< NewColumn then
      NewColumn:= Round ( (NewRow/ FRow)* FColumn)
    else
      NewRow:= Round ( (NewColumn/ FColumn)* FRow)
      
  end;

  SetLength (NewBody, NewRow+ 1);
  for r:= 0 to NewRow do
  begin
    SetLength (NewBody [r], NewColumn+ 1);
    FillChar (NewBody [r, 0], (NewColumn+ 1)* SizeOf (Double), 0);

  end;

  RowCoef:= NewRow/ FRow;
  ColCoef:= NewColumn/ FColumn;
  StartR:= -RowCoef; EndR:= 0;

  for r:= 0 to FRow- 1 do
  begin
    RowPtr:= Self.ScanLine [r];

    StartR:= StartR+ RowCoef;
    EndR:= EndR+ RowCoef;
    IntStartR:= Trunc (StartR);
    IntEndR:= Trunc (EndR- 1e-10);

    StartC:= 0; EndC:= ColCoef;
    IntStartC:= Trunc (StartC);
    IntEndC:= Trunc (EndC- 1e-10);

    for c:= 0 to FColumn- 1 do
    begin
      if C= -1 then
       Break;

      Dummy:= RowPtr^;

{Calculates A+B+C+D...+O's Areas}
      CommonArea:= 1;
      for yc:= IntStartR to IntEndR do
        for xc:= IntStartC to IntEndC do
        begin
          NewBody [yc, xc]:= NewBody [yc, xc]+
            CommonArea* Dummy;

        end;

{Remove the 1,2 and 3's Areas from the Newbodies (Some parts of these areas)}
        CommonHeight:= IntStartR- StartR;//CommonHeight is always <= 0
        yc:= IntStartR;
      
{1}        NewBody [IntStartR, IntStartC]:= NewBody [IntStartR, IntStartC]+
          CommonHeight* (1+ IntStartC- StartC)* Dummy;
        CommonArea:= CommonHeight;{* 1}
{2}
        for xc:= IntStartC+ 1 to IntEndC- 1 do
        begin
          NewBody [yc, xc]:= NewBody [yc, xc]+
            CommonArea* Dummy;

        end;
{3}        NewBody [IntStartR, IntEndC]:= NewBody [IntStartR, IntEndC]+
          CommonHeight* (EndC- IntEndC)* Dummy;

  {Remove the 7,8 and 9's Areas from the Newbodies (Some parts of these areas)}
        CommonHeight:= EndR- (IntEndR+ 1);//CommonHeight is always <= 0
        yc:= IntEndR;

{9}     NewBody [IntEndR, IntStartC]:= NewBody [IntEndR, IntStartC]+
          CommonHeight* (1+ IntStartC- StartC)* Dummy;
        CommonArea:= CommonHeight;{* 1}

{8}
        for xc:= IntStartC+ 1 to IntEndC- 1 do
        begin
          NewBody [yc, xc]:= NewBody [yc, xc]+
            CommonArea* Dummy;

        end;
        
{7}        NewBody [IntEndR, IntEndC]:= NewBody [IntEndR, IntEndC]+
          CommonHeight* (EndC- IntEndC)* Dummy;

  {Remove the 10, 11, 12's Areas from the Newbodies (Some other parts of these areas)}
        CommonWidth:= IntStartC- StartC;//CommonHeight is always <= 0
        xc:= IntStartc;
        
{12}        NewBody [IntStartR, xc]:= NewBody [IntStartR, xc]+
          CommonWidth* (1+ IntStartR- StartR) * Dummy;
          
        CommonArea:= CommonWidth;{* 1}
        for yc:= IntStartR+ 1 to IntEndR- 1 do
        begin
{11}          NewBody [yc, xc]:= NewBody [yc, xc]+
            CommonArea* Dummy;

        end;
{10}        NewBody [IntEndR, xc]:= NewBody [IntEndR, xc]+
          CommonWidth* (EndR- IntEndR)* Dummy;

  {Remove the 4, 5, 6's Areas from the Newbodies (Some other parts of these areas)}
        CommonWidth:= EndC- IntEndC- 1;//CommonWidth is always <= 0
        xc:= IntEndC;

{4}        NewBody [IntStartR, xc]:= NewBody [IntStartR, xc]+
          CommonWidth* (IntStartR+ 1- StartR) * Dummy;
        CommonArea:= CommonWidth;{* 1}
        for yc:= IntStartR+ 1 to IntEndR- 1 do
        begin
{5}
          NewBody [yc, xc]:= NewBody [yc, xc]+
            CommonArea* Dummy;

        end;
{6}
        NewBody [IntEndR, xc]:= NewBody [IntEndR, xc]+
          CommonWidth* (EndR- IntEndR) * Dummy;

        //---------------------------//
        NewBody [IntEndR, IntEndC]:= NewBody [IntEndR, IntEndC]-
          (IntEndR+ 1- EndR)* (IntEndC+ 1- EndC)*  Dummy;
        
        NewBody [IntEndR, IntStartC]:= NewBody [IntEndR, IntStartC]-
          (IntEndR+ 1- EndR)* (StartC- IntStartC)* Dummy;

        NewBody [IntStartR, IntEndC]:= NewBody [IntStartR, IntEndC]-
          (StartR- IntStartR)* (IntEndC+ 1- EndC)*  Dummy;

        NewBody [IntStartR, IntStartC]:= NewBody [IntStartR, IntStartC]-
          (StartR- IntStartR)* (StartC- IntStartC)* Dummy;
        
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
  Result.Column:= NewColumn;
  Result.Row:= NewRow;

  Temp:= 0;
  for r:= 0 to NewRow- 1 do
  begin
    ExPtr:= @NewBody [r, 0];
    RowPtr:= Result.ScanLine [r];

    for c:= 0 to NewColumn- 1 do
    begin
      if c= -1 then
        Break;
      Temp:= Temp+ ExPtr^;
      RowPtr^:= Round (ExPtr^);
      Inc (RowPtr);
      Inc (ExPtr);

    end;

  end;

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

end.
