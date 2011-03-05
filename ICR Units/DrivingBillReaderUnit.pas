unit DrivingBillReaderUnit;

interface
uses
  Classes, SysUtils, FMLImage, GeometryUnit, Graphics,
  StreamUnit, CollectionUnit, RecognitionEngineUnit;

const
  DebugMode= 0;
  {
    1 is for Saving MonoChrome Image.
    2 is for log Vertical and Horizontal Tags.
    4 is Saving the segmented and cropted images of each box.
  }
  

type
  EInvalidFile= class (Exception);
  
  TComponentInfo= record
    Width, Heigth, PointsInCom: Integer;
    MinR, MinC, MaxR, MaxC: Integer;

  end;

  TBoxInfoRegardingTags= class (TObject)
  private
    FVertTagLeftIndex, FVertTagRightIndex,
    FHorzTagTopIndex, FHorzTagBotIndex: Integer;
    FIsHorizontal: Boolean;
    FCellCount: Integer;
    FSize: array of Extended;
    FBoxDataKind: TStringList;
    FValues: TStringList;

    function GetPercentageofSize (Index: Integer): Extended;
    function GetBoxDataKind(Index: Integer): String;

  public
    property VertTagLeftIndex: Integer read FVertTagLeftIndex;
    property VertTagRightIndex: Integer read FVertTagRightIndex;
    property HorzTagTopIndex: Integer read FHorzTagTopIndex;
    property HorzTagBotIndex: Integer read FHorzTagBotIndex;
    property IsHorizontal: Boolean read FIsHorizontal;
    property CellCount: Integer read FCellCount;
    property PercentageOfSize [Index: Integer]: Extended read GetPercentageofSize;
    property BoxDataKind [Index: Integer]: String read GetBoxDataKind;
    property Values: TStringList read FValues;

    procedure LoadFromStream (InputStream: TMyFileStream);
    constructor Create;    
    destructor Destroy; override; 
    
  end;

  TIntegerBoolean= function (Color: Integer): Boolean;
  TBoxInfoRegardingTagsCollection= class (TBaseCollection)
  private
    FBoxImage: TImageCollection;

    function GetBoxInfo (Index: Integer): TBoxInfoRegardingTags;
  public
    property BoxInfo [Index: Integer]: TBoxInfoRegardingTags read GetBoxInfo;

    procedure LoadFromStream (InputStream: TMyFileStream);
    function ExtractImagesAndRecognize (MainImage: TFMLImage; VerticalTagsPosition,
      HorizontalTagsPosition: TPointCollection;
      RecognitionEngine: TRecognitionEngine): TStringList;

  end;

  TDriverBillImage= class (TColoredImage)
  private
    RefImageColumns: Integer;
    RefImageRows: Integer;
    HorizontalTagWidth: Integer;
    MostTopHorizontalTagRowIndex: Integer;
    MostBotHorizontalTagRowIndex: Integer;
    NoOfHorizontalTags: Integer;
    VerticalTagHeight: Integer;
    MostLeftVerticalTagColIndex: Integer;
    MostRightVerticalTagColIndex: Integer;
    NoOfVerticalTags: Integer;
    
    VerticalPositionTag: TPointCollection;
    HorizontalPositionTag: TPointCollection;
    AllBoxInfo: TBoxCollection;
    FImageCollection: TImageCollection;
    FBoxesInfo: TBoxInfoRegardingTagsCollection;

    constructor Create; overload;

    procedure FindTagsPosition (AnImage: TFMLImage);

    function NewInstance: TBaseImage; override;

  public
    constructor Create (Bitmap: TBitmap; InputStream: TMyFileStream); overload;
    destructor Destroy; override;

    function GetCleanedMonoImage: TFMLImage;
    function GetCleanedImage: TColoredImage;
    function GetMonoImge: TFMLImage;
    
    procedure RemoveColorsSatisfyCondition (Fn: TIntegerBoolean);
    function Copy (TL, BR: TPoint): TDriverBillImage;

    function SegmentAndRecognize (RecognitionEngine: TRecognitionEngine):
      TStringList;

  end;
    
implementation

uses
  ComponentsUnit, Math, MyTypes;

function ReadNextLine (InputStream: TMyFileStream): String;
begin
  Result:= InputStream.ReadLine;

  while (Result<> '') and (Result [1]= '#') do
    Result:= InputStream.ReadLine;

end;

function IsRed (AColor: Integer): Boolean;

  function ProbColor (AColor: Byte; x: array of Integer; y: array of Extended): Double;
  var
    i: Integer;

  begin
    for i:= 1 to High (x) do
      if AColor<= x [i] then
      begin
        Result:= ((AColor- x [i- 1])* (y [i]- y [i- 1]))
           / (x [i]- x [i- 1])+ y [i- 1];
        Exit;
        
      end;

  end;

var
  Temp: Extended;

begin
  Temp:= (ProbColor (AColor and $0000FF, [0, 140, 192, 255],
                                          [0, 0.2, 1.0, 1.0])*
           ProbColor ((AColor and $00FF00) shr 8, [0, 140, 220, 255],
                                          [01, 1, 0.2, 0.0])*
           ProbColor ((AColor and $FF0000) shr 16, [0, 140, 220, 255],
                                          [01, 1, 0.2, 0.0]));

  Result:= 0.5< Temp;
  
end;

{ TDriverBillReader }

function TDriverBillImage.Copy (TL, BR: TPoint): TDriverBillImage;
begin
  Result:= inherited CopyPixels (TL, BR) as TDriverBillImage;
  
end;

constructor TDriverBillImage.Create (Bitmap: TBitmap; InputStream: TMyFileStream);

  procedure LoadInfo;
  begin
    RefImageColumns:= StrToInt (ReadNextLine (InputStream));
    RefImageRows:= StrToInt (ReadNextLine (InputStream));
    HorizontalTagWidth:= StrToInt (ReadNextLine (InputStream));
    MostTopHorizontalTagRowIndex:= StrToInt (ReadNextLine (InputStream));
    MostBotHorizontalTagRowIndex:= StrToInt (ReadNextLine (InputStream));
    NoOfHorizontalTags:= StrToInt (ReadNextLine (InputStream));
    VerticalTagHeight:= StrToInt (ReadNextLine (InputStream));
    MostLeftVerticalTagColIndex:= StrToInt (ReadNextLine (InputStream));
    MostRightVerticalTagColIndex:= StrToInt (ReadNextLine (InputStream));
    NoOfVerticalTags:= StrToInt (ReadNextLine (InputStream));

  end;

begin
  inherited Create (it24bit);

  LoadBitMap (Bitmap);
  FImageCollection:= nil;

  LoadInfo;
  FBoxesInfo:= TBoxInfoRegardingTagsCollection.Create;
  FBoxesInfo.LoadFromStream (InputStream);
    
end;

constructor TDriverBillImage.Create;
begin
  inherited Create (it24bit);

  FImageCollection:= nil;
  VerticalPositionTag:= nil;
  HorizontalPositionTag:= nil;
  AllBoxInfo:= nil;

end;

destructor TDriverBillImage.Destroy;
begin
  FImageCollection.Free;
  VerticalPositionTag.Free;
  HorizontalPositionTag.Free;
  
  inherited;
  
end;

procedure TDriverBillImage.FindTagsPosition (AnImage: TFMLImage);
var
  Index,
  r, c: Integer;
  PixPtr: PInteger;
  ComponentInfo: TComponentInfo;


  function FindComponentAt (r, c: Integer; AnImage: TFMLImage): TComponentInfo;
  var
    Index: Integer;

    procedure DFS (r, c: Integer);
    begin
      Inc (Index);

      if Result.MaxR< r then
        Result.MaxR:= r
      else if r< Result.MinR then
        Result.MinR:= r;

      if Result.MaxC< c then
        Result.MaxC:= c
      else if c< Result.MinC then
        Result.MinC:= c;

  //        Color:= AnImage.Body [r, c];
      AnImage.SetPixelColor (r, c, WHITE);

      if (r< FRow- 1) and (AnImage.Body [r+ 1, c]= BLACK) then
        DFS (r+ 1, c);
      if (0< r) and (AnImage.Body [r- 1, c]= BLACK) then
        DFS (r- 1, c);
      if (c< Column- 1) and (AnImage.Body [r, c+ 1]= BLACK) then
        DFS (r, c+ 1);
      if (0< c) and (AnImage.Body [r, c- 1]= BLACK) then
        DFS (r, c- 1);

    end;

  begin
    if AnImage.Body [r, c]= WHITE then
      Result.PointsInCom:= 0
    else
    begin
      Index:= 0;
      Result.MaxC:= c; Result.MinC:= c;
      Result.MaxR:= r; Result.MinR:= r;
      DFS (r, c);
      Result.Width:= Result.MaxC- Result.MinC+ 1;
      Result.Heigth:= Result.MaxR- Result.MinR+ 1;
      Result.PointsInCom:= Index;

    end;

  end;

  
begin
  BLACK:= AnImage.GetBlackColor;
  WHITE:= AnImage.GetWhiteColor;
  HorizontalPositionTag:= TPointCollection.Create;
  VerticalPositionTag:= TPointCollection.Create;
  HorizontalPositionTag.Allocate (NoOfHorizontalTags);
  VerticalPositionTag.Allocate (NoOfVerticalTags);

  Index:= 0;
  c:= AnImage.Column- HorizontalTagWidth div 2;

   for r:= Max (MostTopHorizontalTagRowIndex- 100, 0) to Min (MostBotHorizontalTagRowIndex+ 101, AnImage.Row)- 1 do
  begin
    PixPtr:= AnImage.ScanLine [r];
    Inc (PixPtr, c);

    if PixPtr^= BLACK then
    begin
      ComponentInfo:= FindComponentAt (r, c, AnImage);

      if Abs (ComponentInfo.Width- HorizontalTagWidth)/ HorizontalTagWidth< 0.2 then
      begin
        HorizontalPositionTag.Member [Index]:= TPoint.Create (ComponentInfo.MinR+ ComponentInfo.Heigth div 2,
                                             ComponentInfo.MinC+ ComponentInfo.Width div 2);
        Inc (Index);
            
      end

    end;

  end;

  if Index<> NoOfHorizontalTags then
    raise Exception.Create ('');

  Index:= 0;
  r:= AnImage.Row- VerticalTagHeight div 2;
  PixPtr:= AnImage.ScanLine [r];
  Inc (PixPtr, Max (MostLeftVerticalTagColIndex- 100, 0) );

  for c:= Max (MostLeftVerticalTagColIndex- 100, 0) to Min (MostRightVerticalTagColIndex+ 101, AnImage.Column)- 1 do
  begin
    if PixPtr^= BLACK then
    begin
      ComponentInfo:= FindComponentAt (r, c, AnImage);

      if Abs (ComponentInfo.Heigth- VerticalTagHeight)/ VerticalTagHeight< 0.2 then
      begin
        VerticalPositionTag.Member [Index]:= TPoint.Create (ComponentInfo.MinR+ ComponentInfo.Heigth div 2,
                                             ComponentInfo.MinC+ ComponentInfo.Width div 2);
        Inc (Index);
            
      end;

    end;
    Inc (PixPtr);
        
  end;

  if Index<> NoOfVerticalTags then
    raise Exception.Create ('');
   
end;

function TDriverBillImage.GetCleanedImage: TColoredImage;
var
  r, Activer, Activec: Integer;

  function ExtractContour (r, c: Integer): TComponent;
  const
    AdjancedPixelr: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
    AdjancedPixelc: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
  
  var
    RGBColor: TRGB;
    StartDir: TDirection;
    i: Integer;
  
  begin
    try
      Result:= TComponent.Create;
      RGBColor.r:= 0; RGBColor.g:= 0; RGBColor.b:= 0;
      StartDir:= dE;

      while not Result.HashedData.IsExist (r, c) do
      begin
        Result.Add (r, c, RGBColor);

        StartDir:= TDirection ((Ord (StartDir)+ 5) mod 8);

        for i:= 0 to 7 do
        begin
          if (r+ AdjancedPixelr [StartDir]< FRow) and (0<= r+ AdjancedPixelR [StartDir]) and
             (c+ AdjancedPixelc [StartDir]< FColumn) and (0<= c+ AdjancedPixelc [StartDir]) then
            if IsRed (FBody [r+ AdjancedPixelR [StartDir], c+ AdjancedPixelC [StartDir]]) then
            begin
              Inc (r, AdjancedPixelR [StartDir]);
              Inc (c, AdjancedPixelC [StartDir]);

              Break;

            end;

          StartDir:= TDirection ((Ord (StartDir)+ 1) mod 8);

        end;

      end;

    except
      raise Exception.Create ('Contour');

    end;
  
  end;

var
  Flag: Boolean;
  Comp: TComponent;
  MainImageBox: TDriverBillImage;
(*$J+*)
const
  TL: TPoint= nil;
  BR: TPoint= nil;

(*$J-*)

begin

  Flag:= False;
  Comp:= nil;

  if TL= nil then
  begin
    TL:= TPoint.Create;
    BR:= TPoint.Create;

  end;

  for r:= 0 to (Row- RefImageRows div 2) div RefImageRows do
  begin
    for Activer:= Max (r* RefImageRows- 2, 0) to Min (r* RefImageRows+ RefImageRows div 2+ 3, Row)- 1 do
    begin
      for Activec:= 0 to Column- 1 do
        if IsRed (FBody [Activer, Activec]) then
        begin
          Comp:= ExtractContour (Activer, Activec);

          if (Abs (Comp.Size- 2* 4* (RefImageColumns- RefImageRows))/(8* (RefImageColumns- RefImageRows))< 0.2) and
            (Abs (Comp.Size- 2* 4* (RefImageColumns- RefImageRows))/ Comp.Count< 0.2) and
             (Abs (Comp.Height- RefImageRows)< RefImageRows/ 10) and
             (Abs (Comp.Width- RefImageColumns)< RefImageColumns/ 10) then
          begin
            Flag:= True;
            Break;

          end
          else
          begin
            Comp.Free;
            Comp:= nil;

          end;

        end;

      if Flag then
        Break;

    end;

    if Flag then
      Break;

  end;
      
  if Comp= nil then
    raise EInvalidImage.Create ('Can not find the main box!');

  TL.r:= Comp.MinR; TL.c:= Comp.MinC;
  BR.r:= Comp.MaxR; BR.c:= Comp.MaxC;
  MainImageBox:= Self.Copy (TL, BR) as TDriverBillImage;
  Comp.Free;
  Result:= MainImageBox as  TColoredImage;

end;

function TDriverBillImage.GetCleanedMonoImage: TFMLImage;
var
  r, Activer, Activec: Integer;

  function ExtractContour (r, c: Integer): TComponent;
  const
    AdjancedPixelr: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
    AdjancedPixelc: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
  
  var
    RGBColor: TRGB;
    StartDir: TDirection;
    i: Integer;
  
  begin
    try
      Result:= TComponent.Create;
      RGBColor.r:= 0; RGBColor.g:= 0; RGBColor.b:= 0;
      StartDir:= dE;

      while not Result.HashedData.IsExist (r, c) do
      begin
        Result.Add (r, c, RGBColor);

        StartDir:= TDirection ((Ord (StartDir)+ 5) mod 8);

        for i:= 0 to 7 do
        begin
          if (r+ AdjancedPixelr [StartDir]< FRow) and (0<= r+ AdjancedPixelR [StartDir]) and
             (c+ AdjancedPixelc [StartDir]< FColumn) and (0<= c+ AdjancedPixelc [StartDir]) then
            if IsRed (FBody [r+ AdjancedPixelR [StartDir], c+ AdjancedPixelC [StartDir]]) then
            begin
              Inc (r, AdjancedPixelR [StartDir]);
              Inc (c, AdjancedPixelC [StartDir]);

              Break;

            end;

          StartDir:= TDirection ((Ord (StartDir)+ 1) mod 8);

        end;

      end;

    except
      raise Exception.Create ('Contour');

    end;
  
  end;

var
  Flag: Boolean;
  Comp: TComponent;
  MainImageBox: TDriverBillImage;
(*$J+*)
const
  TL: TPoint= nil;
  BR: TPoint= nil;

(*$J-*)

begin

  Flag:= False;
  Comp:= nil;

  if TL= nil then
  begin
    TL:= TPoint.Create;
    BR:= TPoint.Create;

  end;

  for r:= 0 to (Row- RefImageRows div 2) div RefImageRows do
  begin
    for Activer:= Max (r* RefImageRows- 2, 0) to Min (r* RefImageRows+ RefImageRows div 2+ 3, Row)- 1 do
    begin
      for Activec:= 0 to Column- 1 do
        if IsRed (FBody [Activer, Activec]) then
        begin
          Comp:= ExtractContour (Activer, Activec);

          if (Abs (Comp.Size- 2* 4* (RefImageColumns- RefImageRows))/(8* (RefImageColumns- RefImageRows))< 0.2) and
            (Abs (Comp.Size- 2* 4* (RefImageColumns- RefImageRows))/ Comp.Count< 0.2) and
             (Abs (Comp.Height- RefImageRows)< RefImageRows/ 10) and
             (Abs (Comp.Width- RefImageColumns)< RefImageColumns/ 10) then
          begin
            Flag:= True;
            Break;

          end
          else
          begin
            Comp.Free;
            Comp:= nil;

          end;

        end;

      if Flag then
        Break;

    end;

    if Flag then
      Break;

  end;
      
  if Comp= nil then
    raise EInvalidImage.Create ('Can not find the main box!');

  TL.r:= Comp.MinR; TL.c:= Comp.MinC;
  BR.r:= Comp.MaxR; BR.c:= Comp.MaxC;
  MainImageBox:= Self.Copy (TL, BR) as TDriverBillImage;
  Comp.Free;


  MainImageBox.RemoveColorsSatisfyCondition (@IsRed);

  Result:= MainImageBox.ConvertToBinary;

  MainImageBox.Free;

end;

function TDriverBillImage.GetMonoImge: TFMLImage;
begin

end;

function TDriverBillImage.NewInstance: TBaseImage;
begin
  Result:= TDriverBillImage.Create;
  
end;

procedure TDriverBillImage.RemoveColorsSatisfyCondition(
  Fn: TIntegerBoolean);
var
  r, c: Integer;
  PixPtr: PInteger;

begin
  for r:= 0 to FRow- 1 do
  begin
    PixPtr:= ScanLine [r];

    for c:= 1 to FColumn do
    begin
      if Fn (PixPtr^) then
        PixPtr^:= WHITE;
      Inc (PixPtr);
      
    end;

  end;

end;

function TDriverBillImage.SegmentAndRecognize (RecognitionEngine:
          TRecognitionEngine): TStringList;
var
  WHITE, BLACK: Integer;
  
  function CopyBoxesFrom (AnImage: TFMLImage;
     AllBoxInfo: TBoxCollection): TImageCollection;
  var
    BoxInfo: TBox;
    Ptr: PObject;
    i: Integer;

  begin
    Ptr:= AllBoxInfo.GetPointerToFirst;
    Result:= TImageCollection.Create;
    Result.Allocate (AllBoxInfo.Size);
    
    for i:= 1 to AllBoxInfo.Size do
    begin
      BoxInfo:= Ptr^ as TBox;
      Result.Member [i- 1]:= AnImage.Copy (BoxInfo.TopLeft, BoxInfo.BotRight);

    end;

  end;

var
  MonoChromeImage: TFMLImage;
  
begin
  MonoChromeImage:= GetCleanedMonoImage;

{$IF DebugMode and 1<> 0 THEN}
  MonoChromeImage.Pattern:= 0;
  MonoChromeImage.SaveAsBitmap ('C:\Segmented\Mono.BMP');
{$IFEND}

  FindTagsPosition (MonoChromeImage);
  
{$IF DebugMode and 2<> 0 THEN}
  WriteLn ('Vertical Postion');
  for i:= 0 to VerticalPositionTag.Size- 1 do
    Writeln (VerticalPositionTag.Point [i].ToString);

  WriteLn ('Horizontal Postion');
  for i:= 0 to HorizontalPositionTag.Size- 1 do
    Writeln (HorizontalPositionTag.Point [i].ToString);

{$IFEND}
  Result:= FBoxesInfo.ExtractImagesAndRecognize (MonoChromeImage, VerticalPositionTag,
     HorizontalPositionTag, RecognitionEngine);

{$IF DebugMode and 2<> 0 THEN}
  WriteLn ('Boxes Info');
  for i:= 0 to AllBoxInfo.Size- 1 do
    Writeln (AllBoxInfo.Box [i].TopLeft.ToString, ' ',
       AllBoxInfo.Box [i].BotRight.ToString);

{$IFEND}

  MonoChromeImage.Free;

end;

{ TBoxInfoRegardingTags }

constructor TBoxInfoRegardingTags.Create;
begin
  inherited Create;

  FValues:= TStringList.Create;
  
end;

destructor TBoxInfoRegardingTags.Destroy;
begin
  SetLength (FSize, 0);
  FValues.Free;
  
  inherited;
  
end;

function TBoxInfoRegardingTags.GetBoxDataKind (Index: Integer): String;
begin
  Result:= FBoxDataKind [Index];
  
end;

function TBoxInfoRegardingTags.GetPercentageofSize(Index: Integer): Extended;
begin
  Result:= FSize [Index];
  
end;

procedure TBoxInfoRegardingTags.LoadFromStream (InputStream: TMyFileStream);

var
  i: Integer;
  Sum: Extended;
  S: String;
    
begin
  FVertTagLeftIndex:= StrToInt (ReadNextLine (InputStream));
  FVertTagRightIndex:= StrToInt (ReadNextLine (InputStream));
  FHorzTagTopIndex:= StrToInt (ReadNextLine (InputStream));
  FHorzTagBotIndex:= StrToInt (ReadNextLine (InputStream));
  FIsHorizontal:= UpperCase (ReadNextLine (InputStream))= 'TRUE';
  FCellCount:= StrToInt (ReadNextLine (InputStream));
  SetLength (FSize, FCellCount);
  S:= ReadNextLine (InputStream)+ ' ';
  
  for i:= 1 to FCellCount do
  begin
    FSize [i- 1]:= StrToInt (Copy (S, 1, Pos (' ', S)- 1));
    Sum:= Sum+ FSize [i- 1]; 
    Delete (S, 1, Pos (' ', S));
    S:= TrimLeft (S);

  end;

  S:= Trim (ReadNextLine (InputStream));
  S:= S+ '.';
  FBoxDataKind:= TStringList.Create;
  
  while Pos ('.', S)<> 0 do
  begin
    FBoxDataKind.Add (Copy (S, 1, Pos ('.', S)- 1));
    Delete (S, 1, Pos ('.', S));
    
  end;
  if FBoxDataKind.Count<> FCellCount then
    raise EInvalidFile.Create ('BoxDataKind!');

  
  for i:= 1 to FCellCount do
    FSize [i- 1]:= FSize [i- 1]/ Sum;

end;

{ TBoxInfoRegardingTagsCollection }

function TBoxInfoRegardingTagsCollection.ExtractImagesAndRecognize (
  MainImage: TFMLImage; VerticalTagsPosition,
  HorizontalTagsPosition: TPointCollection;
  RecognitionEngine: TRecognitionEngine): TStringList;
const
(*$J+*)
  TL: TPoint= nil;
  BR: TPoint= nil;
  
(*$J-*)
  procedure Allocate;
  begin
    if TL= nil then
    begin
      TL:= TPoint.Create;
      BR:= TPoint.Create;
    
    end;

  end;

var
  i, j, s, w, h, r, c: Integer;
  ActiveBox: TBox;
  Ptr: PObject;
  ActiveBoxInfo: TBoxInfoRegardingTags;
  IsCheckBox, IsImage: Boolean;
  Temp,
  {MaxPoint4CheckBox, }MaxIndex4CheckBox: Integer;
(*$IF DEBUGMODE and 4<> 0 THEN*)
  DebugImCollection: TImageCollection;
(*$IFEND*)
const
  Threshold= 2;
  CheckBoxMinThreshold: Extended= 0.75;
  
begin
  Result:= TStringList.Create;
  Allocate;
(*$IF DEBUGMODE and 4<> 0 THEN*)
  DebugImCollection:= TImageCollection.Create;
(*$IFEND*)

  s:= 0;
  for i:= 0 to Size- 1 do
    Inc (S, BoxInfo [i].CellCount);

  FBoxImage:= TImageCollection.Create;
  FBoxImage.Allocate (s);

  Ptr:= FBoxImage.GetPointerToFirst;

  for i:= 2 to Size- 1 do
  begin
    ActiveBoxInfo:= BoxInfo [i];
    ActiveBoxInfo.FValues.Add ('Box '+ IntToStr (i+ 1));
    
    TL.r:= HorizontalTagsPosition.Point [ActiveBoxInfo.FHorzTagTopIndex].r+ Threshold;
    TL.c:= VerticalTagsPosition.Point [ActiveBoxInfo.FVertTagLeftIndex].c+ Threshold;
    BR.r:= HorizontalTagsPosition.Point [ActiveBoxInfo.FHorzTagBotIndex].r- Threshold;
    BR.c:= VerticalTagsPosition.Point [ActiveBoxInfo.FVertTagRightIndex].c- Threshold;

    w:= BR.c- TL.c+ 1;
    h:= BR.r- TL.r+ 1;
    
    if ActiveBoxInfo.IsHorizontal then
    begin
      Dec (w, ActiveBoxInfo.CellCount);
      BR.c:= TL.c+ Round (w* ActiveBoxInfo.FSize [0]);
      BR.r:= TL.r+ h;

    end
    else
    begin
      Dec (h, ActiveBoxInfo.CellCount);
      BR.c:= TL.c+ w;
      BR.r:= TL.r+ Round (h* ActiveBoxInfo.FSize [0]);

    end;

    IsCheckBox:= ActiveBoxInfo.BoxDataKind [0]= 'C';
    if IsCheckBox then
    begin
//      MaxPoint4CheckBox:= -1;
      MaxIndex4CheckBox:= 0;
      
    end
    else
      IsImage:= ActiveBoxInfo.BoxDataKind [0]= 'I';

    for j:= 1 to ActiveBoxInfo.CellCount do
    begin

      Ptr^:= MainImage.Copy (TL, BR);
      if IsCheckBox then
      begin
        Temp:= (Ptr^ as TFMLImage).BlackPixCountInImage;

        if //(MaxPoint4CheckBox< Temp) and
           (((Ptr^ as TFMLImage).Row* (Ptr^ as TFMLImage).Column)* CheckBoxMinThreshold< Temp) then
        begin
//          MaxPoint4CheckBox:= Temp;
          MaxIndex4CheckBox:= MaxIndex4CheckBox or (1 shl j);
          
        end;

      end
      else if IsImage then
      begin
      end
      else
      begin
        ActiveBoxInfo.FValues.Add (RecognitionEngine.Classify (Ptr^ as TFMLImage, ActiveBoxInfo.BoxDataKind [j- 1]));
        
      end;
(*$IF DEBUGMODE and 4<> 0 THEN*)
      (Ptr^ as TFMLImage).Pattern:= 0;
      DebugImCollection.AddImage (Ptr^ as TFMLImage);
      
(*$IFEND*)

      Inc (Ptr);

      if ActiveBoxInfo.IsHorizontal then
      begin
        TL.c:= BR.c+ 1;
        if j<> ActiveBoxInfo.CellCount then
          BR.c:= TL.c+ Round (w* ActiveBoxInfo.FSize [j]);

      end
      else
      begin
        TL.r:= BR.r+ 1;
        if j<> ActiveBoxInfo.CellCount then
          BR.r:= TL.r+ Round (h* ActiveBoxInfo.FSize [j]);

      end;

    end;

    if IsCheckBox then
      ActiveBoxInfo.FValues.Add (IntToStr (MaxIndex4CheckBox));

    Result.AddStrings (ActiveBoxInfo.FValues);
    
  end;

  (*$IF DEBUGMODE and 4<> 0 THEN*)
    DebugImCollection.SaveToFile ('C:\Segmented\AllImages.FML');
    DebugImCollection.Clear;
    DebugImCollection.Free;
  (*$IFEND*)
  
end;


function TBoxInfoRegardingTagsCollection.GetBoxInfo (
  Index: Integer): TBoxInfoRegardingTags;
begin
  Result:= inherited Member [Index] as TBoxInfoRegardingTags;
  
end;

procedure TBoxInfoRegardingTagsCollection.LoadFromStream (InputStream: TMyFileStream);
var
  i: Integer;
  n: Integer;
  ActiveBoxInfo: TBoxInfoRegardingTags;
  Ptr: PObject;

begin
  n:= StrToInt (ReadNextLine (InputStream));
  Allocate (n);

  Ptr:= Self.GetPointerToFirst;

  while 0< n do
  begin
    ActiveBoxInfo:= TBoxInfoRegardingTags.Create;
    ActiveBoxInfo.LoadFromStream (InputStream);
    Ptr^:= ActiveBoxInfo;

    Dec (n);
    Inc (Ptr);
    
  end;

end;

end.
