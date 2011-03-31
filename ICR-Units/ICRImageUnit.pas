unit ICRImageUnit;

interface
uses
  Classes, SysUtils, FMLImage, GeometryUnit, Graphics,
  StreamUnit, CollectionUnit, RecognitionEngineUnit,
    StorageUnit;

const
  ICRDebugMode= 0;
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

  TICRImage= class (TColoredImage)
  private
//    RefImageColumns: Integer;
    RefImageRows: Integer;
    HorizontalTagWidth,
    HorizontalTagHeight: Integer;
    MostTopHorizontalTagRowIndex: Integer;
    MostBotHorizontalTagRowIndex: Integer;
    HorizontalTagCol: Integer;
    NoOfHorizontalTags: Integer;
    VerticalTagHeight: Integer;
    MostLeftVerticalTagColIndex: Integer;
    MostRightVerticalTagColIndex: Integer;
    NoOfVerticalTags: Integer;

    VerticalPositionTag: TPointCollection;
    HorizontalPositionTopLeft: TPointCollection;
    HorizontalPositionBotRight: TPointCollection;
    AllBoxInfo: TBoxCollection;
    FImageCollection: TImageCollection;

    constructor Create; overload;

    procedure FindTagsPosition (AnImage: TFMLImage;
      ConfigManager: TConfigManager);
    
  protected
    function NewInstance: TBaseImage; override;

  public
    constructor Create (Image: TPicture); overload;
    destructor Destroy; override;

    function GetCleanedMonoImage: TFMLImage;

    procedure RemoveColorsSatisfyCondition (Fn: TIntegerBoolean);
    function Copy (TL, BR: TPoint): TICRImage;

    function SegmentAndRecognize (RecognitionEngine: TRecognitionEngine;
      ConfigManager: TConfigManager):
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
    Result:= 0;
    
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

function TICRImage.Copy (TL, BR: TPoint): TICRImage;
begin
  Result:= inherited CopyPixels (TL, BR) as TICRImage;
  
end;

constructor TICRImage.Create (Image: TPicture);
begin
  inherited Create (it24bit);

//  Image.Bitmap.SaveToFile ('C:\MainBitmap.bmp');
  LoadBitMap (Image.Bitmap);
  FImageCollection:= nil;
    
end;

constructor TICRImage.Create;
begin
  inherited Create (it24bit);

  FImageCollection:= nil;
  VerticalPositionTag:= nil;
  HorizontalPositionTopLeft:= nil;
  HorizontalPositionBotRight:= nil;
  AllBoxInfo:= nil;

end;

destructor TICRImage.Destroy;
begin
  FImageCollection.Free;
  VerticalPositionTag.Free;
  HorizontalPositionTopLeft.Free;
  HorizontalPositionBotRight.Free;

  inherited;
  
end;

procedure TICRImage.FindTagsPosition (AnImage: TFMLImage;
            ConfigManager: TConfigManager);
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

var
  VisitedCs: array of Integer;
  i, j: Integer;
  Flag: Boolean;

begin
  BLACK:= AnImage.GetBlackColor;
  WHITE:= AnImage.GetWhiteColor;
  HorizontalPositionTopLeft:= TPointCollection.Create;
  HorizontalPositionBotRight:= TPointCollection.Create;
  VerticalPositionTag:= TPointCollection.Create;
  HorizontalPositionTopLeft.Allocate (NoOfHorizontalTags);
  HorizontalPositionBotRight.Allocate (NoOfHorizontalTags);
  VerticalPositionTag.Allocate (NoOfVerticalTags);

  Index:= 0;
  SetLength (VisitedCs, 0);

  while True do
  begin

    for i:= 0 to NoOfHorizontalTags- 1 do
    begin
      Flag:= True;

      for j:= 0 to High (VisitedCs) do
        if  (ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [i].Right].ColIndex-
        ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [i].Left].ColIndex) div 2
          >Abs (ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [i].Left].ColIndex+
        ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [i].Right].ColIndex) div 2- VisitedCs [j] then
      begin
        Flag:= False;
        Break;

      end;

      if Flag then
      begin
        SetLength (VisitedCs, Length (VisitedCs)+ 1);
        c:= (ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [i].Left].ColIndex+
        ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [i].Right].ColIndex) div 2;
        VisitedCs [High (VisitedCs)]:= c;
        Break;

      end;

    end;

    if not Flag then
      Break;

    for r:= Max (MostTopHorizontalTagRowIndex- 100, 0) to Min (MostBotHorizontalTagRowIndex+ 101, AnImage.Row)- 1 do
    begin
      PixPtr:= AnImage.ScanLine [r];
      Inc (PixPtr, c);

      if PixPtr^= BLACK then
      begin
        ComponentInfo:= FindComponentAt (r, c, AnImage);

        if (Abs (ComponentInfo.Width- HorizontalTagWidth)/ HorizontalTagWidth< 0.2) and
          (4< ComponentInfo.Heigth) then
        begin
          HorizontalPositionTopLeft.Member [Index]:= TPoint.Create (ComponentInfo.MinR, ComponentInfo.MinC);
          HorizontalPositionBotRight.Member [Index]:= TPoint.Create (ComponentInfo.MaxR, ComponentInfo.MaxC);
          Inc (Index);
            
        end

      end;

    end;

  end;

  if Index<> NoOfHorizontalTags then
    raise Exception.Create ('');

  if NoOfVerticalTags<> 0 then
  begin
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
  
end;

function TICRImage.GetCleanedMonoImage: TFMLImage;
begin
  RemoveColorsSatisfyCondition (@IsRed);
  Result:= ConvertToBinary;

end;

function TICRImage.NewInstance: TBaseImage;
begin
  Result:= TICRImage.Create;
  
end;

procedure TICRImage.RemoveColorsSatisfyCondition(
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
      if (r= 0) and (c= 0) then
      ;

      if Fn (PixPtr^) then
        PixPtr^:= WHITE;
      Inc (PixPtr);
      
    end;

  end;

end;

function TICRImage.SegmentAndRecognize (RecognitionEngine:
          TRecognitionEngine; ConfigManager: TConfigManager):
TStringList;
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

  procedure ConfigureParameters;
  var
    i: Integer;

  begin
    NoOfHorizontalTags:= ConfigManager.RowIndicators.Size;
    NoOfVerticalTags:= 0;
    HorizontalTagWidth:= ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [0].Right].ColIndex-
      ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [0].Left].ColIndex+ 1;
    HorizontalTagHeight:= ConfigManager.HorizontalLines.HorizontalLine [ConfigManager.RowIndicators.RowIndicator [0].Bot].RowIndex-
      ConfigManager.HorizontalLines.HorizontalLine [ConfigManager.RowIndicators.RowIndicator [0].Top].RowIndex+ 1;

    MostTopHorizontalTagRowIndex:= ConfigManager.HorizontalLines.HorizontalLine [ConfigManager.RowIndicators.RowIndicator [0].Top].RowIndex;
    MostBotHorizontalTagRowIndex:= ConfigManager.HorizontalLines.HorizontalLine [ConfigManager.RowIndicators.RowIndicator [0].Bot].RowIndex;

    HorizontalTagCol:= (ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [0].Left].ColIndex+
                       ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [0].Right].ColIndex) div 2;
    for i:= 1 to ConfigManager.RowIndicators.Size- 1 do
    begin
      if ConfigManager.HorizontalLines.HorizontalLine [ConfigManager.RowIndicators.RowIndicator [i].Top].RowIndex<
        MostTopHorizontalTagRowIndex then
        MostTopHorizontalTagRowIndex:= ConfigManager.HorizontalLines.HorizontalLine [ConfigManager.RowIndicators.RowIndicator [i].Top].RowIndex;

      if MostBotHorizontalTagRowIndex< ConfigManager.HorizontalLines.HorizontalLine [ConfigManager.RowIndicators.RowIndicator [i].Bot].RowIndex then
        MostBotHorizontalTagRowIndex:= ConfigManager.HorizontalLines.HorizontalLine [ConfigManager.RowIndicators.RowIndicator [i].Bot].RowIndex;
        
    end;

  end;
  
var
  MonoChromeImage: TFMLImage;
  i, j: Integer;
  BoxInfo: TBoxInfo;
  CellInfo: TCellInfo;
  Deltac: Integer;
  CellImage: TFMLImage;
  BoxResult: Integer;
  
(*$J+*)
const
  TL: TPoint= nil;
  BR: TPoint= nil;
(*$J-*)
  CheckBoxMinThreshold: Extended= 0.50;

begin
  if TL= nil then
  begin
    TL:= TPoint.Create;
    BR:= TPoint.Create;
    
  end;
  
  ConfigureParameters;

  MonoChromeImage:= GetCleanedMonoImage;
  MonoChromeImage.SaveAsBitmap ('C:\Mono.BMP');

  FindTagsPosition (MonoChromeImage, ConfigManager);

  Deltac:= 0;
  for i:= 0 to ConfigManager.RowIndicators.Size- 1 do
  begin
    Inc (Deltac, ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [i].Left].ColIndex
       - HorizontalPositionTopLeft.Point [i].c);
    Inc (Deltac, ConfigManager.VerticalLines.VerticalLine [ConfigManager.RowIndicators.RowIndicator [i].Right].ColIndex
       - HorizontalPositionBotRight.Point [i].c);
       
  end;
  Deltac:= Deltac div (2* ConfigManager.RowIndicators.Size);
  Result:= TStringList.Create;
  
  for i:= 0 to ConfigManager.AllBoxesInfo.Size- 1 do
  begin
    BoxInfo:= ConfigManager.AllBoxesInfo.BoxInfo [i];
//    if BoxInfo.Kind= CheckBox then
    BoxResult:= 0;
    
    for j:= 0 to BoxInfo.Size- 1 do
    begin
      CellInfo:= BoxInfo.Cell [j];

      TL.r:= HorizontalPositionTopLeft.Point [CellInfo.Top div 2].r;
      TL.c:= Deltac+ ConfigManager.VerticalLines.VerticalLine [CellInfo.Left].ColIndex;

      BR.r:= HorizontalPositionBotRight.Point [CellInfo.Bot div 2].r;
      BR.c:= Deltac+ ConfigManager.VerticalLines.VerticalLine [CellInfo.Right].ColIndex;

      CellImage:= MonoChromeImage.Copy (TL, BR);
//      CellImage.SaveAsBitmap ('c:\a.bmp');
      if CellImage.Row* CellImage.Column* CheckBoxMinThreshold< CellImage.BlackPixCountInImage (True) then
        BoxResult:= BoxResult or (1 shl j);
      CellImage.Free;

    end;
    Result.Add (IntToStr (BoxResult));
    
  end;

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
(*$IF ICRDEBUGMODE and 4<> 0 THEN*)
  DebugImCollection: TImageCollection;
(*$IFEND*)
const
  Threshold= 2;
  CheckBoxMinThreshold: Extended= 0.75;
  
begin
  Result:= TStringList.Create;
  Allocate;
(*$IF ICRDEBUGMODE and 4<> 0 THEN*)
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
(*$IF ICRDEBUGMODE and 4<> 0 THEN*)
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

  (*$IF ICRDEBUGMODE and 4<> 0 THEN*)
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
