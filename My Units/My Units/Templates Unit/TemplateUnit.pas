unit TemplateUnit;
{
  This unit is used in
    SepantaFormProcessor.dpr,
    VoteRecognizer.dpr
}

interface
uses
  CollectionUnit, ComponentsUnit, GeometryUnit,
    SVMClientUnit, FMLImage;

type
  TGuideBoxInfo= class (TObject)
  private
    FBottomRightPoint: TPoint;
    FCaption: String;
    FTopLeftPoint: TPoint;

  public
    property Caption: String read FCaption;
    property TopLeftPoint: TPoint read FTopLeftPoint;
    property BottomRightPoint: TPoint read FBottomRightPoint;

    constructor Create; overload;
    constructor Create (TLP, BRP: TPoint); overload;
    procedure Free;

    procedure LoadFromFile (var InputFileHandle: TextFile);
    function FindDisplacement (Image: TFMLImage): TPoint;

  end;

  TGuideBoxCollection= class (TBaseCollection)
  private
    function GetGuideBox(Index: Integer): TGuideBoxInfo;
  public
    property GuideBox [Index: Integer]: TGuideBoxInfo read GetGuideBox;

    procedure Free (FreeObj: Boolean= False);

    procedure LoadFromFile (var InputFile: TextFile);
    //skip the first line and then  
    // Read From InputFile till it reaches an empty line
    
  end;

  TLineInfo= record
    rTopLeft,
    cTopLeft,
    rTopRight,
    cTopRight: Integer;
    Len: Integer;
        
  end;

  TBoxInfo= record
    rTopLeft,
    cTopLeft,
    rTopRight,
    cTopRight,
    rBotRight,
    cBotRight: Integer;
        
  end;

  TSkewReplacementInfo= record
    Replacement: TPoint;
    Skew: TPoint;/// Skew.x/ Skew.y is tangent of skew angle
    SkewSource: TPoint;//the skew and Replacement is calced  based on this point.

  end;

  TSearchBoxCollection= class (TBoxCollection)
  private
    function GetSearchPoints(Index: Integer): TBox;
  public
    property SearchPoint [Index: Integer]: TBox read GetSearchPoints;
    procedure LoadFromFile (var InputFile: TextFile);
    //skip the first line and then
    // Read From InputFile till it reaches an empty line

  end;

  TGeneralComponentCollection= class (TBaseCollection)
  private
    FTopLeft, FBotRight: TPoint;
    FGuideBoxCollection: TGuideBoxCollection;
    FSearchBoxCollection: TSearchBoxCollection;

  published
    procedure AddNewComponent (Component: TComponent); virtual;

  public
    constructor Create;
    procedure Free;

    function IsComponentMine (Component: TComponent; OriginPos: TPoint= nil): Boolean; virtual; abstract;
    procedure LoadFromFile (var InputFile: TextFile); virtual; abstract;
    function Recognize (SVMClient: TSVMClient): String; virtual; abstract;

  end;
  
  TGeneralImageCollection= class (TBaseCollection)
  published
    MinImageProperties: TPoint;
    MaxImageProperties: TPoint;
    PointsInImage: TPoint;//x is for min and y is for max
    FSearchPoints: TSearchBoxCollection;
    FGuideBoxes: TGuideBoxCollection;
    FMainBoxBorder: TBox;

  public
    constructor Create; overload;
    procedure Free (FreeObj: Boolean= False); 

    procedure LoadFromConfigFile (FileName: String); virtual; abstract;
    procedure LoadBitmap (BitmapFilename: String); virtual; abstract;

  end;
  
implementation

uses
  ExceptionUnit, MyTypes, SysUtils;



{ TGeneralImageCollection }

constructor TGeneralImageCollection.Create;
begin
  inherited;

  MinImageProperties:= nil;
  MaxImageProperties:= nil;
  PointsInImage:= nil;
  FSearchPoints:= nil;
  FGuideBoxes:= nil;
  FMainBoxBorder:= nil;

end;

procedure TGeneralImageCollection.Free (FreeObj: Boolean);
begin
  if MinImageProperties<> nil then
    MinImageProperties.Free;
  if MaxImageProperties<> nil then
    MaxImageProperties.Free;
  if PointsInImage<> nil then
    PointsInImage.Free;
  if FSearchPoints<> nil then
    FSearchPoints.Free;
  if FGuideBoxes<> nil then
    FGuideBoxes.Free;
  if FMainBoxBorder<> nil then
    FMainBoxBorder.Free;

  inherited Free (FreeObj);
  
end;

{ TGeneralComponentCollection }

procedure TGeneralComponentCollection.AddNewComponent (
  Component: TComponent);
begin
  inherited Add (Component);
  
end;

constructor TGeneralComponentCollection.Create;
begin
  inherited;

  FTopLeft:= TPoint.Create;
  FBotRight:= TPoint.Create;
  FGuideBoxCollection:= TGuideBoxCollection.Create;
  
end;

procedure TGeneralComponentCollection.Free;
begin
  FTopLeft.Free;
  FBotRight.Free;
  FGuideBoxCollection.Free;
  
  inherited Free (False);

end;

{ TGuideBoxInfo }

constructor TGuideBoxInfo.Create;
begin
  inherited;
  
  FTopLeftPoint:= nil;
  FBottomRightPoint:= nil;

end;

constructor TGuideBoxInfo.Create (TLP, BRP: TPoint);
begin
  inherited Create;

  FTopLeftPoint:= TLP.Copy;
  FBottomRightPoint:= BRP.Copy;
  
end;

function TGuideBoxInfo.FindDisplacement (Image: TFMLImage): TPoint;
begin
  raise ENotImplemented.Create ('FindDisplacement');
  
end;

procedure TGuideBoxInfo.Free;
begin
  FTopLeftPoint.Free;
  FBottomRightPoint.Free;
  
  inherited;

end;

procedure TGuideBoxInfo.LoadFromFile (var InputFileHandle: TextFile);
var
  S: String;
  
begin
  if FTopLeftPoint<> nil then
    FTopLeftPoint.Free;
  if FBottomRightPoint<> nil then
    FBottomRightPoint.Free;

  Readln (InputFileHandle, FCaption);

  Readln (InputFileHandle, S);
  FTopLeftPoint:= TPoint.Create (S);
  
  Readln (InputFileHandle, S);
  FBottomRightPoint:= TPoint.Create (S);

end;

{ TGuideBoxCollection }

procedure TGuideBoxCollection.Free (FreeObj: Boolean);
var
  Ptr: PObject;
  i: Integer;

begin
  Ptr:= GetPointerToFirst;

  for i:= 0 to Size- 1 do
  begin
    TGuideBoxInfo (Ptr^).Free;
    Inc (Ptr);

  end;

end;

function TGuideBoxCollection.GetGuideBox (Index: Integer): TGuideBoxInfo;
begin
  Result:= Member [Index] as TGuideBoxInfo;
  
end;

procedure TGuideBoxCollection.LoadFromFile (var InputFile: TextFile);
var
  S: String;
  TLPoint, BRPoint: TPoint;
  NewBox: TGuideBoxInfo;
  Count: Integer;
  
begin
  ReadLn (InputFile, Count);
  ReadLn (InputFile, S);
  S:= TrimLeft (TrimRight (S));

  while 0< Count do
  begin
    Dec (Count);

    ReadLn (S);
    TLPoint:= TPoint.Create (S);

    ReadLn (InputFile, S);
    BRPoint:= TPoint.Create (S);

    NewBox:= TGuideBoxInfo.Create (TLPoint, BRPoint);
    Self.Add (NewBox);

  end;

end;

{ TSearchBoxCollection }

function TSearchBoxCollection.GetSearchPoints (Index: Integer): TBox;
begin
  Result:= Member  [Index] as TBox;
    
end;

procedure TSearchBoxCollection.LoadFromFile (var InputFile: TextFile);
var
  S: String;
  TLPoint, BRPoint: TPoint;
  NewBox: TBox;
  Count: Integer;
  
begin
  ReadLn (InputFile, Count);

  while 0< Count do
  begin
    Dec (Count);
    ReadLn (InputFile, S);
    TLPoint:= TPoint.Create (S);

    ReadLn (InputFile, S);
    BRPoint:= TPoint.Create (S);
    NewBox:= TBox.Create (TLPoint, BRPoint);
    Self.Add (NewBox);

  end;
  
end;

end.
