unit ICRUnit;

interface
(*$define ICRUnitDebugMode*)

uses
  Classes, FMLImage, HashUnit, CollectionUnit, ComponentsUnit,
  SVMClientUnit, SysUtils, FeatureUnit, GeometryUnit
  , Graphics, GeneralTypesUnit, PostProcessorClientUnit,
  MyTypes;

type
  EInvalidLocationCheck= class (Exception)
  public
    constructor Create (P1, P2, P3, P4: Integer); overload;
    constructor Create (Str: String); overload;

  end;
  
  ETextNotSet= class (Exception)
  public
    constructor Create;

  end;

  TICRSVMClient= class (TSVMClient)
  private
  public

  end;

  TSkewReplacementInfo= record
    rTopLeft, cTopLeft, rTopRight, cTopRight: Integer;
    Len: Integer;
    
  end;

  TICRRecognizer= class (TObject)
  private
    FSVMSetCollection: TSVMSetCollection;
    FSVMClients: TSVMClients;
    FPostProcessor: TPostProcessorClient;
    FilePath: String;

    function ExtractContour (Image: TFMLImage; r, c: Integer;
      StartingDir: TDirection= dS): TComponent;

    function FindReplacement (Image: TFMLImage;
            LocationChecks: TLocationCheckCollection): TPoint;

    procedure FindRealPositionOfLocationChecks (Image: TFMLImage;
            LocationChecks: TLocationCheckCollection);

    function ExtractAFormItemInfo (Image: TFMLImage;
      AFormItemInfo: TFormItemInfo): TFMLImage;

    function ExtractAndRecognizeFieldSet (Image: TFMLImage;
      AFieldSet: TFieldSet): TImageCollection;

      {
       If it can not determine the class of the ItemImage,
       the function returns false.
      }
    function RecognizeAFormItem (ItemImage: TFMLImage;
      FormItemInfo: TFormItemInfo; SVMClient: TSVMClient;
      SVMSet: TSVMSetInfo; NewSize, SmoothLevel: Integer;
      var SVMRating: TIntegerArray): Boolean;

  public
    constructor Create (SVMSetCollection: TSVMSetCollection;
            PostProcessorInfoCol: TPostProcessorInfoCollection;
         SVMServerHost: String; SVMServerPort: Integer;
         PostProcServerHost: String; PostProcServerPort: Integer;
         FilePath: String);
    destructor Destroy; override;

    procedure Recognize (ABitmap: TBitmap; AFormInfo: TFormInfo; Prefix: String= '');

  end;

  TICREntry= class (TObject)
  private
    FTopLeft, FBotRight: TPoint;
    FComponentCollection: TComponentCollection;
    FBitmap: TBitmap;
    FText: String;

    procedure AddNewComponent (Component: TComponent);
    function GetBitmap: TBitmap;
    function GetText: String;

  public
    property ComponentCollection: TComponentCollection read FComponentCollection;
    property Bitmap: TBitmap read GetBitmap;
    property Text: String read GetText;

    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile (var InputFile: TextFile);
    function IsComponentMine (Component: TComponent; OriginPos: TPoint= nil): Boolean;

    procedure Print (BaseFileName: String);
    function MergeIfNecessary: Boolean;
    function SegmentIfNecessary: Boolean;

    function Recognize (SVMClient: TICRSVMClient): String;

  end;

  TICREntryCollection= class (TBaseCollection)
  private
    MinImagePropertiesForSpecialCase: TPoint;
    FSVMClient: TICRSVMClient;
    FFieldSet: TFieldSet;

    function ExtractImages (NewImage: TFMLImage): TImageCollection;
    function GeTICREntry(Index: Integer): TICREntry;

    function FindReplacement (Image: TFMLImage): TPoint;
    function FindSkewAndReplacement (Image: TFMLImage; Threshold: Extended= 2/3): TSkewReplacementInfo;

  public
    property VoteEntry [Index: Integer]: TICREntry read GeTICREntry;
    property SVMClient: TICRSVMClient read FSVMClient;

    constructor Create (FieldSet: TFieldSet; SVMClient: TICRSVMClient);

    procedure LoadBitmap (Bitmap: TBitmap); overload;
    procedure Load (FileName: string);

    procedure Print;
    procedure RecognizeAll; 

  end;

implementation
uses
  ExceptionUnit, Math, PostProcessorUnit, WideStringUnit;
  
{ TICREntry }

constructor TICREntry.Create;
begin
  inherited;

  FComponentCollection:= TComponentCollection.Create;
  FBitmap:= nil;
  FText:= 'NOT SET YET!';
  
end;

procedure TICREntry.AddNewComponent (Component: TComponent);
var
  i, ReplacementIndex: Integer;
  ComponentsPtr,
  NextComponentsPtr: PComponent;

  
begin
  ComponentsPtr:= FComponentCollection.GetPointerToFirst;

  ReplacementIndex:= FComponentCollection.Size;
  for i:= 0 to FComponentCollection.Size- 1 do
  begin

    if Component.CenterOfMass.c/ Component.Count<
      ComponentsPtr^.CenterOfMass.c/ ComponentsPtr^.Count then
    begin
      ReplacementIndex:= i;
      Break;

    end;

    Inc (ComponentsPtr);

  end;

  FComponentCollection.Add (nil);
  NextComponentsPtr:= FComponentCollection.GetPointerToFirst;
  ComponentsPtr:= NextComponentsPtr;
  if 1< FComponentCollection.Size then
  begin
    Inc (NextComponentsPtr, FComponentCollection.Size- 1);
    Inc (ComponentsPtr, FComponentCollection.Size- 2);

    for i:= FComponentCollection.Size- 2 downto ReplacementIndex do
    begin
      NextComponentsPtr^:= ComponentsPtr^;
      Dec (NextComponentsPtr);
      Dec (ComponentsPtr);

    end;
    
  end;

  NextComponentsPtr^:= Component;
  
end;

destructor TICREntry.Destroy;

begin
  FTopLeft.Free;
  FBotRight.Free;
  FComponentCollection.Free;
  if FBitmap<> nil then
    FBitmap.Free;

  inherited Free;

end;

function TICREntry.IsComponentMine (Component: TComponent; OriginPos: TPoint): Boolean;
var
  Point: TPoint;

begin
  Point:= Component.CenterOfMass.Copy;
  Point.Scale (1/ Component.Count);
  if OriginPos<> nil then
    Point.Move (OriginPos);

  
  if (Point.c< FBotRight.c) and (FTopLeft.c< Point.c) and
    (Point.r< FBotRight.r) and (FTopLeft.r< Point.r) then
  begin
    Result:= True;
    AddNewComponent (Component);
    
  end
  else
    Result:= False;

  Point.Free;

end;

procedure TICREntry.LoadFromFile (var InputFile: TextFile);
var
  S: String;

begin
  Readln (InputFile, S);

  Readln (InputFile, S);
  FTopLeft:= TPoint.Create (S);

  Readln (InputFile, S);
  FBotRight:= TPoint.Create (S);

end;

function TICREntryCollection.ExtractImages (NewImage: TFMLImage): TImageCollection;
const
  BlacknessThr= 250;
{
  procedure SetArrArrDefValue (var ArrayArrayPointer: TArrArrInt; Row, Col, DefVal: Integer);
  var
    r, c: Integer;
    IntPtr: PInteger;

  begin
    SetLength (ArrayArrayPointer, Row);

    for r:= 0 to Row- 1 do
    begin
      SetLength (ArrayArrayPointer [r], Col);

      IntPtr:= @ArrayArrayPointer [r, 0];

      for c:= 0 to Col- 1 do
      begin
        IntPtr^:= DefVal;
        Inc (IntPtr);
      end;

    end;

  end;

  procedure SetArrDefValue (var ArrayPointer: TArrInt; Row, DefVal: Integer);
  var
    r: Integer;
    IntPtr: ^Integer;

  begin
    SetLength (ArrayPointer, Row);

    IntPtr:= @ArrayPointer [0];
    for r:= 0 to Row- 1 do
    begin
      IntPtr^:= DefVal;
      Inc (IntPtr);
    end;

  end;

  procedure FreeArrArr (var ArrayArrayPointer: TArrArrInt);
  var
    r: Integer;

  begin

    for r:= 0 to High (ArrayArrayPointer) do
      SetLength (ArrayArrayPointer [r], 0);
    SetLength (ArrayArrayPointer, 0);

  end;

var
  ComponentCounter: Integer;
  AssignedComponent: TArrArrInt;
  MaxCount, MaxMapIndex: Integer;
  CompXSum, CompYSum: TArrInt;
  MapCount: TArrInt;
  Map: TArrInt;
  BitMap: TBitmap;

  function FindBiggestComponent (Top, Left, But, Right: Integer): TPoint;
  var
    c, r: Integer;
    MinIndex, MaxIndex,
    UpIndex, LeftIndex,
    i: Integer;
    CurPixPtr, LeftPixPtr,
    UpPixPtr: PInteger;
    WhiteInt, BlackInt: Integer;//For simplication in process

  begin
    if Left< 0 then
      Left:= 0;
    if NewImage.FColumn< Right then
      Right:= NewImage.FColumn- 1;
    if Top< 0 then
      Top:= 0;
    if NewImage.Row< But then
      But:= NewImage.Row- 1;



    SetArrArrDefValue (AssignedComponent, But+ 1, Right+ 1, (But- Top+ 1)* (Right- Left+ 1));
    SetArrDefValue (Map, 10000, 0);
    SetArrDefValue (CompXSum, 10000, 0);
    SetArrDefValue (CompYSum, 10000, 0);
    SetArrDefValue (MapCount, 10000, 0);

    MaxCount:= -1;
    ComponentCounter:= 1;
    Map [1]:= 1;
    MapCount [1]:= 0;

    WhiteInt:= 255;
    BlackInt:= 0;

    for r:= Top to But- 1 do
    begin
      CurPixPtr:= @NewImage.FBody [r, 0];
      Inc (CurPixPtr, Left);

      for c:= Left to Right- 1 do
      begin

        if c<> Left then
        begin
          LeftPixPtr:= CurPixPtr;
          Dec (LeftPixPtr);
        end
        else
          LeftPixPtr:= @WhiteInt;// To not satisfy the if conditions

        if r<> Top then
        begin
//          UpPixPtr:= CurPixPtr;??!!
          UpPixPtr:= @NewImage.FBody [r- 1, c];
        end
        else
          UpPixPtr:= @WhiteInt;// To not satisfy the if conditions

        if CurPixPtr^< BlacknessThr then
        begin

          UpIndex:= ComponentCounter;
          if UpPixPtr^< BlacknessThr then
            UpIndex:= AssignedComponent [r- 1, c];

          LeftIndex:= ComponentCounter;
          if LeftPixPtr^< BlacknessThr then
            LeftIndex:= AssignedComponent [r, c- 1];

          MinIndex:= Min (LeftIndex, UpIndex);
          MaxIndex:= Max (LeftIndex, UpIndex);

          if (MinIndex<> MaxIndex) and (MaxIndex<> ComponentCounter) then
          begin
            MinIndex:= Map [MinIndex];

            for i:= 1 to ComponentCounter do
              if Map [i]= MaxIndex then
              begin
                Map [i]:= MinIndex;

                Inc (MapCount [MinIndex], MapCount [i]);
                Inc (CompXSum [MinIndex], CompXSum [i]);
                Inc (CompYSum [MinIndex], CompYSum [i]);

                MapCount [i]:= 0;
                CompYSum [i]:= 0;
                CompXSum [i]:= 0;
              end;

          end;

          AssignedComponent [r, c]:= MinIndex;
          Inc (CompXSum [MinIndex], c);
          Inc (CompYSum [MinIndex], r);

          Inc (MapCount [MinIndex]);

          if MinIndex= ComponentCounter then
          begin
            Map [ComponentCounter]:= ComponentCounter;
            Inc (ComponentCounter);
          end;

          if MaxCount< MapCount [MinIndex] then
          begin
            MaxCount:= MapCount [MinIndex] ;
            MaxMapIndex:= MinIndex;
          end;
        end;

        Inc (CurPixPtr);
      end;
    end;

    for i:= 0 to ComponentCounter do
      if (MapCount [i]<> 0) and (Map [i]<> i) then
      begin
        Inc (MapCount [Map [i]], MapCount [i]);
        Inc (CompXSum [Map [i]], CompXSum [i]);
        Inc (CompYSum [Map [i]], CompYSum [i]);
        MapCount [i]:= 0;

        if MaxCount< MapCount [Map [i]] then
        begin
          MaxCount:= MapCount [Map [i]];
          MaxIndex:= Map [i];
        end;
      end;

    for r:= Top to But- 1 do
    begin
      for c:= Left to Right- 1 do
      begin
        i:= AssignedComponent [r][c];
        System.Write (' (', Copy (IntToStr (i)+ '****', 1, 3), ')');
      end;
      WriteLn;
    end;
    for r:= 0 to ComponentCounter do
      WriteLn (' (', r, '):', Map [r], ' ', MapCount [r]);
    WriteLn (MaxMapIndex);
    WriteLn (CompXSum [MaxMapIndex], ' ',MapCount [MaxMapIndex], ' ',
      CompYSum [MaxMapIndex] );
    Flush (Output);

    Result:= TPoint.Create (CompXSum [MaxMapIndex] div MapCount [MaxMapIndex],
      CompYSum [MaxMapIndex] div MapCount [MaxMapIndex]);

    SetLength (MapCount, 0);
    SetLength (Map, 0);
    SetLength (CompXSum, 0);
    SetLength (CompYSum, 0);
    FreeArrArr (AssignedComponent);

  end;

var
  IntPtr: ^Integer;
  GuidPoints: array [0..6] of TPoint;
  r, c: Integer;
  i: Integer;
}
begin
  raise ENotImplemented.Create ('ExtractImages');
{
  for i:= 1 to 3 do
    GuidPoints [i]:= FindBiggestComponent (FSearchPoints [i, 0].x,
      FSearchPoints [i, 0].y, FSearchPoints [i, 1].x, FSearchPoints [i, 1].y);


  for i:= 1 to 3 do
    GuidPoints [i].Free;
}
end;

procedure TICREntryCollection.Load (FileName: string);
var
  Bitmap: TBitmap;

begin
  Bitmap:= TBitmap.Create;
  Bitmap.LoadFromFile (FileName);

  LoadBitMap (Bitmap);

  Bitmap.Free;

end;

procedure TICREntryCollection.LoadBitmap (Bitmap: TBitmap);

  function IsFromSomeOtherSearch (ActiveComponent: TComponent;
    TopLeftSearchPoint, BotRightSearchPoint: TPoint): Boolean;
  begin
    Result:= False;

    if (ActiveComponent.MinC= 0) or (BotRightSearchPoint.c-
      TopLeftSearchPoint.c<= ActiveComponent.MaxC) or
      (ActiveComponent.MinR= 0) or (BotRightSearchPoint.r-
      TopLeftSearchPoint.r<= ActiveComponent.MaxR) then
    else
      Exit;

    if (ActiveComponent.Width< MinImagePropertiesForSpecialCase.c) or
       (ActiveComponent.Height< MinImagePropertiesForSpecialCase.r) then
    begin
      Result:= True;
      Exit;
      
    end;

  end;
{
var
  Flag: Boolean;
  AllComponents1: TComponentCollection;
  ActiveComponent: TComponent;
  SkewAndReplacementInfo: TSkewReplacementInfo;
  Replacement: TPoint;
  SkewAngle: Extended;
}
begin
{
  NewImage:= TFMLImage.Create;

  NewImage.LoadBitMap (Bitmap);

(*$ifdef DEBUG_MODE*)
  NewImage.GetAsBitmap (False).SaveToFile ('C:\NewImage.txt');
(*$endif*)
  SkewAndReplacementInfo:= FindSkewAndReplacement (NewImage);
  Replacement:= TPoint.Create (
   SkewAndReplacementInfo.cTopLeft- FMainBoxBorder.TopLeft.x,
   SkewAndReplacementInfo.rTopLeft- FMainBoxBorder.TopLeft.y);
  for CompIndex:= 0 to FSearchPoints.Size- 1 do
  begin
    FSearchPoints.SearchPoint [CompIndex].Move (Replacement);
{
  Make the box bigger!!
    FSearchPoints [CompIndex, 0].Rotate (FMainBoxBorder [0], SkewAngle);
    FSearchPoints [CompIndex, 1].Rotate (FMainBoxBorder [1], SkewAngle);
}
{
  end;

  for i:= 0 to Size- 1 do
  begin
    VoteEntry [i].FTopLeft.Move (Replacement);
    VoteEntry [i].FBotRight.Move (Replacement);

  end;

  for CompIndex:= 0 to FSearchPoints.Size- 1 do
  begin
    AllComponents1:= NewImage.FindAllComponentsInBox (
      FSearchPoints.SearchPoint [CompIndex].TopLeft,
                FSearchPoints.SearchPoint [CompIndex].BotRight);

    for i:= 0 to AllComponents1.Count- 1 do
    begin
      ActiveComponent:= AllComponents1.Component [i];

      if (ActiveComponent.Width< MaxImageProperties.x) and
         (MinImageProperties.x< ActiveComponent.Width) and
         (ActiveComponent.Height< MaxImageProperties.y) and
         (MinImageProperties.y< ActiveComponent.Height) and
         (ActiveComponent.Percentage< PointsInImage.y) and
         (PointsInImage.x< ActiveComponent.Percentage) then
      begin

        if IsFromSomeOtherSearch (ActiveComponent,
           FSearchPoints.SearchPoint [CompIndex].TopLeft,
           FSearchPoints.SearchPoint [CompIndex].BotRight) then
          Continue;


        Flag:= False;
        for j:= 0 to Size- 1 do
          if VoteEntry [j].IsComponentMine (ActiveComponent, FSearchPoints.SearchPoint [CompIndex].TopLeft) then
          begin
            Flag:= True;
            Break;

          end;

        if not Flag then
          ActiveComponent.Free;

      end
      else
      begin
      (*$ifdef DEBUG_MODE*)
        TempImage:= TFMLImage.Create (ActiveComponent);
        TempImage.SaveAsText ('C:\NewImage.txt');
        TempImage.Free;

      (*$endif*)
        ActiveComponent.Free;


      end;

{      FSearchPoints [CompIndex, 0].Move (-Replacement);
       FSearchPoints [CompIndex, 1].Move (-Replacement));
}
{
    end;

    AllComponents1.Free (False);

  end;
  NewImage.Free;

  for CompIndex:= 0 to FSearchPoints.Size- 1 do
    FSearchPoints.SearchPoint [CompIndex].Move (-Replacement.x, -Replacement.y);

  for i:= 0 to Size- 1 do
  begin
    VoteEntry [i].FTopLeft.Move (-Replacement.x, -Replacement.y);
    VoteEntry [i].FBotRight.Move (-Replacement.x, -Replacement.y);

  end;

  Replacement.Free;
}
end;

constructor TICREntryCollection.Create (FieldSet: TFieldSet;
    SVMClient: TICRSVMClient);
    
begin
  inherited Create;

  FSVMClient:= SVMClient;

end;

function TICREntryCollection.GeTICREntry (Index: Integer): TICREntry;
begin
  Result:= (Member [Index]) as TICREntry;
  
end;

procedure TICREntry.Print (BaseFileName: String);
var
  i: Integer;
  NewImageCol: TImageCollection;
  NewImage: TFMLImage;

begin
  NewImageCol:= TImageCollection.Create;
  
  for i:= 0 to FComponentCollection.Size- 1 do
  begin
    NewImage:= TFMLImage.Create (FComponentCollection.Component [i]);
    NewImage.Pattern:= UnImportantPattern;

    NewImageCol.AddImage (NewImage);
    
  end;

  NewImageCol.SaveToFile (BaseFileName+ '.FML');

  NewImageCol.Free;

end;

procedure TICREntryCollection.Print;
var
  i: Integer;

begin
  for i:= 0 to Size- 1 do
    VoteEntry [i].Print (IntToStr (i));

end;

function TICREntryCollection.FindReplacement (Image: TFMLImage): TPoint;
{
var
  SkewAndReplacementInfo: TSkewReplacementInfo;
}  
begin
{
  SkewAndReplacementInfo:= FindSkewAndReplacement (Image);

  Result:= TPoint.Create (
    (SkewAndReplacementInfo.cTopLeft-
       FMainBoxBorder.TopLeft.x),
    (SkewAndReplacementInfo.rTopLeft-
       FMainBoxBorder.TopLeft.y));
}
  Result:= nil;
  
end;

function TICREntry.MergeIfNecessary: Boolean;
var
  MergeCandid1,
  MergeCandid2,
  i, j: Integer;
  Temp,
  MinDist: Extended;

begin
  Result:= False;

  if 5< ComponentCollection.Size then
  begin
    MergeCandid1:= -1; MergeCandid2:= -1;
    
    while 5< ComponentCollection.Size do
    begin
      MinDist:= 1e100;

      for i:= 0 to ComponentCollection.Size- 2 do
        for j:= i+ 1 to ComponentCollection.Size- 1 do
        begin
          Temp:= ComponentCollection.Component [i].FindMinDistToComp
                (ComponentCollection.Component [j]);

          if Temp< MinDist then
          begin
            MergeCandid1:= i;
            MergeCandid2:= j;
            MinDist:= Temp;
            Result:= True;

          end;

        end;

       ComponentCollection.Component [MergeCandid1].Merge
                (ComponentCollection.Component [MergeCandid2]);

       ComponentCollection.Delete (MergeCandid2);

    end;

  end;
  
end;

function TICREntry.GetBitmap: TBitmap;
var
  i: Integer;

begin
  raise ENotImplemented.Create ('Get Bitmap');
  
  Result:= TBitmap.Create;
  for i:= 0 to ComponentCollection.Size- 1 do
  begin
//    ComponentCollection.Component [i].
  end;

  FBitmap:= Result;
  
end;

procedure TICREntryCollection.RecognizeAll;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
  begin
    VoteEntry [i].Recognize (SVMClient);
//    Break;
    
  end;


end;

function TICREntry.GetText: String;
begin
  if FText= 'NOT SET YET' then
    raise ETextNotSet.Create;

  Result:= FText;
  
end;

function TICREntry.Recognize (SVMClient: TICRSVMClient): String;
const
  Mask: array [0..5, 0..3] of char=
    (
      (' ', ' ', ' ', ' '),//0
      ('3', '6', '7', '-'),//1
      ('0', '3', '7', '9'),//2
      ('0', '3', '8', '9'),//3
      ('0', '7', '8', '9'),//4
      ('1', '2', '5', '8')//5
    );
{
var
  i, MaxIndex: Integer;
  MaxValue: Extended;
  Component: TComponent;
  DigitImage: TFMLImage;
  DigitResult: TDoubleCollection;
}
begin
  raise ENotImplemented.Create ('TICREntry.Recognize');
  
{
  FText:= '';
  for i:= 0 to FComponentCollection.Count- 1 do
  begin
    Component:= FComponentCollection.Component [i];
    DigitImage:= TFMLImage.Create (Component);
    DigitImage.Pattern:= UnImportantPattern;

    MaxIndex:= DigitResult.MaxIndex;
    MaxValue:= DigitResult.Member [MaxIndex];
    FText:= FText+ Mask [i+ 1, MaxIndex];

    DigitResult.Free;
    DigitImage.Free;

  end;

  Result:= FText;
}
end;

function TICREntry.SegmentIfNecessary: Boolean;
const
  MinPercentage= 30;{??!!}
  MaxPercentage= 200;
  MinWidthThr= 50;
  
var
  i, j: Integer;
  MinPoint,
  MaxPoint,
  PointAvg: Integer;
  TempImage: TFMLImage;
  BlackPixInCols: TBlackPixelCountInColumns;
  TopLeft, BotRight: TPoint;
  NewComponentCollection: TComponentCollection;
  
begin
  BlackPixInCols:= nil;
  Result:= False;

  if ComponentCollection.Size< 5 then
  begin// Here we are going to check if the image can be segmented in two parts by a vertical line

    for i:= 0 to ComponentCollection.Size- 1 do
      if MinWidthThr< ComponentCollection.Component [i].Width then
      begin
        TempImage:= TFMLImage.Create (ComponentCollection.Component [i]);
        BlackPixInCols:= TempImage.BlackPixelCountInColumns;

        PointAvg:= Trunc (ComponentCollection.Component [i].Count/
            (ComponentCollection.Component [i].MaxC-
            ComponentCollection.Component [i].MinC));

        MinPoint:= BlackPixInCols [0];
        MaxPoint:= BlackPixInCols [0];

        for j:= 0 to High (BlackPixInCols) do
          if MaxPoint< BlackPixInCols [j] then
            MaxPoint:= BlackPixInCols [j]
          else if BlackPixInCols [j]< MinPoint then
            MinPoint:= BlackPixInCols [j];

        if PointAvg* MinPercentage< 100* MinPoint then
          MinPoint:= -1;
        if 100* MaxPoint< PointAvg* MaxPercentage then
          MaxPoint:= -1;

        if (MinPoint= -1) and (MaxPoint= -1) then
          Continue;

        for j:= 0 to High (BlackPixInCols) do
          if (BlackPixInCols [j]= MinPoint) or (BlackPixInCols [j]= MaxPoint) then
            TempImage.ClearColumn (j);
        TopLeft:= TPoint.Create (0, 0);
        BotRight:= TPoint.Create (TempImage.Column, TempImage.Row);

        NewComponentCollection:= TempImage.FindAllComponentsInBox (TopLeft, BotRight);
        TopLeft.Free;
        BotRight.Free;

        TopLeft:= ComponentCollection.Component [i].GetMinimum;
        ComponentCollection.Delete (i);

        Result:= 1< NewComponentCollection.Size;

        for j:= 0 to NewComponentCollection.Size- 1 do
        begin
          TopLeft.Scale (NewComponentCollection.Component [j].Count);
          NewComponentCollection.Component [j].CenterOfMass.Move (
            TopLeft);
          AddNewComponent (NewComponentCollection.Component [j]);
          TopLeft.Scale (1/ NewComponentCollection.Component [j].Count);

        end;
        TopLeft.Free;
        NewComponentCollection.Clear;        
        NewComponentCollection.Free;
        TempImage.Free;

      end;

  end;


end;

{ ETextNotSet }

constructor ETextNotSet.Create;
begin
  inherited Create ('Text is not set!!');
  
end;

function TICREntryCollection.FindSkewAndReplacement (Image: TFMLImage;
    Threshold: Extended): TSkewReplacementInfo;
    
const
  AddC: array [1..8] of Integer=     (-1,  0, +1, +1, +1,  0, -1, -1);
  AddR: array [1..8] of Integer=     (-1, -1, -1,  0, +1, +1, +1,  0);
  TopRightScoreInc:
  array [1..8] of Integer=           ( 0, +1, +2, +1,  0, -1, -2, -1);
  TopLeftScoreInc:
  array [1..8] of Integer=           ( 2, +1,  0, -1, -2, -1,  0, +1);

  function FindLineIn (r, c: Integer): TSkewReplacementInfo;
  var
    i: Integer;
    TopLeftSum, TopRightSum: Integer;
    Newr1, Newc1, Newr2, Newc2: Integer;
    ActiveSum,
    State: Integer;
    
  begin
    State:= 4;// to be set to UP when added by 5 in mod 8

    Result.Len:= 0;
    Result.rTopLeft:= r; Result.rTopRight:= r;
    Result.cTopLeft:= c; Result.cTopRight:= c;

{The following codes tries to find the Top Right point of the black line on which the pixel r, c is placed }
{We define an score measure for each point on the line which is formulated as follows:
  Image.Row+ c- r}
{
  ActiveSum is used to avoid recalculating the formulas
}
      
    Newr1:= r; Newc1:= c;
    TopRightSum:= Image.Row+ c- r;

    ActiveSum:= TopRightSum;// Sum of Newr1 and Newc1
    while Image.Body [Newr1, Newc1]= BLACK do
    begin
      State:= (State+ 5) mod 8+ 1;

      Inc (Result.Len);
      for i:= 1 to 8 do
      begin
        Newr2:= Newr1+ AddR [State];
        Newc2:= Newc1+ AddC [State];

        if Image.Body [Newr2, Newc2]= BLACK then
        begin
          Newr1:= Newr2;
          Newc1:= Newc2;
          Inc (ActiveSum, TopRightScoreInc [State]);

          Break;

        end;

        State:= State mod 8+ 1;

      end;

      if TopRightSum< ActiveSum then
      begin
        TopRightSum:= ActiveSum;
        Result.rTopRight:= Newr1;
        Result.cTopRight:= Newc1;

      end;

      if ActiveSum+ 10< TopRightSum{Thr1} then//Should be checked
        Break;

    end;

{The following codes tries to find the Top Left point of the black line on which the pixel r, c is placed }    
    TopLeftSum:= Image.Row+ Image.Column- r- c;
    Newr1:= r; Newc1:= c;
    ActiveSum:= TopLeftSum;
    State:= 0;// to be set to UP when added by 2 in mod 8
    while Image.Body [Newr1, Newc1]= BLACK do
    begin
      State:= (State+ 1) mod 8+ 1;
       
      Inc (Result.Len);
      for i:= 1 to 8 do
      begin
        Newr2:= Newr1+ AddR [State];
        Newc2:= Newc1+ AddC [State];

        if Image.Body [Newr2, Newc2]= BLACK then
        begin
          Newr1:= Newr2;
          Newc1:= Newc2;
          Inc (ActiveSum, TopLeftScoreInc [State]);
          Break;
          
        end;

        State:= (State+ 6) mod 8+ 1;
         
      end;


      if TopLeftSum< ActiveSum then
      begin
        TopLeftSum:= ActiveSum;
        Result.rTopLeft:= Newr1;
        Result.cTopLeft:= Newc1;

      end;

      if ActiveSum+ 10< TopLeftSum{Thr1} then//Should be revised
        Break;

    end;

  end;
{
var
  i, j: Integer;
  r, c: Integer;
}
begin
  raise ENotImplemented.Create ('FindSkewAndReplacement');
{
  This function tries to find a line whose length is about 2/3 of the page width and
  then returns its TopRight and TopLeft points.
}
{
  c:= Image.Column div 2;
  for r:= 0 to Image.Row div 2 do
  begin
    if Image.Body [r, c]= BLACK then
    begin
      Result:= FindLineIn (r, c);
      if Threshold* (FMainBoxBorder.BotRight.x- FMainBoxBorder.BotRight.x)<
          Result.Len then
        Exit;

    end;

  end;
}
end;

{ TICRRecognizer }

constructor TICRRecognizer.Create (SVMSetCollection: TSVMSetCollection;
            PostProcessorInfoCol: TPostProcessorInfoCollection;
         SVMServerHost: String; SVMServerPort: Integer;
         PostProcServerHost: String; PostProcServerPort: Integer;
         FilePath: String);
         
var
  i, j: Integer;
  ActiveSVMClient: TICRSVMClient;
  ActiveSVMInfo: TSVMInfo;

begin
  inherited Create;

  Self.FilePath:= FilePath;
  FSVMClients:= TSVMClients.Create;
  FSVMSetCollection:= SVMSetCollection;
  
  for i:= 0 to SVMSetCollection.Size- 1 do
  begin
    ActiveSVMClient:= TICRSVMClient.Create (SVMServerHost, SVMServerPort);

    for j:= 0 to SVMSetCollection.SVMSet [i].Size- 1 do
    begin
{ TODO : Check if the order of passing parameters to open is correct or not! }
      ActiveSVMInfo:= SVMSetCollection.SVMSet [i].SVMInfo [j];
      ActiveSVMClient.OpenSVM (ActiveSVMInfo.KernelName,
              ActiveSVMInfo.SVMName);
              
    end;

    FSVMClients.AddNewSVMClient (ActiveSVMClient);
    
  end;

//
  FPostProcessor:= TPostProcessorClient.Create (PostProcServerHost,
     PostProcServerPort);

  for i:= 0 to PostProcessorInfoCol.Size- 1 do
    FPostProcessor.LoadDictionary (PostProcessorInfoCol.PostProcessorInfo [i].DictionaryName);

    
end;

function TICRRecognizer.ExtractAFormItemInfo (Image: TFMLImage;
  AFormItemInfo: TFormItemInfo): TFMLImage;
const
  MinCompWidth: Integer= 4;
  MinCompHeight: Integer= 4;
  MinCompArea: Integer= 25;
  MaxCompWidth: Integer= 200;
  MaxCompHeight: Integer= 200;

  MinLetterWidth: Integer= 4;
  MinLetterHeight: Integer= 10;

  procedure DeleteInvalidAndNoisyComponents (AComponentCollection: TComponentCollection);
  var
    i: Integer;
    ActiveComponent,
    ComponentWithMaxArea: TComponent;
    MaxCompIndex: Integer;
    MaxArea: Integer;
    TempPoint: TPoint;
    MaxCompMinPoint, MaxCompMaxPoint: TPoint;

  begin
    if AComponentCollection.Size= 0 then
      Exit;

    ActiveComponent:= AComponentCollection.Component [0];
    MaxArea:= ActiveComponent.GetArea;
    MaxCompIndex:= 0;

    for i:= 1 to AComponentCollection.Size- 1 do
    begin
      ActiveComponent:= AComponentCollection.Component [i];
      if MaxArea< ActiveComponent.GetArea then
      begin
        MaxArea:= ActiveComponent.GetArea;
        MaxCompIndex:= i;

      end;

    end;

    ComponentWithMaxArea:= AComponentCollection.Component [MaxCompIndex];
    MaxCompMinPoint:= ComponentWithMaxArea.GetMinimum;
    MaxCompMaxPoint:= ComponentWithMaxArea.GetMaximum;

    for i:= AComponentCollection.Size- 1 downto 0 do
    begin
      ActiveComponent:= AComponentCollection.Component [i];
      if 3* ActiveComponent.GetArea< MaxArea then
      begin
        TempPoint:= ActiveComponent.CenterOfMass.Copy.Scale (1/ ActiveComponent.Count);

        if not (((MaxCompMinPoint.r<= TempPoint.r) and (TempPoint.r<= MaxCompMaxPoint.r)) or
         ((MaxCompMinPoint.c< TempPoint.c) and (TempPoint.c< MaxCompMaxPoint.c))) then
          AComponentCollection.Delete (i);
            
        TempPoint.Free;

      end;

    end;
      
    MaxCompMinPoint.Free;
    MaxCompMaxPoint.Free;
      
  end;

const
  OutOfBoxThreshold: Extended= 1/ 8;
  SearchArea: Integer= 150;

var
(*$IFDEF DEBUG_MODE *)
  TempImge: TFMLImage;
(*$ENDIF*)
  i: Integer;
  TopLeft, BotRight: TPoint;
  Components: TComponentCollection;
  ActiveComponent: TComponent;
  OutOfBoxComp: TComponent;
  Pos, OutOfBoxComCOM: TPoint;
  FormItemInfoWidth,
  FormItemInfoHeight,
  r_CenterOfFormItemInfo,
  C_CenterOfFormItemInfo: Integer;
  TempImage: TFMLImage;
  
begin
  TopLeft:= AFormItemInfo.RealTopLeft.Copy;
  BotRight:= AFormItemInfo.RealBottomRight.Copy;
  FormItemInfoWidth:= BotRight.c- TopLeft.c;
  FormItemInfoHeight:= BotRight.r- TopLeft.r;
{      TopLeft.Move (-SearchArea, -SearchArea);
  BotRight.Move (SearchArea, SearchArea);
}
  r_CenterOfFormItemInfo:= FormItemInfoWidth div 2+ TopLeft.c;
  c_CenterOfFormItemInfo:= FormItemInfoHeight div 2+ TopLeft.r;
  
  Components:= Image.FindAllComponentsInBox (TopLeft, BotRight);

  for i:= Components.Size- 1 downto 0 do
  begin
    ActiveComponent:= Components.Component [i];
    if (FormItemInfoWidth div 4< Abs (ActiveComponent.CenterOfMass.r/ ActiveComponent.Count- r_CenterOfFormItemInfo))
    or (FormItemInfoHeight div 4< Abs (ActiveComponent.CenterOfMass.c/ ActiveComponent.Count- C_CenterOfFormItemInfo)) then
      if (ActiveComponent.Width<= MaxCompWidth) and
         (ActiveComponent.Height<= MaxCompHeight) and
        (MinCompWidth< ActiveComponent.Width) and
         (MinCompHeight< ActiveComponent.Height) and
         (MinCompArea< ActiveComponent.Count) then
      else
      begin
        Components.Delete (i);
        Continue;

      end;

    if (FormItemInfoWidth- 2< ActiveComponent.MaxC) or (ActiveComponent.MinC< 2)
    or (FormItemInfoHeight- 2< ActiveComponent.MaxR) or (ActiveComponent.MinR< 2) then
      if ActiveComponent.Count<> 0 then
      begin
        Pos:= ActiveComponent.GetPixel (0).Location.Copy;
        Pos.Move (TopLeft);

        OutOfBoxComp:= Image.GetComponentByOnePoint (Pos);
        Pos.Free;
        
        OutOfBoxComCOM:= OutOfBoxComp.CenterOfMass.Copy.Scale (1/ OutOfBoxComp.Count);

        if (OutOfBoxComCOM.r< TopLeft.r) or (OutOfBoxComCOM.c< TopLeft.c) or (BotRight.r< OutOfBoxComCOM.r) or
              (BotRight.c< OutOfBoxComCOM.c) then
        begin
          OutOfBoxComp.Free;
          Components.Delete (i);
          OutOfBoxComCOM.Free;
          Continue;

        end;

        if (OutOfBoxComp.Width<= MaxCompWidth) and
           (OutOfBoxComp.Height<= MaxCompHeight) and
          (MinCompWidth< OutOfBoxComp.Width) and
           (MinCompHeight< OutOfBoxComp.Height) then
        begin
//          Pos.Move (0, 0)
        end
        else
        begin
          OutOfBoxComp.Free;
          Components.Delete (i);
          OutOfBoxComCOM.Free;
          Continue;

        end;

        TempImage:= TFMLImage.Create (OutOfBoxComp);
        TempImage.SaveAsText ('C:\OutOfBox.txt');
        TempImage.Free;

        OutOfBoxComp.Move (-TopLeft.r, -TopLeft.c);
        OutOfBoxComCOM.Free;

        Components.Add (OutOfBoxComp);
        Components.Delete (i);

      end;

  end;

  DeleteInvalidAndNoisyComponents (Components);
  Result:= TFMLImage.Create (Components);
  Components.Free;
  TopLeft.Free;
  BotRight.Free;

  if Result.IsBlank<> mbTrue then
    if (Result.Row< MinLetterWidth) or
       (Result.Column< MinLetterWidth) then
    begin
      Result.Free;
      Result:= TFMLImage.Create;

    end;

end;

function TICRRecognizer.ExtractAndRecognizeFieldSet (Image: TFMLImage;
  AFieldSet: TFieldSet): TImageCollection;
const
  NewSizes: array [itNone..itCheck] of Integer= (0, 50, 50, 80, 0);
  SmoothDeg: array [itNone..itCheck] of Integer= (0, 2, 2, 2, 0);

var
  i, k, SVMIndex: Integer;
  Ptr: PObject;
  ActiveFormItemInfo: TFormItemInfo;
  ExtractedImage: TFMLImage;
  PostWord: TPostProcessorWord;
  IntArray: TIntegerArray;

begin
  Result:= TImageCollection.Create;

  SVMIndex:= FSVMSetCollection.SVMSetIndexByName [AFieldSet.FieldSetInfo.SVMCollectionID];

  Ptr:= AFieldSet.GetPointerToFirst;
  PostWord:= AFieldSet.PostProcessorWord;

  SetLength (IntArray, FSVMClients.SVMClient [SVMIndex].LoadedSVMCount);

  for i:= 1 to AFieldSet.Size do
  begin
    ActiveFormItemInfo:= TFormItemInfo (Ptr^);
//  ActiveFormItemInfo:= AFieldSet.ItemInfo [i];
    ExtractedImage:= ExtractAFormItemInfo (Image,
           ActiveFormItemInfo);

    if ExtractedImage.IsBlank<> mbTrue then
    begin
      RecognizeAFormItem (ExtractedImage, ActiveFormItemInfo,
          FSVMClients.SVMClient [SVMIndex], FSVMSetCollection.SVMSet [SVMIndex],
            NewSizes [ActiveFormItemInfo.ItemType],
            SmoothDeg [ActiveFormItemInfo.ItemType],
                IntArray);

      for k:= 0 to FSVMSetCollection.SVMSet [SVMIndex].Size- 1 do
        IntArray [k]:= FSVMSetCollection.SVMSet [SVMIndex].SVMInfo [IntArray [k]].Index;

      AFieldSet.AddCharacterToPostWord (IntArray);
      Result.AddImage (ExtractedImage);

    end
    else
      ExtractedImage.Free;

    Inc (Ptr);

  end;


end;

function TICRRecognizer.ExtractContour (Image: TFMLImage; r, c: Integer;
          StartingDir: TDirection): TComponent;
const
  AdjancedPixelR: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
  AdjancedPixelC: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);

var
  RGBColor: TRGB;
  StartDir: TDirection;
  i: Integer;

begin
  RGBColor.r:= 0; RGBColor.g:= 0; RGBColor.b:= 0;

  StartDir:= StartingDir;
  Result:= TComponent.Create;

  while not Result.HashedData.IsExist (r, c) do
  begin
    Result.Add (r, c, RGBColor);

    StartDir:= TDirection ( (Ord (StartDir)+ 5) mod 8);

    for i:= 0 to 7 do
    begin
      if Image.Body [r+ AdjancedPixelR [StartDir], c+ AdjancedPixelC [StartDir]]= BLACK then
      begin
        Inc (r, AdjancedPixelR [StartDir]);
        Inc (c, AdjancedPixelC [StartDir]);

        Break;

      end;

      StartDir:= TDirection ( (Ord (StartDir)+ 1) mod 8);

    end;

  end;

end;

procedure TICRRecognizer.FindRealPositionOfLocationChecks (Image: TFMLImage;
  LocationChecks: TLocationCheckCollection);
const
  Threshold: Extended= 2/ 3;
  SearchArea: Integer= 200;
  
var
  LocationChecksPos: array [0..4] of TPoint;
  WidthSum, HeightSum,
  RepC, RepR,
  ImageCol, ImageRow, 
  r, c, SignR, SignC,
  rInc, cInc: Integer;
  ActiveLoactionCheck: TLocationCheck;
  Component: TComponent;
  IgnoredComponents: TComponentCollection;
  Found: Boolean;
  APoint: TPoint;
  Slope: Extended;

begin
  ActiveLoactionCheck:= LocationChecks.LocationCheck [0];
  WidthSum:= 0; HeightSum:= 0;
  for r:= 0 to 4 do
    LocationChecksPos [r]:= nil;

  Found:= False;
  IgnoredComponents:= TComponentCollection.Create;
  ImageRow:= Image.Row; ImageCol:= Image.Column;
  
  SignR:= 1;
  r:= ActiveLoactionCheck.AnnouncedTop;
  for rInc:= 1 to Max (0, ActiveLoactionCheck.AnnouncedTop- ActiveLoactionCheck.AnnouncedHeight- SearchArea)+
     Min (ImageRow- 1, ActiveLoactionCheck.AnnouncedTop+ ActiveLoactionCheck.AnnouncedHeight+ SearchArea) do
  begin
    Inc (r, SignR* rInc);
    SignR:= -SignR;
    if (r< 0) or (ImageRow<= r) then
      Continue;

    c:= ActiveLoactionCheck.AnnouncedLeft; SignC:= 1;
    
    for cInc:= 1 to Max (0, ActiveLoactionCheck.AnnouncedLeft- ActiveLoactionCheck.AnnouncedWidth- SearchArea)+
    Min (ImageCol- 1, ActiveLoactionCheck.AnnouncedLeft+ ActiveLoactionCheck.AnnouncedWidth+ SearchArea) do
    begin
      Inc (c, SignC* cInc);
      SignC:= -SignC;
      if (c< 0) or (ImageCol<= c) then
        Continue;

       if not IgnoredComponents.IsExists (r, c) then
         if Image.Body [r, c]= BLACK then
         begin
           APoint:= TPoint.Create (r, c);
           Component:= Image.GetComponentByOnePoint (APoint);
           APoint.Free;
         
           if (Threshold* ActiveLoactionCheck.AnnouncedWidth< Component.Width) and
             (Threshold* Component.Width< ActiveLoactionCheck.AnnouncedWidth) and
             (Threshold* (ActiveLoactionCheck.AnnouncedHeight)< Component.Height) and
             (Threshold* Component.Height< ActiveLoactionCheck.AnnouncedHeight) and
           (Component.Height* Component.Width* Threshold< Component.Count) then
           begin
             LocationChecksPos [1]:= Component.CenterOfMass.Copy;
             LocationChecksPos [1].Scale (1/ Component.Count);
             Inc (WidthSum, Component.Width);
             Inc (HeightSum, Component.Height);

             Component.Free;
             Found:= True;
             Break;

           end
           else
             IgnoredComponents.Add (Component);

         end;

    end;// end for cInc

    if Found then
      Break;

  end;

  if not Found then
  begin
    IgnoredComponents.Free;
    raise EInvalidLocationCheck.Create ('LocationCheck  No 1 was not Found!');

  end;
  
    
  RepC:= LocationChecksPos [1].c- ActiveLoactionCheck.AnnouncedLeft;
  RepR:= LocationChecksPos [1].r- ActiveLoactionCheck.AnnouncedTop;

  ActiveLoactionCheck.AnnouncedTop:= ActiveLoactionCheck.AnnouncedTop+ RepR;
  ActiveLoactionCheck.AnnouncedBottom:= ActiveLoactionCheck.AnnouncedBottom+ RepR;
  ActiveLoactionCheck.AnnouncedLeft:= ActiveLoactionCheck.AnnouncedLeft+ RepC;
  ActiveLoactionCheck.AnnouncedRight:= ActiveLoactionCheck.AnnouncedRight+ RepC;

  Found:= False;
  for r:= Max (0, ActiveLoactionCheck.AnnouncedTop- ActiveLoactionCheck.AnnouncedHeight- SearchArea) to
     Min (Image.Row- 1, ActiveLoactionCheck.AnnouncedTop+ ActiveLoactionCheck.AnnouncedHeight+ SearchArea) do
  begin
    for c:= Min (Image.Column- 1, ActiveLoactionCheck.AnnouncedRight+ ActiveLoactionCheck.AnnouncedWidth+ SearchArea) downto
      Max (0, ActiveLoactionCheck.AnnouncedRight- ActiveLoactionCheck.AnnouncedWidth- SearchArea) do
      if not IgnoredComponents.IsExists (r, c) and (Image.Body [r, c]= BLACK) then
        begin
          APoint:= TPoint.Create (r, c);
          Component:= Image.GetComponentByOnePoint (APoint);
          APoint.Free;

          if (Threshold* ActiveLoactionCheck.AnnouncedWidth< Component.Width) and
           (Threshold* Component.Width< ActiveLoactionCheck.AnnouncedWidth) and
           (Threshold* (ActiveLoactionCheck.AnnouncedHeight)< Component.Height) and
           (Threshold* Component.Height< ActiveLoactionCheck.AnnouncedHeight) and
           (Component.Height* Component.Width* Threshold< Component.Count) then
          begin
            LocationChecksPos [2]:= Component.CenterOfMass.Copy;
            LocationChecksPos [2].Scale (1/ Component.Count);

            Inc (WidthSum, Component.Width);
            Inc (HeightSum, Component.Height);
            Component.Free;
            Found:= True;
            Break;

          end
          else
            IgnoredComponents.Add (Component);

        end;

     if Found then
       Break;

  end;

  if not Found then
  begin
    IgnoredComponents.Free;
    raise EInvalidLocationCheck.Create ('LocationCheck  No 2 was not Found!');

  end;

  Slope:= (LocationChecksPos [2].r- LocationChecksPos [1].r)/
     (LocationChecksPos [2].c- LocationChecksPos [1].c); //(TopRight.r- TopLeft.r)/ (TopRight.c- TopLeft.c)  

  Found:= False;
  for r:= Min (Image.Row- 1, ActiveLoactionCheck.AnnouncedBottom+ ActiveLoactionCheck.AnnouncedHeight+ SearchArea+ RepR- Round (Slope* ActiveLoactionCheck.AnnouncedHeight)) downto
     Max (0, ActiveLoactionCheck.AnnouncedBottom- ActiveLoactionCheck.AnnouncedHeight- SearchArea+ RepR- Round (Slope* ActiveLoactionCheck.AnnouncedHeight)) do
  begin
    for c:= Max (0, ActiveLoactionCheck.AnnouncedLeft- ActiveLoactionCheck.AnnouncedWidth- SearchArea+ RepC) to
    Min (Image.Column- 1, ActiveLoactionCheck.AnnouncedLeft+ ActiveLoactionCheck.AnnouncedWidth+ SearchArea+ RepC) do
       if not IgnoredComponents.IsExists (r, c) and (Image.Body [r, c]= BLACK) then
       begin
         APoint:= TPoint.Create (r, c);
         Component:= Image.GetComponentByOnePoint (APoint);
         APoint.Free;

         if (Threshold* ActiveLoactionCheck.AnnouncedWidth< Component.Width) and
           (Threshold* Component.Width< ActiveLoactionCheck.AnnouncedWidth) and
           (Threshold* (ActiveLoactionCheck.AnnouncedHeight)< Component.Height) and
           (Threshold* Component.Height< ActiveLoactionCheck.AnnouncedHeight) and
           (Component.Height* Component.Width* Threshold< Component.Count) then
         begin
           LocationChecksPos [3]:= Component.CenterOfMass.Copy;
           LocationChecksPos [3].Scale (1/ Component.Count);
           Inc (WidthSum, Component.Width);
           Inc (HeightSum, Component.Height);

           Found:= True;
           Component.Free;
           Break;

         end
         else
           Component.Free;

       end;
       
    if Found then
     Break;


  end;
  
  Found:= False;
  
  for r:= Max (0, ActiveLoactionCheck.AnnouncedBottom- ActiveLoactionCheck.AnnouncedHeight- SearchArea+ RepR- Round (Slope* ActiveLoactionCheck.AnnouncedHeight)) to
     Min (Image.Row- 1, ActiveLoactionCheck.AnnouncedBottom+ ActiveLoactionCheck.AnnouncedHeight+ SearchArea+ RepR- Round (Slope* ActiveLoactionCheck.AnnouncedHeight)) do
  begin
    for c:= Min (Image.Column- 1, ActiveLoactionCheck.AnnouncedRight+ ActiveLoactionCheck.AnnouncedWidth+ SearchArea+ RepC) downto
       Max (0, ActiveLoactionCheck.AnnouncedRight- ActiveLoactionCheck.AnnouncedWidth- SearchArea+ RepC) do
      if not IgnoredComponents.IsExists (r, c) and (Image.Body [r, c]= BLACK) then
       begin
         APoint:= TPoint.Create (r, c);
         Component:= Image.GetComponentByOnePoint (APoint);
         APoint.Free;

         if (Threshold* ActiveLoactionCheck.AnnouncedWidth< Component.Width) and
           (Threshold* Component.Width< ActiveLoactionCheck.AnnouncedWidth) and
           (Threshold* (ActiveLoactionCheck.AnnouncedHeight)< Component.Height) and
           (Threshold* Component.Height< ActiveLoactionCheck.AnnouncedHeight) and
           (Component.Height* Component.Width* Threshold<
             Component.Count) then
         begin
           LocationChecksPos [4]:= Component.CenterOfMass.Copy;
           LocationChecksPos [4].Scale (1/ Component.Count);

           Inc (WidthSum, Component.Width);
           Inc (HeightSum, Component.Height);
           Found:= True;
           Component.Free;
           Break;

         end
         else
           Component.Free;
       end;

    if Found then
     Break;
     
  end;


  ActiveLoactionCheck.AnnouncedTop:= ActiveLoactionCheck.AnnouncedTop- RepR;
  ActiveLoactionCheck.AnnouncedBottom:= ActiveLoactionCheck.AnnouncedBottom- RepR;
  ActiveLoactionCheck.AnnouncedLeft:= ActiveLoactionCheck.AnnouncedLeft- RepC;
  ActiveLoactionCheck.AnnouncedRight:= ActiveLoactionCheck.AnnouncedRight- RepC;

  for r:= 1 to 4 do
    if LocationChecksPos [r]= nil then
    begin
      IgnoredComponents.Free;
      raise EInvalidLocationCheck.Create ('LocationCheck  No '+ IntToStr (r)+ ' was not Found!');
      
    end;

  ActiveLoactionCheck.RealWidth:= WidthSum div 4;
  ActiveLoactionCheck.RealHeight:= HeightSum div 4;
  ActiveLoactionCheck.RealTopLeft:= LocationChecksPos [1];
  ActiveLoactionCheck.RealTopRight:= LocationChecksPos [2];
  ActiveLoactionCheck.RealBotLeft:= LocationChecksPos [3];
  ActiveLoactionCheck.RealBotRight:= LocationChecksPos [4];

  IgnoredComponents.Free;

end;

function TICRRecognizer.FindReplacement (Image: TFMLImage;
  LocationChecks: TLocationCheckCollection): TPoint;
var
  ActiveLocatoinCheck: TLocationCheck;

begin

  ActiveLocatoinCheck:= LocationChecks.LocationCheck [0];
  
    Result:= TPoint.Create (
      ActiveLocatoinCheck.RealTopLeft.r- ActiveLocatoinCheck.AnnouncedTop
//      ActiveLocatoinCheck.RealBottom- ActiveLocatoinCheck.AnnouncedBottom) div 2,
      ,
      ActiveLocatoinCheck.RealTopLeft.c- ActiveLocatoinCheck.AnnouncedLeft
//      ActiveLocatoinCheck.RealRight- ActiveLocatoinCheck.AnnouncedRight) div 2);
      );
      
end;

destructor TICRRecognizer.Destroy;
begin
  FSVMClients.Free;
  FSVMSetCollection:= nil;
  
  inherited;

end;

procedure TICRRecognizer.Recognize (ABitmap: TBitmap;  AFormInfo: TFormInfo; Prefix: String);

  function FindAngle (DeltaX, DeltaY: Integer): Extended;// find the angle in Radian
  begin
    if DeltaX= 0 then
      Result:= Pi/ 2
    else
      Result:= ArcTan (DeltaY/ DeltaX);

    if DeltaX< 0 then
      Result:= Result+ Pi;

  end;
  
var
  Image: TFMLImage;
  i, j: Integer;
  Ptr: PObject;
  FieldSetImageCollection: TImageCollection;
  ActiveFieldSet: TFieldSet;
  SkewAngle: Extended;
  Center,
  Replacement, StretchCoef: TPoint;
  ActiveLocationCheck: TLocationCheck;
  TempStrList: TStringList;
  TempString: String;
  CharPtr: PChar;
  
begin

  Image:= TFMLImage.Create;
  Image.LoadBitMap (ABitmap);
  FindRealPositionOfLocationChecks (Image, AFormInfo.LocationCheckCollection);

  ActiveLocationCheck:= AFormInfo.LocationCheckCollection.LocationCheck [0];
  Replacement:= TPoint.Create (ActiveLocationCheck.RealTopLeft.r-
      ActiveLocationCheck.AnnouncedTop,
      ActiveLocationCheck.RealTopLeft.c
      -ActiveLocationCheck.AnnouncedLeft);

  StretchCoef:= TPoint.Create (
    ((ActiveLocationCheck.RealTopLeft.r+ ActiveLocationCheck.RealTopRight.r-
      ActiveLocationCheck.RealBotLeft.r- ActiveLocationCheck.RealBotRight.r)* 100) div
      (2* (ActiveLocationCheck.AnnouncedTop- ActiveLocationCheck.AnnouncedBottom)),
                              ((ActiveLocationCheck.RealTopLeft.c+ ActiveLocationCheck.RealBotLeft.c-
                               ActiveLocationCheck.RealTopRight.c- ActiveLocationCheck.RealBotRight.c)* 100) div
            (2* (ActiveLocationCheck.AnnouncedLeft- ActiveLocationCheck.AnnouncedRight)));// 100 times greater!!

  SkewAngle:= FindAngle (ActiveLocationCheck.RealTopRight.c- ActiveLocationCheck.RealTopLeft.c,
           ActiveLocationCheck.RealTopRight.r- ActiveLocationCheck.RealTopLeft.r);

(*$IFDEF ICRUnitDebugMode*)
  AssignFile (Output, 'C:\Temp.txt');
  Rewrite (Output);
  WriteLn ('Top= ', ActiveLocationCheck.AnnouncedTop);
  WriteLn ('Right= ',   ActiveLocationCheck.AnnouncedRight);
  WriteLn ('Bot= ',   ActiveLocationCheck.AnnouncedBottom);
  WriteLn ('Left= ',   ActiveLocationCheck.AnnouncedLeft);

  WriteLn;

  WriteLn ('R.TL= ', ActiveLocationCheck.RealTopLeft.ToString);
  WriteLn ('R.TR= ', ActiveLocationCheck.RealTopRight.ToString);
  WriteLn ('R.BL= ', ActiveLocationCheck.RealBotLeft.ToString);
  WriteLn ('R.BR= ', ActiveLocationCheck.RealBotRight.ToString);

  WriteLn;
  WriteLn ('Replacement: ', Replacement.ToString);
  WriteLn ('StrachCoef: ', StretchCoef.ToString);

  SkewAngle:= FindAngle (ActiveLocationCheck.RealTopRight.c- ActiveLocationCheck.RealTopLeft.c,
           ActiveLocationCheck.RealTopRight.r- ActiveLocationCheck.RealTopLeft.r);
  WriteLn (Output, 'SkewAngle= ', SkewAngle:0:3);
(*$ENDIF*)

  Ptr:= AFormInfo.FormItemCollection.GetPointerToFirst;

  Center:= ActiveLocationCheck.RealTopLeft;

  for i:= 1 to AFormInfo.FormItemCollection.Size do
  begin
    ActiveFieldSet:= TFieldSet (Ptr^);

    for j:= 0 to ActiveFieldSet.Size- 1 do
    begin
(*$IFDEF ICRUnitDebugMode*)
      WriteLn (Output, i, ' ', j, ' Announced TL= ', ActiveFieldSet.ItemInfo [j].AnnouncedTopLeft.ToString);
      WriteLn (Output, i, ' ', j, ' Announced BR= ', ActiveFieldSet.ItemInfo [j].AnnouncedBottomRight.ToString);
(*$ENDIF*)

      ActiveFieldSet.ItemInfo [j].RealTopLeft.Scale (StretchCoef.r/ 100, StretchCoef.c/ 100).Rotate (-SkewAngle);
      ActiveFieldSet.ItemInfo [j].RealTopLeft.Move (Center);

      ActiveFieldSet.ItemInfo [j].RealBottomRight.Scale (StretchCoef.r/ 100, StretchCoef.c/ 100).Rotate (-SkewAngle);
      ActiveFieldSet.ItemInfo [j].RealBottomRight.Move (Center);

(*$IFDEF ICRUnitDebugMode*)
      WriteLn (Output, i, ' ', j, ' ReaL TL= ', ActiveFieldSet.ItemInfo [j].RealTopLeft.ToString);
      WriteLn (Output, i, ' ', j, ' ReaL BR= ', ActiveFieldSet.ItemInfo [j].RealBottomRight.ToString);
(*$ENDIF*)

    end;

    Inc (Ptr);

  end;

(*$IFDEF ICRUnitDebugMode*)
    CloseFile (Output);

(*$ENDIF*)

  StretchCoef.Free;
  Replacement.Free;

  Ptr:= AFormInfo.FormItemCollection.GetPointerToFirst;
  for i:= 0 to AFormInfo.FormItemCollection.Size- 1 do
  begin
    ActiveFieldSet:= TFieldSet (Ptr^);

    FieldSetImageCollection:= ExtractAndRecognizeFieldSet (Image, ActiveFieldSet);

(*$IFDEF ICRUnitDebugMode*)
  if not DirectoryExists (FilePath+ 'Extracted\') then
    CreateDir (FilePath+ 'Extracted\');

    FieldSetImageCollection.SaveFilesAsBitmap (FilePath+ 'Extracted\'+ Prefix+ IntToStr (i)+ '-');
(*$ENDIF*)
    if UpperCase (ActiveFieldSet.PostProcessor)<> UpperCase ('NONE') then
    begin
      TempStrList:= FPostProcessor.AskQuery (ActiveFieldSet.PostProcessor, ActiveFieldSet.PostProcessorWord);
      if 0< TempStrList.Count then
      begin
        TempString:= TempStrList.Strings [0];
        CharPtr:= PChar (TempString);
        ActiveFieldSet.CorrectedValue:= ReadWideStringFromACharArray (CharPtr, Length (TempString));
        
      end;

      TempStrList.Free;
      
    end;

    FieldSetImageCollection.Free;

    Inc (Ptr);

  end;

  Image.Free;
  
end;

function TICRRecognizer.RecognizeAFormItem (ItemImage: TFMLImage;
  FormItemInfo: TFormItemInfo; SVMClient: TSVMClient;
  SVMSet: TSVMSetInfo; NewSize, SmoothLevel: Integer;
  var SVMRating: TIntegerArray): Boolean;
  
var
  FeatureVector: TFeatureVectorBasedOnGradiant;
  WinnerSVMData: TSVMBatchModeResult;
begin
  Result:= True;

  if ItemImage.ImageThickness<= 5 then
    ItemImage.ThickTheImage;

// The New ImageItem is a refrence to Handwritten Obj

  FeatureVector:= ItemImage.ExtractFeatures (NewSize, SmoothLevel);
  WinnerSVMData:= SVMClient.AskBatchQuery (FeatureVector, SVMRating);

{ TODO -cRecognition -oAmir : If the winner svm's score was negative}
//  if WinnerSVMData.MaxValue< 0 then
//

  FormItemInfo.Value:= SVMSet.SVMInfo [WinnerSVMData.MaxIndex].Caption;

end;

{ EInvalidLocationCheck }

constructor EInvalidLocationCheck.Create (Str: String);
begin
  inherited Create (Str);

end;

constructor EInvalidLocationCheck.Create(P1, P2, P3, P4: Integer);
begin
  inherited Create ('No check Location founds statisfying the given Location '+
     '('+ IntToStr (P1)+ ','+ IntToStr (P2)+ '):('+
       IntToStr (P3)+ ','+ IntToStr (P4)+ ')');

end;

end.
