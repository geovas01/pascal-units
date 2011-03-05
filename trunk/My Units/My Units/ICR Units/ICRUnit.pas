unit ICRUnit;

interface
uses
  FMLImage, HashUnit, CollectionUnit, ComponentsUnit,
  SVMClientUnit, SysUtils, FeatureUnit, GeometryUnit
  , Graphics, GeneralTypesUnit;

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
    FSVMClients: TSVMClients;

    function ExtractContour (Image: TFMLImage; r, c: Integer;
      StartingDir: TDirection= dS): TComponent;

    function FindReplacement (Image: TFMLImage;
            LocationChecks: TLocationCheckCollection): TPoint;

    procedure FindRealPositionOfLocationChecks (Image: TFMLImage;
            LocationChecks: TLocationCheckCollection;
             var Skew: TPoint);
             
  public
    constructor Create (SVMSetCollection: TSVMSetCollection;
         ServerHost: String; ServerPort: Integer);
    procedure Free;

    procedure Recognize (ABitmap: TBitmap; AFormInfo: TFormInfo);

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
    procedure Free;

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
    procedure Free;

    procedure LoadBitmap (Bitmap: TBitmap); overload;
    procedure Load (FileName: string);

    procedure Print;
    procedure RecognizeAll; 

  end;

implementation
uses
  ExceptionUnit, Math;
  
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
  ComponentsPtr:= FComponentCollection.ComponentPointer;

  ReplacementIndex:= FComponentCollection.Count;
  for i:= 0 to FComponentCollection.Count- 1 do
  begin

    if Component.CenterOfMass.c/ Component.Count<
      ComponentsPtr^.CenterOfMass.c/ ComponentsPtr^.Count then
    begin
      ReplacementIndex:= i;
      Break;

    end;

    Inc (ComponentsPtr);

  end;

  FComponentCollection.AddComponent (nil);
  NextComponentsPtr:= FComponentCollection.ComponentPointer;
  ComponentsPtr:= NextComponentsPtr;
  if 1< FComponentCollection.Count then
  begin
    Inc (NextComponentsPtr, FComponentCollection.Count- 1);
    Inc (ComponentsPtr, FComponentCollection.Count- 2);

    for i:= FComponentCollection.Count- 2 downto ReplacementIndex do
    begin
      NextComponentsPtr^:= ComponentsPtr^;
      Dec (NextComponentsPtr);
      Dec (ComponentsPtr);

    end;
    
  end;

  NextComponentsPtr^:= Component;
  
end;

procedure TICREntry.Free;

begin
  FTopLeft.Free;
  FBotRight.Free;
  FComponentCollection.Free (True);
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
  raise ENotImplemented.Create ('');
{
  for i:= 1 to 3 do
    GuidPoints [i]:= FindBiggestComponent (FSearchPoints [i, 0].x,
      FSearchPoints [i, 0].y, FSearchPoints [i, 1].x, FSearchPoints [i, 1].y);


  for i:= 1 to 3 do
    GuidPoints [i].Free;
}
end;

procedure TICREntryCollection.Free;
var
  i: Integer;
  
begin

  for i:= 0 to Size- 1 do
    VoteEntry [i].Free;
    
  inherited Free;
  
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
  
  for i:= 0 to FComponentCollection.Count- 1 do
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

  if 5< ComponentCollection.Count then
  begin
    MergeCandid1:= -1; MergeCandid2:= -1;
    
    while 5< ComponentCollection.Count do
    begin
      MinDist:= 1e100;

      for i:= 0 to ComponentCollection.Count- 2 do
        for j:= i+ 1 to ComponentCollection.Count- 1 do
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

       ComponentCollection.Component [MergeCandid2].Free;
       ComponentCollection.DeleteComponent (MergeCandid2);

    end;

  end;
  
end;

function TICREntry.GetBitmap: TBitmap;
var
  i: Integer;

begin
  raise ENotImplemented.Create ('Get Bitmap');
  
  Result:= TBitmap.Create;
  for i:= 0 to ComponentCollection.Count- 1 do
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

var
  counter: integer;

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

var
  i, j, MaxIndex: Integer;
  MaxValue: Extended;
  Component: TComponent;
  DigitImage: TFMLImage;
  DigitResult: TDoubleCollection;

begin
  FText:= '';
  for i:= 0 to FComponentCollection.Count- 1 do
  begin
    Component:= FComponentCollection.Component [i];
    DigitImage:= TFMLImage.Create (Component);
    DigitImage.Pattern:= UnImportantPattern;
    
//    DigitImage.SaveInFMLFile ('C:\QImage.FML');
//    DigitImage.SaveAsText ('C:\QImage.txt');
//    DigitImage.ExtractFeatures (50, 10);
//    DigitResult:= SVMClient.QueryOnIthDigit (i+ 1, DigitImage.ExtractFeatures (50, 10));
//    Inc (counter);
//    DigitImage.ExtractFeatures (50,10).SaveToFile('C:\1.txt');
    MaxIndex:= DigitResult.MaxIndex;
    MaxValue:= DigitResult.Member [MaxIndex];
    FText:= FText+ Mask [i+ 1, MaxIndex];

    DigitResult.Free;
    DigitImage.Free;

  end;

  Result:= FText;
  
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
  Change: Boolean;
  
begin
  Result:= False;
  
  if ComponentCollection.Count< 5 then
  begin// Here we are going to check if the image can be segmented in two parts by a vertical line

    for i:= 0 to ComponentCollection.Count- 1 do
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

        NewComponentCollection:= TempImage.FindAllComponentsInBox (TopLeft, BotRight,
False);
        TopLeft.Free;
        BotRight.Free;

        TopLeft:= ComponentCollection.Component [i].GetMinimum;
        ComponentCollection.DeleteComponent (i, True);

        Result:= 1< NewComponentCollection.Count;

        for j:= 0 to NewComponentCollection.Count- 1 do
        begin
          TopLeft.Scale (NewComponentCollection.Component [j].Count);
          NewComponentCollection.Component [j].CenterOfMass.Move (
            TopLeft);
          AddNewComponent (NewComponentCollection.Component [j]);
          TopLeft.Scale (1/ NewComponentCollection.Component [j].Count);

        end;
        TopLeft.Free;
        NewComponentCollection.Free (False);
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

var
  i, j: Integer;
  r, c: Integer;
  
begin
  c:= Image.Column div 2;
{
  This function tries to find a line whose length is about 2/3 of the page width and
  then returns its TopRight and TopLeft points. 
}
{
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
         ServerHost: String; ServerPort: Integer);
var
  i, j: Integer;
  ActiveSVMClient: TICRSVMClient;
  ActiveSVMInfo: TSVMInfo;

begin
  inherited Create;

  FSVMClients:= TSVMClients.Create;
  Exit;

  for i:= 0 to SVMSetCollection.Size- 1 do
  begin
    ActiveSVMClient:= TICRSVMClient.Create (ServerHost, ServerPort);

    for j:= 0 to SVMSetCollection.SVMSet [i].Size- 1 do
    begin
      ActiveSVMInfo:= SVMSetCollection.SVMSet [i].SVMInfo [j];
      
{ TODO : Check if the order of passing parameters to open is correct or not! }      ActiveSVMInfo:= SVMSetCollection.SVMSet [i].SVMInfo [j];
      ActiveSVMClient.OpenSVM (ActiveSVMInfo.KernelName,
              ActiveSVMInfo.SVMName);
              
    end;

    FSVMClients.AddNewSVMClient (ActiveSVMClient);
    
  end;

end;

function TICRRecognizer.ExtractContour (Image: TFMLImage; r, c: Integer;
          StartingDir: TDirection): TComponent;
const
  AdjancedPixelR: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
  AdjancedPixelC: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);

var
  RGBColor: TRGB;
  x, y: Integer;
  StartDir: TDirection;
  i, j: Integer;

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
  LocationChecks: TLocationCheckCollection; var Skew: TPoint);
const
  Threshold: Extended= 2/ 3;
  
var
  LocationChecksPos: array [0..4] of TPoint;
  WidthSum, HeightSum, 
  r, c: Integer;
  Newr: Integer;
  
  ActiveLoactionCheck: TLocationCheck;
  Component: TComponent;
  Found: Boolean;
begin
  ActiveLoactionCheck:= LocationChecks.LocationCheck [0];
  WidthSum:= 0; HeightSum:= 0;
  for r:= 0 to 4 do
    LocationChecksPos [r]:= nil;

  Found:= False;
  for c:= Max (0, ActiveLoactionCheck.AnnouncedLeft- ActiveLoactionCheck.AnnouncedWidth) to
  Min (Image.Column- 1, ActiveLoactionCheck.AnnouncedLeft+ ActiveLoactionCheck.AnnouncedWidth ) do
  begin
    for r:= Max (0, ActiveLoactionCheck.AnnouncedTop- ActiveLoactionCheck.AnnouncedHeight) to
       Min (Image.Row- 1, ActiveLoactionCheck.AnnouncedTop+ ActiveLoactionCheck.AnnouncedHeight) do
       if Image.Body [r, c]= BLACK then
       begin
         Newr:= r;
         while Image.Body [Newr, c]= BLACK do
         begin
           Dec (Newr);
           if Newr< 0 then                    
             Break;

         end;
         Inc (Newr);

         Component:= ExtractContour (Image, Newr, c, dS);
         if (Threshold* ActiveLoactionCheck.AnnouncedWidth< Component.Width) and
           (Threshold* Component.Width< ActiveLoactionCheck.AnnouncedWidth) and
           (Threshold* (ActiveLoactionCheck.AnnouncedHeight)< Component.Height) and
           (Threshold* Component.Height< ActiveLoactionCheck.AnnouncedHeight) then
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
           Component.Free;

       end;
       
    if Found then
     Break;
  end;

  Found:= False;
  for c:= Max (0, ActiveLoactionCheck.AnnouncedRight- ActiveLoactionCheck.AnnouncedWidth) to
  Min (Image.Column- 1, ActiveLoactionCheck.AnnouncedRight+ ActiveLoactionCheck.AnnouncedWidth) do
  begin
    for r:= Max (0, ActiveLoactionCheck.AnnouncedTop- ActiveLoactionCheck.AnnouncedHeight) to
       Min (Image.Row- 1, ActiveLoactionCheck.AnnouncedTop+ ActiveLoactionCheck.AnnouncedHeight) do
       if Image.Body [r, c]= BLACK then
       begin
         Component:= ExtractContour (Image, r, c, dS);
         if (Threshold* ActiveLoactionCheck.AnnouncedWidth< Component.Width) and
           (Threshold* Component.Width< ActiveLoactionCheck.AnnouncedWidth) and
           (Threshold* (ActiveLoactionCheck.AnnouncedHeight)< Component.Height) and
           (Threshold* Component.Height< ActiveLoactionCheck.AnnouncedHeight) then
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
           Component.Free;

       end;
       
     if Found then
       Break;

  end;
  Found:= False;
  for c:= Max (0, ActiveLoactionCheck.AnnouncedLeft- ActiveLoactionCheck.AnnouncedWidth) to
  Min (Image.Column- 1, ActiveLoactionCheck.AnnouncedLeft+ ActiveLoactionCheck.AnnouncedWidth) do
  begin
    for r:= Max (0, ActiveLoactionCheck.AnnouncedBottom- ActiveLoactionCheck.AnnouncedHeight) to
       Min (Image.Row- 1, ActiveLoactionCheck.AnnouncedBottom+ ActiveLoactionCheck.AnnouncedHeight) do
       if Image.Body [r, c]= BLACK then
       begin
         Component:= ExtractContour (Image, r, c, dS);
         if (Threshold* ActiveLoactionCheck.AnnouncedWidth< Component.Width) and
           (Threshold* Component.Width< ActiveLoactionCheck.AnnouncedWidth) and
           (Threshold* (ActiveLoactionCheck.AnnouncedHeight)< Component.Height) and
           (Threshold* Component.Height< ActiveLoactionCheck.AnnouncedHeight) then
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
  for c:= Max (0, ActiveLoactionCheck.AnnouncedRight- ActiveLoactionCheck.AnnouncedWidth) to
  Min (Image.Column- 1, ActiveLoactionCheck.AnnouncedRight+ ActiveLoactionCheck.AnnouncedWidth) do
  begin
    for r:= Max (0, ActiveLoactionCheck.AnnouncedBottom- ActiveLoactionCheck.AnnouncedHeight) to
       Min (Image.Row- 1, ActiveLoactionCheck.AnnouncedBottom+ ActiveLoactionCheck.AnnouncedHeight) do
       if Image.Body [r, c]= BLACK then
       begin
         Component:= ExtractContour (Image, r, c, dS);
         if (Threshold* ActiveLoactionCheck.AnnouncedWidth< Component.Width) and
           (Threshold* Component.Width< ActiveLoactionCheck.AnnouncedWidth) and
           (Threshold* (ActiveLoactionCheck.AnnouncedHeight)< Component.Height) and
           (Threshold* Component.Height< ActiveLoactionCheck.AnnouncedHeight) then
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


  for r:= 1 to 4 do
    if LocationChecksPos [r]= nil then
      raise EInvalidLocationCheck.Create (IntToStr (r)+ 'th LocationCheck was not Found!');


  ActiveLoactionCheck.RealTopLeft:= LocationChecksPos [1];
  ActiveLoactionCheck.RealTopRight:= LocationChecksPos [2];
  ActiveLoactionCheck.RealBotLeft:= LocationChecksPos [3];
  ActiveLoactionCheck.RealBotRight:= LocationChecksPos [4];

  Skew.r:= ((LocationChecksPos [1].r- LocationChecksPos [2].r)+
           (LocationChecksPos [3].r- LocationChecksPos [4].r)) div 2;
  Skew.c:= ((LocationChecksPos [2].r- LocationChecksPos [1].r)+
           (LocationChecksPos [4].r- LocationChecksPos [3].r)) div 2;

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

procedure TICRRecognizer.Free;
begin
  FSVMClients.Free;

  inherited;
    
end;

procedure TICRRecognizer.Recognize (ABitmap: TBitmap;  AFormInfo: TFormInfo);
const
  MinCompWidth: Integer= 3;
  MinCompHeight: Integer= 3;
  MaxCompWidth: Integer= 200;
  MaxCompHeight: Integer= 200;
  
var
  SkewRegardingTLCheckLocation: TPoint;

  function ExtractAndRecognizeFieldSet (Image: TFMLImage;
     AFieldSet: TFieldSet): TImageCollection;

    function ExtractAFormItemInfo (Image: TFMLImage;
      AFormItemInfo: TFormItemInfo): TFMLImage;
    var
      TempImge: TFMLImage;
      i,
      r, c: Integer;
//      RepWithOutSkew: TPoint;
      TopLeft, BotRight: TPoint;
      Components: TComponentCollection;

    begin
      TopLeft:= AFormItemInfo.AnnouncedTopLeft.Copy;
      BotRight:= AFormItemInfo.AnnouncedBottomRight.Copy;
      
//      RepWithOutSkew:= 
      Components:= Image.FindAllComponentsInBox (TopLeft, BotRight, False);
      for i:= Components.Count- 1 downto 0 do
      begin
        if (Components.Component [i].Width<= MaxCompWidth) and
           (Components.Component [i].Height<= MaxCompHeight) and
          (MinCompWidth< Components.Component [i].Width) and
           (MinCompHeight< Components.Component [i].Height)then
        else
          Components.DeleteComponent (i, True);

      end;

      Result:= TFMLImage.Create (Components);
      Components.Free;

    end;

     
  var
    r, c: Integer;
    i: Integer;
    Ptr: PObject;
    ActiveFormItemInfo: TFormItemInfo;

  begin
    Result:= TImageCollection.Create;
    
    Ptr:= AFieldSet.GetPointerToFirst;

    for i:= 1 to AFieldSet.Size do
    begin
      ActiveFormItemInfo:= TFormItemInfo (Ptr^);
      Result.AddImage (ExtractAFormItemInfo (Image,
             ActiveFormItemInfo));
      Inc (Ptr);
      
    end;

  end;
  
var
  Image: TFMLImage;
  i, j, k: Integer;
  Ptr: PObject;
  Center: TPoint;
  FieldSetImageCollection: TImageCollection;
  ActiveFieldSet: TFieldSet;

begin
  Image:= TFMLImage.Create;
  Image.LoadBitMap (ABitmap);

  SkewRegardingTLCheckLocation:= TPoint.Create (0, 0);
  FindRealPositionOfLocationChecks (Image, AFormInfo.LocationCheckCollection,
    SkewRegardingTLCheckLocation);

  AssignFile (Output, 'C:\Temp.txt');
  Rewrite (Output);

  WriteLn (AFormInfo.LocationCheckCollection.LocationCheck [0].RealTopLeft.ToString);
  WriteLn (AFormInfo.LocationCheckCollection.LocationCheck [0].RealTopRight.ToString);
  WriteLn (AFormInfo.LocationCheckCollection.LocationCheck [0].RealBotLeft.ToString);
  WriteLn (AFormInfo.LocationCheckCollection.LocationCheck [0].RealBotRight.ToString);

  Center:= TPoint.Create (-AFormInfo.LocationCheckCollection.LocationCheck [0].AnnouncedTop+
     AFormInfo.LocationCheckCollection.LocationCheck [0].RealTopLeft.r,
    -AFormInfo.LocationCheckCollection.LocationCheck [0].AnnouncedLeft+
    AFormInfo.LocationCheckCollection.LocationCheck [0].RealTopLeft.c);

  Ptr:= AFormInfo.FormItemCollection.GetPointerToFirst;
  for i:= 0 to AFormInfo.FormItemCollection.Size- 1 do
  begin
    ActiveFieldSet:= TFieldSet (Ptr^);
    
    for j:= 0 to AFormInfo.FormItemCollection.ItemInfo [i].Size- 1 do
    begin
      ActiveFieldSet.ItemInfo [j].AnnouncedTopLeft.Move (Center);
      ActiveFieldSet.ItemInfo [j].AnnouncedBottomRight.Move (Center);

    end;

    Inc (Ptr);

  end;
  
  Center.Free;
  
  Ptr:= AFormInfo.FormItemCollection.GetPointerToFirst;
  for i:= 1 to AFormInfo.FormItemCollection.Size do
  begin
    ActiveFieldSet:= TFieldSet (Ptr^);

    FieldSetImageCollection:= ExtractAndRecognizeFieldSet (Image, ActiveFieldSet);
    FieldSetImageCollection.SaveFilesAsBitmap ('..\Samples\Extracted\');
    FieldSetImageCollection.Free;
    Inc (Ptr);
    
  end;
  
  CloseFile (Output);

  SkewRegardingTLCheckLocation.Free;
  Image.Free;
  
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
