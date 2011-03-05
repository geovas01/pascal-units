unit VoteUnit;

interface
uses
  FMLImage, HashUnit, CollectionUnit, ComponentsUnit,
  SVMClientUnit, SysUtils, FeatureUnit, GeometryUnit
  , Graphics, TemplateUnit;

const
  NumberOfDigitInVote= 5;
    
type
  ETextNotSet= class (Exception)
  public
    constructor Create;
    
  end;

  TVotingSVMClient= class (TSVMClient)
  private
    IthDigitSVMHandles: array [0..NumberOfDigitInVote] of array of Integer;
     
  public
    constructor Create (ConfigFile: String);
    procedure Free;

    function QueryOnIthDigit (DigitIndex: Integer; FeatureVector: TFeatureVectorBasedOnGradiant):
                 TDoubleCollection;

  end;
  
  TVoteEntry= class (TObject)
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

    function Recognize (SVMClient: TVotingSVMClient): String;

  end;

  TVotingEntryCollection= class (TGeneralImageCollection)
  private
    MinImagePropertiesForSpecialCase: TPoint;
    FSVMClient: TVotingSVMClient;

    function ExtractImages (NewImage: TFMLImage): TImageCollection;
    function GetVoteEntry (Index: Integer): TVoteEntry;

    function FindReplacement (Image: TFMLImage): TPoint;
    function FindSkewAndReplacement (Image: TFMLImage; Threshold: Extended= 2/3): TSkewReplacementInfo;

  public
    property VoteEntry [Index: Integer]: TVoteEntry read GetVoteEntry;
    property SVMClient: TVotingSVMClient read FSVMClient;

    constructor Create (ConfigFileName: String; SVMClient: TVotingSVMClient); 
    destructor Destroy; override;

    procedure LoadBitmap (Bitmap: TBitmap); overload;
    procedure Load (FileName: string);
    procedure LoadFromConfigFile (FileName: String);

    procedure Print;
    procedure RecognizeAll; 

  end;

implementation
uses
  ExceptionUnit, Math, MyTypes;
  
{ TVoteEntry }

constructor TVoteEntry.Create;
begin
  inherited;

  FComponentCollection:= TComponentCollection.Create;
  FBitmap:= nil;
  FText:= 'NOT SET YET!';
  
end;

procedure TVoteEntry.AddNewComponent (Component: TComponent);
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

procedure TVoteEntry.Free;

begin
  FTopLeft.Free;
  FBotRight.Free;
  FComponentCollection.Free;
  if FBitmap<> nil then
    FBitmap.Free;

  inherited Free;

end;

function TVoteEntry.IsComponentMine (Component: TComponent; OriginPos: TPoint): Boolean;
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
    FComponentCollection.Add (Component);
    
  end
  else
    Result:= False;

  Point.Free;

end;

procedure TVoteEntry.LoadFromFile (var InputFile: TextFile);
var
  S: String;

begin
  Readln (InputFile, S);

  Readln (InputFile, S);
  FTopLeft:= TPoint.Create (S);

  Readln (InputFile, S);
  FBotRight:= TPoint.Create (S);

end;

function TVotingEntryCollection.ExtractImages (NewImage: TFMLImage): TImageCollection;
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

procedure TVotingEntryCollection.Load (FileName: string);
var
  Bitmap: TBitmap;

begin
  Bitmap:= TBitmap.Create;
  Bitmap.LoadFromFile (FileName);

  LoadBitMap (Bitmap);

  Bitmap.Free;

end;

procedure TVotingEntryCollection.LoadBitmap (Bitmap: TBitmap);

  function IsFromSomeOtherSearch (ActiveComponent: TComponent;
    TopLeftSearchPoint, BotRightSearchPoint: TPoint): Boolean;
  var
    i, j: Integer;

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

var
  TempImage,
  NewImage: TFMLImage;
  i, j, SearchBoxIndex: Integer;
  Flag: Boolean;
  AllComponents1: TComponentCollection;
  ActiveComponent: TComponent;
  ActiveSearchBox: TBox;
  SkewAndReplacementInfo: TSkewReplacementInfo;
  Replacement: TPoint;
  SkewAngle: Extended;
  Ptr, CompPtr: PObject;
  
begin

  NewImage:= TFMLImage.Create;

  NewImage.LoadBitMap (Bitmap);
(*$ifdef DEBUG_MODE*)
  NewImage.GetAsBitmap (False).SaveToFile ('C:\NewImage.txt');
(*$endif*)

  SkewAndReplacementInfo:= FindSkewAndReplacement (NewImage);
  Replacement:= SkewAndReplacementInfo.Replacement;

  for SearchBoxIndex:= 0 to FSearchPoints.Size- 1 do
  begin
    Ptr:= FSearchPoints.GetPointerToFirst;
    (Ptr^ as TBox).Move (Replacement);

{
  Make the box bigger, not implemented yet!
    It can be done by rotating the topleft and bot right with appropriate angle
    and  ...

    FSearchPoints [CompIndex, 0].Rotate (FMainBoxBorder [0], SkewAngle);
    FSearchPoints [CompIndex, 1].Rotate (FMainBoxBorder [1], SkewAngle);
}
    Inc (Ptr);
    
  end;

  Ptr:= GetPointerToFirst;
  for i:= 0 to Size- 1 do
  begin
    (Ptr^ as TVoteEntry).FTopLeft.Move (Replacement);
    (Ptr^ as TVoteEntry).FBotRight.Move (Replacement);
    Inc (Ptr);

  end;

  Ptr:= FSearchPoints.GetPointerToFirst;
  for SearchBoxIndex:= 0 to FSearchPoints.Size- 1 do
  begin
    ActiveSearchBox:= (Ptr^ as TBox);
    AllComponents1:= NewImage.FindAllComponentsInBox (
      ActiveSearchBox.TopLeft, ActiveSearchBox.BotRight);
    Inc (Ptr);

    CompPtr:= AllComponents1.GetPointerToFirst;
    for i:= 0 to AllComponents1.Size- 1 do
    begin
      ActiveComponent:= CompPtr^ as TComponent;
      Inc (CompPtr);

      if (ActiveComponent.Width< MaxImageProperties.c) and
         (MinImageProperties.c< ActiveComponent.Width) and
         (ActiveComponent.Height< MaxImageProperties.r) and
         (MinImageProperties.r< ActiveComponent.Height) and
         (ActiveComponent.Percentage< PointsInImage.r) and
         (PointsInImage.c< ActiveComponent.Percentage) then
      begin

        if IsFromSomeOtherSearch (ActiveComponent,
           ActiveSearchBox.TopLeft,
           ActiveSearchBox.BotRight) then
          Continue;

        Flag:= False;
        for j:= 0 to Size- 1 do
          if VoteEntry [j].IsComponentMine (ActiveComponent, ActiveSearchBox.TopLeft) then
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
    end;

    AllComponents1.Clear;
    AllComponents1.Free;

  end;
  NewImage.Free;


  for i:= 0 to Size- 1  do
  begin
    Flag:= True;

    while Flag do
    begin
      Flag:= VoteEntry [i].MergeIfNecessary;
      Flag:= Flag or VoteEntry [i].SegmentIfNecessary;

    end;

  end;

  for SearchBoxIndex:= 0 to FSearchPoints.Size- 1 do
    FSearchPoints.SearchPoint [SearchBoxIndex].Move (-Replacement.c, -Replacement.r);

  for i:= 0 to Size- 1 do
  begin
    VoteEntry [i].FTopLeft.Move (-Replacement.c, -Replacement.r);
    VoteEntry [i].FBotRight.Move (-Replacement.c, -Replacement.r);

  end;

  SkewAndReplacementInfo.Replacement.Free;
  SkewAndReplacementInfo.Skew.Free;
  SkewAndReplacementInfo.SkewSource.Free;

end;

procedure TVotingEntryCollection.LoadFromConfigFile (FileName: String);
var
  InputFileHandle: TextFile;
  ServerPort: Integer;
  ServerHost,
  S: String;
  VoteEntry: TVoteEntry;
  i, VoteEntrySize: Integer;
  Count: Integer;

begin

  if FileExists (FileName) then
  begin
    
    AssignFile (InputFileHandle, FileName);
    Reset (InputFileHandle);

    ReadLn (InputFileHandle, S);
//    ReadLn (InputFileHandle, S);
    FMainBoxBorder:= TBox.Create;
    FMainBoxBorder.LoadFromFile (InputFileHandle);
    ReadLn (InputFileHandle, S);

    FGuideBoxes:= TGuideBoxCollection.Create;
    FGuideBoxes.LoadFromFile (InputFileHandle);

    ReadLn (InputFileHandle, S);
    FSearchPoints:= TSearchBoxCollection.Create;
    FSearchPoints.LoadFromFile (InputFileHandle);

    ReadLn (InputFileHandle, S);

{
    SetLength (FCenterPoints, 7);
    for i:= 1 to 6 do
    begin
      ReadLn (InputFileHandle, S);
      FCenterPoints [i]:= TPoint.Create (S);

    end;
}

    ReadLn (InputFileHandle, S);
    ReadLn (InputFileHandle, S);

    ReadLn (InputFileHandle, S);
    MinImageProperties:= TPoint.Create (S);

    ReadLn (InputFileHandle, S);
    ReadLn (InputFileHandle, S);
    MaxImageProperties:= TPoint.Create (S);

    ReadLn (InputFileHandle, S);
    ReadLn (InputFileHandle, S);
    MinImagePropertiesForSpecialCase:= TPoint.Create (S);

    ReadLn (InputFileHandle, S);
    ReadLn (InputFileHandle, S);
    PointsInImage:= TPoint.Create (S);

    ReadLn (InputFileHandle, S);
    ReadLn (InputFileHandle, S);
    ReadLn (InputFileHandle, VoteEntrySize);

    for i:= 1 to VoteEntrySize do
    begin
      VoteEntry:= TVoteEntry.Create;
      Self.Add (VoteEntry);

      VoteEntry.LoadFromFile (InputFileHandle);
      
    end;

    CloseFile (InputFileHandle);
    
  end
  else
    raise EFileNotFound.Create (FileName);

end;

constructor TVotingEntryCollection.Create (ConfigFileName: String;
    SVMClient: TVotingSVMClient);
begin
  inherited Create;

  FSVMClient:= SVMClient;
  MinImagePropertiesForSpecialCase:= nil;
  LoadFromConfigFile (ConfigFileName);

end;

function TVotingEntryCollection.GetVoteEntry (Index: Integer): TVoteEntry;
begin
  Result:= (Member [Index]) as TVoteEntry;
  
end;

procedure TVoteEntry.Print (BaseFileName: String);
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

procedure TVotingEntryCollection.Print;
var
  i: Integer;

begin
  for i:= 0 to Size- 1 do
    VoteEntry [i].Print (IntToStr (i));

end;

function TVotingEntryCollection.FindReplacement (Image: TFMLImage): TPoint;
var
  SkewAndReplacementInfo: TSkewReplacementInfo;
  
begin
  SkewAndReplacementInfo:= FindSkewAndReplacement (Image);

  Result:= SkewAndReplacementInfo.Replacement;

end;

function TVoteEntry.MergeIfNecessary: Boolean;
var
  MergeCandid1,
  MergeCandid2,
  SumOfBlack, 
  i, j: Integer;
  Temp,
  MinDist: Extended;
  ActiveComponent: TComponent;

begin
  Result:= False;
  
  if 5< ComponentCollection.Size then
  begin
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

       ComponentCollection.Component [MergeCandid2].Free;
       ComponentCollection.Delete (MergeCandid2);

    end;

  end;
  
end;

function TVoteEntry.GetBitmap: TBitmap;
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

procedure TVotingEntryCollection.RecognizeAll;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
  begin
    VoteEntry [i].Recognize (SVMClient);
//    Break;
    
  end;


end;

function TVoteEntry.GetText: String;
begin
  if FText= 'NOT SET YET' then
    raise ETextNotSet.Create;

  Result:= FText;
  
end;

var
  counter: integer;

function TVoteEntry.Recognize (SVMClient: TVotingSVMClient): String;
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
  for i:= 0 to FComponentCollection.Size- 1 do
  begin
    Component:= FComponentCollection.Component [i];
    DigitImage:= TFMLImage.Create (Component);
    DigitImage.Pattern:= UnImportantPattern;
    
//    DigitImage.SaveInFMLFile ('C:\QImage.FML');
//    DigitImage.SaveAsText ('C:\QImage.txt');
//    DigitImage.ExtractFeatures (50, 10);
    DigitResult:= SVMClient.QueryOnIthDigit (i+ 1, DigitImage.ExtractFeatures (35, 10));
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

function TVoteEntry.SegmentIfNecessary: Boolean;
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
  
  if ComponentCollection.Size< 5 then
  begin// Here we are going to check if the image can be segmented in two parts by a vertical line

    for i:= 0 to ComponentCollection.Size- 1 do
      if MinWidthThr< ComponentCollection.Component [i].Width then
      begin
        TempImage:= TFMLImage.Create (ComponentCollection.Component [i]);
        BlackPixInCols:= TempImage.BlackPixelCountInColumns;

        PointAvg:= Trunc (ComponentCollection.Component [i].Count/
            (ComponentCollection.Component [i].MaxC-
            ComponentCollection.Component [i].MinR));

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

{ TVotingSVMClient }

constructor TVotingSVMClient.Create (ConfigFile: String);
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

  for i:= 0 to NumberOfDigitInVote do
    SetLength (IthDigitSVMHandles [i], 0);

  inherited Create (ServerHost, ServerPort);

  for i:= 1 to NumberOfDigitInVote do
  begin
    Readln (InputFile, TempString);
    
    while Trim (TempString)<> '' do
    begin

      SetLength (IthDigitSVMHandles [i],
        Length (IthDigitSVMHandles [i])+ 1);
         
      SVMFile:= TempString;
      Readln (InputFile, TempString);
      KernelFile:= TempString;

      IthDigitSVMHandles [i,
        High (IthDigitSVMHandles [i])]:= OpenSVM (KernelFile, SVMFile);
        
      Readln (InputFile, TempString);
    
    end;
    
  end;

  CloseFile (InputFile);
  
end;

procedure TVotingSVMClient.Free;
var
  i: Integer;

begin
  for i:= 0 to High (IthDigitSVMHandles) do
    SetLength (IthDigitSVMHandles [i], 0);

  inherited Free;
  
end;

function TVotingSVMClient.QueryOnIthDigit (DigitIndex: Integer;
  FeatureVector: TFeatureVectorBasedOnGradiant): TDoubleCollection;
var
  i: Integer;

begin
  Result:= TDoubleCollection.Create;

  for i:= 0 to High (IthDigitSVMHandles [DigitIndex]) do
    Result.Add (Self.AskQuery (IthDigitSVMHandles [DigitIndex, i],
      FeatureVector));

end;

function TVotingEntryCollection.FindSkewAndReplacement (Image: TFMLImage;
    Threshold: Extended): TSkewReplacementInfo;
    
const
  AddC: array [1..8] of Integer=     (-1,  0, +1, +1, +1,  0, -1, -1);
  AddR: array [1..8] of Integer=     (-1, -1, -1,  0, +1, +1, +1,  0);
  TopRightScoreInc:
  array [1..8] of Integer=           ( 0, +1, +2, +1,  0, -1, -2, -1);
  TopLeftScoreInc:
  array [1..8] of Integer=           ( 2, +1,  0, -1, -2, -1,  0, +1);

  procedure FindLineIn (r, c: Integer; var Len, rTopLeft, rTopRight,
  cTopLeft, cTopRight: Integer);
  var
    i: Integer;
    TopLeftSum, TopRightSum: Integer;
    Newr1, Newc1, Newr2, Newc2: Integer;
    ActiveSum,
    State: Integer;
    ImageFBody: TArrayofArrayofInt;
    
  begin
    State:= 4;// to be set to UP when added by 5 in mod 8.

    Len:= 0;
    rTopLeft:= r; rTopRight:= r;
    cTopLeft:= c; cTopRight:= c;

{The following codes tries to find the Top Right point of the black line on which the pixel r, c is placed }
{We define an score measure for each point on the line which is formulated as follows:
  Image.Row+ c- r}
{
  ActiveSum is used to avoid recalculating the formulas
}

    Newr1:= r; Newc1:= c;
    TopRightSum:= Image.Row+ c- r;
    ImageFBody:= Image.GetBodyArray;
    ActiveSum:= TopRightSum;// Sum of Newr1 and Newc1
    
    while ImageFBody [Newr1, Newc1]= BLACK do
    begin
      State:= (State+ 5) mod 8+ 1;

      Inc (Len);
      for i:= 1 to 8 do
      begin
        Newr2:= Newr1+ AddR [State];
        Newc2:= Newc1+ AddC [State];

        if ImageFBody [Newr2, Newc2]= BLACK then
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
        rTopRight:= Newr1;
        cTopRight:= Newc1;

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
       
      Inc (Len);
      for i:= 1 to 8 do
      begin
        Newr2:= Newr1+ AddR [State];
        Newc2:= Newc1+ AddC [State];

        if ImageFBody [Newr2, Newc2]= BLACK then
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
        rTopLeft:= Newr1;
        cTopLeft:= Newc1;

      end;

      if ActiveSum+ 10< TopLeftSum{Thr1} then//Should be revised
        Break;

    end;

  end;

var
  i, j: Integer;
  r, c: Integer;
  Len, rTopLeft, rTopRight,
  cTopLeft, cTopRight: Integer;

begin
  c:= Image.Column div 2;
{
  This function tries to find a line whose length is about 2/3 of the page width and
  then returns its TopRight and TopLeft points. 
}
  for r:= 0 to Image.Row div 2 do
  begin
    if Image.Body [r, c]= BLACK then
    begin
      FindLineIn (r, c, Len, rTopLeft, rTopRight,
       cTopLeft, cTopRight);

      if Threshold* (FMainBoxBorder.BotRight.c- FMainBoxBorder.BotRight.c)<
          Len then
      begin
        Result.Skew:= TPoint.Create (rTopLeft- rTopRight, cTopLeft- cTopRight);
        Result.Replacement:= TPoint.Create (rTopLeft- FMainBoxBorder.TopLeft.r,
          cTopLeft- FMainBoxBorder.TopLeft.c);
        Result.SkewSource:= TPoint.Create (rTopLeft, cTopLeft);
        Exit;
        
      end;

    end;

  end;

end;

destructor TVotingEntryCollection.Destroy;
begin
  MinImagePropertiesForSpecialCase.Free;
  FSVMClient:= nil;

  inherited;
  
end;

end.
