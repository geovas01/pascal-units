unit ICLFeatureUnit;
(*$define _Debug_Matching_Details*)

interface
uses
  CollectionUnit, GeometryUnit, {HashUnit,}
  ComponentsUnit, SysUtils, MyTypes,
  StreamUnit;


const
  WeightArrayLen= 11;
  CONST_ICL_MIN_COMPONENT_SIZE = 2;
  CONST_USE_REGION_LABELS_FOR_DEBUG = False;
  
type
  EInvalidComponent= class (Exception);
  
  TWeightArray= array of Extended;

  EInvalidFreemanFeatureRow= class (Exception);

  TARowOfFreemanFeature= TLongWordCollection;
  TICLMode= (iclUp= 1, iclRight= 2, iclDown= 4,
    iclLeft= 8, iclBlackPixInFreeman= 16,
    iclHole= 0);
    TICLNeighberhoodDirection= (iclnUp= 1);
  TICLLabel= Byte;
  
  TICLFeatureComponent= class (TObject)
  private
    FPercentageOfPointsInComp: Integer;
    FPointCollection: TPointCollection;
    HashedData: THash;
    FCenterOfMass: TPoint;

    FICLLabel: TICLLabel;
    FID: Integer;
    FMinPoint, FMaxPoint: TPoint;
    function GetMaxPoint: TPoint;
    function GetMinPoint: TPoint;

  public
    property ID: Integer read FID;
    property ICLLabel: TICLLabel read FICLLabel;
    property PointsInComponent: TPointCollection read FPointCollection;
    property CenterOfMass: TPoint read FCenterOfMass;
    property MinPoint: TPoint read GetMinPoint;
    property MaxPoint: TPoint read GetMaxPoint;

    procedure AddPoint (Point: TPoint); overload;
    procedure AddPoint (r, c: Integer); overload;

    constructor Create (CompID: Integer; ICRLabel: TICLLabel); overload;
    constructor Create (ICLLabel: TICLLabel); overload;
    destructor Destroy; override;

    procedure Merge (AnotherComponent: TICLFeatureComponent);
    function GetNumberOfPointBetweenLines (MinY, MaxY: Integer;
          MinX, MaxX: Integer): Integer;

  end;

  TICLFeatureComponentCollection= class (TBaseCollection)
  private
    function GetComponent(Index: Integer): TICLFeatureComponent;
  public
    property Component [Index: Integer]: TICLFeatureComponent read GetComponent;

    constructor Create;

    procedure AddComponent (NewComponent: TICLFeatureComponent);
    procedure SetComponent (Index: Integer; NewComponent: TICLFeatureComponent);
    
  end;

  TICLNeighborRegion= class (TObject)
  private
  public
    constructor Create (RegionID: Integer;
       Direction: TICLNeighberhoodDirection);
    destructor Destroy; override;

  end;

  TICLNeighborRegionCollection= class (TBaseCollection)
  private
    function GetICLNeighbor(Index: Integer): TICLNeighborRegion;
    
  public
    property ICLNeighbor [Index: Integer]: TICLNeighborRegion read GetICLNeighbor;

    procedure AddICLNeighbor (NewICLNeighbor: TICLNeighborRegion);
    
  end;

  TMomentums= record
    momentum11,
    momentum20,
    momentum02:Double;
  end;

  TElongation= record
    Epsilon ,
    Phi ,
    x, y :Double;
  end;

  TICLDescriptor= class (TObject)
  private
    FCompAreaRegardingImArea: Integer;// Multiplied by 100
    FCOMCordRegardingImage: TPoint;// Multiplied by 100
    FLabel: TICLLabel;
    FPointsInRegion: Integer;
    FComponentHeightRegImHeight,
    FComponentWidthRegImWidth: Extended;
    FElongation: TElongation;

    
    constructor Create; overload;

  public
    property  CompAreaRegardingImArea: Integer
        read FCompAreaRegardingImArea;
    property COMCordRegardingImage: TPoint
      read FCOMCordRegardingImage;
    property PointsInRegion: Integer read FPointsInRegion;
    property ComponentHeightRegardingImageHeight: Extended read FComponentHeightRegImHeight;
    property ComponentWidthRegardingImageWidth: Extended read FComponentWidthRegImWidth;
    property ICLLabel: TICLLabel read FLabel;

    constructor Create (ICLRegion: TICLFeatureComponent;
      ImageHeigth, ImageWidth: Integer); overload;
    destructor Destroy; override;

    function FindDistance (AnotherICLDescriptor: TICLDescriptor;
    WeightedArray: TWeightArray): Extended;
    function ToString: String;
    function IsMatchable (AnotherICLDescriptor: TICLDescriptor): Boolean;
    function CalculateMomentum (ICLRegion: TICLFeatureComponent):TMomentums;
    procedure SaveToFile (var OutputFile: TextFile);
    procedure SaveToStream (var OutputStream: TMyFileStream);
    procedure FstSaveToFile(var OutputFile: TextFile;
      Selected : array of Boolean; CountOfSubWordParams: Integer; ConstInc: Double;
      RegionCoef: array of Integer);    procedure LoadFromFile (var InputFile: TextFile);

  end;

  TDistanceInfo= record
    Value: Extended;
    Mapped: array of Integer;
    ScoreForMap: array of Extended;
  end;

  TRelPosInfo = class
    public
      x,y : Double;
      function ToString: string;
      constructor Create; overload;
      constructor Create (Str: String);overload;
  end;

  TICLDescriptorCollection= class (TBaseCollection)
  private
    FImageWidth: Integer;
    FImageHeight: Integer;
    FImageBlackPixDensity: Double;//In percent
//    FRelImageWidth: Double;//In percent
    FRelImageHeight: Double;//In percent
    FBlackPercOnBaseLine: Double;//In percent
    FBlackPercUpperBaseLine: Double;//In percent
    FBlackPercBelowBaseLine: Double;//In percent
    FCOMRelPos: TRelPosInfo;

    function GetICLDescriptor (Index: Integer): TICLDescriptor;

  public
    property ICLDescriptor [Index: Integer]: TICLDescriptor read GetICLDescriptor;
    property ImageWidth: Integer read FImageWidth;
    property ImageHeight: Integer read FImageHeight;
    property BlackPercOnBaseLine: Double read FBlackPercOnBaseLine;
    property COMRelPos: TRelPosInfo read FCOMRelPos;

    constructor Create;
    destructor Destroy; override;

    function FindDistanceDetailed (AnotherICLDescCol: TICLDescriptorCollection
     ;WeightArray: TWeightArray): TDistanceInfo;
    function FindDistance (AnotherICLDescCol: TICLDescriptorCollection;
        WeightArray: TWeightArray): Extended;
    procedure AddICLDescriptor (NewICLDescriptor: TICLDescriptor);

    function ToString: String;

    procedure SaveToFile (var OutputFile: TextFile);
    procedure SaveToStream (OutputStream: TMyFileStream); 
    procedure LoadFromStream (InputStream: TMyFileStream); 

    procedure LoadFromFile (var InputFile: TextFile);
    procedure FstSaveToFile (var OutputFile: TextFile; Selected:array of Boolean;
          CountOfSubWordParams, MissingValue: Integer; ConstInc: Double;
          SubWordCoef, RegionCoef: array of Integer);

  end;

  TICLClassDefinition= class (TBaseCollection)
  private
    FClassName: String;
    FWeightArray: TWeightArray;
    function GetICLDescriptorCollection(
      Index: Integer): TICLDescriptorCollection;

  public
    property ClassName: String read FClassName;
    property ICLDescriptorCollection [Index: Integer]: TICLDescriptorCollection
      read GetICLDescriptorCollection;

    constructor Create; overload;
    constructor Create (ClassName: String; WeightArray: array of Extended); overload;

    procedure LoadFromFile (FileName: String); overload;
    procedure LoadFromFile (var InputFile: TextFile); overload;

    procedure SaveToFile (FileName: String); overload;
    procedure SaveToFile (var OutputFile: TextFile); overload;

    procedure AddICLDescriptorCollection (NewICLDescriptorCollection:
      TICLDescriptorCollection);

    function Evaluate (Query: TICLDescriptorCollection;
      WeightArray: TWeightArray): Extended;

  end;

  TICLClassDefinitionCollection= class (TBaseCollection)
  private
    function GetICLClassDefiniont (Index: Integer): TICLClassDefinition;

  public
    property ICLClassDefinition [Index: Integer]: TICLClassDefinition
      read GetICLClassDefiniont;

    constructor Create;

    procedure AddClassDefinition (NewICLClassDefinition: TICLClassDefinition);
    function FindClass (AQuery: TICLDescriptorCollection;
      WeightArray: TWeightArray): Integer;

    procedure SaveToFile (FileName: String);
    procedure LoadFromFile (FileName: String);

  end;

  TSubwordInfo= class (TObject)
  private
    FBaseLineHeightCoef: Integer;
    FBaseLineWidthCoef: Integer;
    FBaseLineHeight: Integer;
    FThickness: Integer;

  public
    property BaseLineHeightCoef: Integer read FBaseLineHeightCoef write FBaseLineHeightCoef;
    property BaseLineWidthCoef: Integer read FBaseLineWidthCoef write FBaseLineWidthCoef;
    property BaseLineHeight: Integer read FBaseLineHeight write FBaseLineHeight;
    property Thickness: Integer read FThickness write FThickness;

    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile (Filename: String);

  end;

  TFreemanFeature= class (TBaseCollection)
  private
    FColumns, FRows: Integer;
    FImageBlackPixDensity: Integer;
    FComponents: TICLFeatureComponentCollection;

    function GetARowofFreemanFeature(
      Index: Integer): TARowOfFreemanFeature;

  public
    property ARowofFreemanFeature [Index: Integer]: TARowOfFreemanFeature
      read GetARowofFreemanFeature;
    property Components: TICLFeatureComponentCollection read
      FComponents;

    procedure AddNextRowOfFreemanFeature
      (ARowOfFreemanFeature: TARowOfFreemanFeature);

    constructor Create (Row, Colum, BlackPixDensity: Integer);
    destructor Destroy; override;


    procedure FindComponents;
    function DescribeAllComponents (SubwordInfo: TSubwordInfo;
    ImageCoM: TPoint): TICLDescriptorCollection;
    function DescribeAllFeatures: String;
    
  end;

implementation

uses Math, ExceptionUnit, VarUtils, Classes;
type
  TBoolGraph= array of array of Boolean;
  
function HaveCompleteMatching (Graph: TBoolGraph): Boolean;
var
  RightMatchedWith,
  LeftMatchedWith: array of Integer;
  Visited: array of Boolean;
  i: Integer;
  LeftSideSize, RightSideSize: Integer;

  function DFS (LeftNodeIndex: Integer): Boolean;
  var
    i: Integer;

  begin
    for i:= 0 to RightSideSize- 1 do
      if (not Visited [i]) and (Graph [LeftNodeIndex, i]) then
      begin
        Visited [i]:= True;

        if RightMatchedWith [i]= -1 then
        begin
          RightMatchedWith [i]:= LeftNodeIndex;
          LeftMatchedWith [LeftNodeIndex]:= i;
          Result:= True;
          Exit;

        end
        else if DFS (RightMatchedWith [i]) then
        begin
          RightMatchedWith [i]:= LeftNodeIndex;
          LeftMatchedWith [LeftNodeIndex]:= i;
          Result:= True;
          Exit;

        end;

      end;

    Result:= False;

  end;

begin
  LeftSideSize:= Length (Graph);
  RightSideSize:= Length (Graph [0]);
  Result:= True;
  SetLength (RightMatchedWith, RightSideSize);
  SetLength (LeftMatchedWith, LeftSideSize);
  FillChar (RightMatchedWith [0], RightSideSize* SizeOf (Integer), 255);
  FillChar (LeftMatchedWith [0], LeftSideSize* SizeOf (Integer), 255);

  SetLength (Visited, RightSideSize);

  for i:= 0 to LeftSideSize- 1 do
  begin
    FillChar (Visited [0], RightSideSize* SizeOf (Boolean), 0);

    if not DFS (i) then
    begin
      Result:= False;
      Break;

    end;

  end;

  SetLength (Visited, 0);
  SetLength (RightMatchedWith, 0);
  SetLength (LeftMatchedWith, 0);
  
end;


{ TFreemanFeatute }

procedure TFreemanFeature.AddNextRowOfFreemanFeature (
  ARowOfFreemanFeature: TARowOfFreemanFeature);
begin
 if ARowOfFreemanFeature.Size<> FColumns then
    raise EInvalidFreemanFeatureRow.Create ('Invalid Size!');

  inherited Add (ARowOfFreemanFeature);

end;

constructor TFreemanFeature.Create (Row, Colum, BlackPixDensity: Integer);
begin
  inherited Create;
  
  FRows:= Row;
  FColumns:= Colum;
  FImageBlackPixDensity:= BlackPixDensity;
  
end;

function TFreemanFeature.DescribeAllComponents (SubwordInfo: TSubwordInfo;
    ImageCoM: TPoint): TICLDescriptorCollection;
var
  BlackPixOnBaseLine,
  BlackPixUpperBaseLine,
  BlackPixBelowBaseLine: Integer;

  function Describe (ICLRegion: TICLFeatureComponent): TICLDescriptor;
  var
    i: Integer;

  begin                          
    Result:= TICLDescriptor.Create (ICLRegion, FRows, FColumns);
    for i:=0 to ICLRegion.PointsInComponent.Size- 1 do
      if ICLRegion.PointsInComponent.Point [i].r<
           SubwordInfo.FBaseLineHeight then
        Inc(BlackPixUpperBaseLine)
      else if ICLRegion.PointsInComponent.Point [i].r >
            SubwordInfo.FBaseLineHeight + SubwordInfo.FThickness then
        Inc(BlackPixBelowBaseLine)
        else
          inc(BlackPixOnBaseLine);

    Result.FCompAreaRegardingImArea:= ICLRegion.FPercentageOfPointsInComp;

  end;

var
  i: Integer;
  Ptr: PObject;
  TotalPoints:Integer;
  
begin
  BlackPixOnBaseLine:= 0;
  BlackPixUpperBaseLine:= 0;
  BlackPixBelowBaseLine:= 0;
  Result:= TICLDescriptorCollection.Create;
  Result.FImageWidth:= FColumns;
  Result.FImageHeight:= FRows;
  Result.FImageBlackPixDensity:= FImageBlackPixDensity;

{  Result.FRelImageWidth:=
    FColumns/ (SubwordInfo.FThickness * SubwordInfo.FBaseLineWidthCoef);
  Result.FRelImageHeight:=
    FRows/ (SubwordInfo.FThickness * SubwordInfo.FBaseLineHeightCoef);
 }
  Result.FCOMRelPos.x:= ImageCoM.c/ FColumns;
  Result.FCOMRelPos.y:= ImageCoM.r / FRows;

  Ptr:= FComponents.GetPointerToFirst;

  if FComponents.Size<> 0 then
  begin
    for i:= 0 to FComponents.Size- 1 do
    begin
      Result.AddICLDescriptor (Describe (TICLFeatureComponent (Ptr^)));
      Inc (Ptr);

    end;

    TotalPoints:= BlackPixOnBaseLine+BlackPixUpperBaseLine+BlackPixBelowBaseLine;
    Result.FBlackPercOnBaseLine:= BlackPixOnBaseLine / TotalPoints;
    Result.FBlackPercUpperBaseLine:= BlackPixUpperBaseLine / TotalPoints;
    Result.FBlackPercBelowBaseLine:= BlackPixBelowBaseLine / TotalPoints;

  end;

end;

function TFreemanFeature.DescribeAllFeatures: String;
begin
  Result:= '';
  
end;

procedure TFreemanFeature.FindComponents;
const
  ICLCompMinThr= CONST_ICL_MIN_COMPONENT_SIZE;

var
  TotalWhitePixCount,
  r, c: Integer;
  IsVisited: array of array of Boolean;
  RowPtrs: array of PObject;
  IsVisitedPtr: PBoolean;
  PixPtr: POBject;
  ActiveComp: TICLFeatureComponent;
  TempPtr: PObject;

  procedure DFS (r, c: Integer; CurPtr: PObject);
  var
    ThisPixelLabel: LongWord;

  begin
    if not ActiveComp.HashedData.IsExist (r, c) then
    begin
      Inc (TotalWhitePixCount);
      
      ActiveComp.AddPoint (r, c);
      IsVisited [r, c]:= True;
      ThisPixelLabel:= PLongWord (CurPtr^)^;

      TempPtr:= CurPtr;
      if c+ 1< FColumns then
      begin
        Inc (TempPtr);// Right Point
        if ThisPixelLabel= PLongWord (TempPtr^)^ then
          DFS (r, c+ 1, TempPtr);

      end;

      TempPtr:= CurPtr;
      if 0< c then
      begin
        Dec (TempPtr);// Left Point
        if ThisPixelLabel= PLongWord (TempPtr^)^ then
          DFS (r, c- 1, TempPtr);

      end;

      TempPtr:= CurPtr;
      if r+ 1< FRows then
      begin
        TempPtr:= RowPtrs [r+ 1];
        Inc (TempPtr, c);

        if ThisPixelLabel= PLongWord (TempPtr^)^ then
          DFS (r+ 1, c, TempPtr);

      end;

      TempPtr:= CurPtr;
      if 0< r then
      begin
        TempPtr:= RowPtrs [r- 1];
        Inc (TempPtr, c);

        if ThisPixelLabel= PLongWord (TempPtr^)^ then
          DFS (r- 1, c, TempPtr);

      end;

    end;

  end;

  function CheckIfIsHole (r, c: Integer): Boolean;
  var
    PixPtr: PObject;

  begin
    IsVisited [r, c]:= False;
    Result:= False;
    PixPtr:= RowPtrs [r];
    Inc (PixPtr, c- 1);

    if 0< c then
    begin
      if (PLongWord (PixPtr^)^<> 0) then
      begin
        if (PLongWord (PixPtr^)^<> LongWord (Ord (iclBlackPixInFreeman)))  then
          Exit;

      end
      else
        if IsVisited [r, c- 1] then
          if not CheckIfIsHole (r, c- 1) then
            Exit;
          
    end;

    if c< FColumns- 1 then
    begin

      Inc (PixPtr, 2);

      if (PLongWord (PixPtr^)^<> 0) then
      begin
        if (PLongWord (PixPtr^)^<> LongWord (Ord (iclBlackPixInFreeman)))  then
          Exit;

      end
      else
        if IsVisited [r, c+ 1] then
          if not CheckIfIsHole (r, c+ 1) then

          Exit;
          
    end;

    if 0< r then
    begin
      PixPtr:= RowPtrs [r- 1];
      Inc (PixPtr, c);

      if (PLongWord (PixPtr^)^<> 0) then
      begin
        if (PLongWord (PixPtr^)^<> LongWord (Ord (iclBlackPixInFreeman))) then
          Exit;

      end
      else
        if IsVisited [r- 1, c]then
          if not CheckIfIsHole (r- 1, c) then
            Exit;
          
    end;

    if r< FRows- 1 then
    begin
      PixPtr:= RowPtrs [r+ 1];
      Inc (PixPtr, c);

      if (PLongWord (PixPtr^)^<> 0) then
      begin
        if (PLongWord (PixPtr^)^<> LongWord (Ord (iclBlackPixInFreeman))) then
          Exit;

      end
      else
        if IsVisited [r+ 1, c] then
          if not CheckIfIsHole (r+ 1, c) then
            Exit;

    end;

    Result:= True;

  end;


var
  i: Integer;
  CompPtr: PObject;
    
begin
  SetLength (IsVisited, FRows);
  SetLength (RowPtrs, FRows);

  for r:= 0 to FRows- 1 do
  begin
    SetLength (IsVisited [r], FColumns);
    FillChar (IsVisited [r, 0], SizeOf (Boolean)* FColumns, 0);
    RowPtrs [r]:= ARowofFreemanFeature [r].GetPointerToFirst;

  end;

  FComponents:= TICLFeatureComponentCollection.Create;

  TotalWhitePixCount:= 0;

  for r:= 0 to FRows- 1 do
  begin
    PixPtr:= RowPtrs [r];
    IsVisitedPtr:= @IsVisited [r, 0];

    for c:= 0 to FColumns- 1 do
    begin
      if (PLongWord (PixPtr^)^<> LongWord (Ord (iclBlackPixInFreeman))) and
        (not IsVisitedPtr^) then
      begin
        Inc (TotalWhitePixCount);
        ActiveComp:= TICLFeatureComponent.Create (PLongWord (PixPtr^)^);
        DFS (r, c, PixPtr);

        FComponents.AddComponent (ActiveComp)

      end;

      Inc (PixPtr);
      Inc (IsVisitedPtr);

    end;

  end;

  CompPtr:= FComponents.GetPointerToFirst;
  Inc (CompPtr, FComponents.Size- 1);

  for i:= FComponents.Size- 1 downto 0 do
  begin
    TICLFeatureComponent (CompPtr^).FPercentageOfPointsInComp:=
      (TICLFeatureComponent (CompPtr^).FPointCollection.Size* 100) div  (FRows* FColumns){TotalWhitePixCount};
    Dec (CompPtr);
  end;

  for i:= 0 to FComponents.Size- 1 do
  begin
    //All IsVisited Must be true
    if FComponents.Component [i].ICLLabel= 0 then
      if CheckIfIsHole (FComponents.Component [i].FPointCollection.Point [0].r,
       FComponents.Component [i].FPointCollection.Point [0].c) then
        FComponents.Component [i].FICLLabel:= Ord (iclHole);
  end;

  CompPtr:= FComponents.GetPointerToFirst;
  Inc (CompPtr, FComponents.Size- 1);
  for i:= FComponents.Size- 1 downto 0 do
  begin
    if (100* TICLFeatureComponent (CompPtr^).FPointCollection.Size <
          FRows* FColumns* ICLCompMinThr)
       and (TICLFeatureComponent (CompPtr^).FICLLabel <> Byte (Ord (iclHole))) then
      FComponents.Delete (i);

    Dec (CompPtr);
  end;

  for r:= 0 to FRows- 1 do
    SetLength (IsVisited [r], 0);
  SetLength (IsVisited, 0);
  SetLength (RowPtrs, 0);

end;

destructor TFreemanFeature.Destroy;
begin
  if FComponents<> nil then
    FComponents.Free;

  inherited;
  
end;

function TFreemanFeature.GetARowofFreemanFeature ( 
  Index: Integer): TARowOfFreemanFeature;
begin
  Result:= Member [Index] as TARowOfFreemanFeature;
  
end;

{ TICLFeatureComponents }

procedure TICLFeatureComponent.AddPoint (Point: TPoint);
begin
  AddPoint (Point.r, Point.c);
  
end;

procedure TICLFeatureComponent.AddPoint(r, c: Integer);
var
  NewPoint: TPoint;

begin
  if HashedData.IsExist (r, c) then
    Exit;

  if FPointCollection.Size= 0 then
  begin
    if FMinPoint<> nil then
      FMinPoint.Free;
    if FMaxPoint<> nil then
      FMaxPoint.Free;

    FMinPoint:= TPoint.Create (r, c);
    FMaxPoint:= TPoint.Create (r, c);

  end
  else
  begin
    if r< FMinPoint.r then
      FMinPoint.r:= r
    else if FMaxPoint.r< r then
      FMaxPoint.r:= r;

    if c< FMinPoint.c then
      FMinPoint.c:= c
    else if FMaxPoint.c< c then
      FMaxPoint.c:= c;

  end;

  FCenterOfMass.Move (r, c);

  HashedData.Insert (r, c);

  NewPoint:= TPoint.Create (r, c);
  FPointCollection.AddPoint (NewPoint);
  
end;

constructor TICLFeatureComponent.Create (CompID: Integer; ICRLabel: TICLLabel);
begin
  inherited Create;

  FICLLabel:= ICLLabel;
  FID:= CompID;

  FPointCollection:= TPointCollection.Create;
  HashedData:= THash.Create;
  FCenterOfMass:= TPoint.Create (0, 0);

  FMinPoint:= nil;
  FMaxPoint:= nil;

end;

constructor TICLFeatureComponent.Create (ICLLabel: TICLLabel);
begin
  inherited Create;

  FICLLabel:= ICLLabel;
  FID:= 0;

  FPointCollection:= TPointCollection.Create;
  HashedData:= THash.Create;
  FCenterOfMass:= TPoint.Create (0, 0);

  FMinPoint:= nil;
  FMaxPoint:= nil;

end;

destructor TICLFeatureComponent.Destroy;
begin

  HashedData.Free;
  FCenterOfMass.Free;
  FPointCollection.Free;

  if FMinPoint<> nil then
    FMinPoint.Free;
  if FMaxPoint<> nil then
    FMaxPoint.Free;
  
  inherited;

end;

{ TICLFeatureComponentCollection }

procedure TICLFeatureComponentCollection.AddComponent(
  NewComponent: TICLFeatureComponent);
begin
  inherited Add (NewComponent);
  
end;

constructor TICLFeatureComponentCollection.Create;
begin
  inherited Create;

end;

function TICLFeatureComponentCollection.GetComponent (
  Index: Integer): TICLFeatureComponent;
begin
  Result:= Member [Index] as TICLFeatureComponent;

end;

function TICLFeatureComponent.GetMaxPoint: TPoint;
begin
  if FPointCollection.Size= 0 then
    Result:= TPoint.Create (0, 0)
  else
    Result:= FMaxPoint.Copy;

end;

function TICLFeatureComponent.GetMinPoint: TPoint;
begin
  if FPointCollection.Size= 0 then
    Result:= TPoint.Create (0, 0)
  else
    Result:= FMinPoint.Copy;

end;

function TICLFeatureComponent.GetNumberOfPointBetweenLines (MinY,
  MaxY: Integer; MinX, MaxX: Integer): Integer;
var
  x, y: Integer;

begin
  Result:= 0;
  
  for x:= MinX to MaxX do
  begin
    for y:= MinY to MaxY do
      if HashedData.IsExist (x, y) then
        Inc (Result);

  end;

end;

procedure TICLFeatureComponent.Merge(
  AnotherComponent: TICLFeatureComponent);
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

      Self.AddPoint (Point.r, Point.c);

    end;

  end;

end;

procedure TICLFeatureComponentCollection.SetComponent(Index: Integer;
  NewComponent: TICLFeatureComponent);
begin
  FMembers [Index]:= NewComponent;

end;

function BalancedDiff (Source, Data, Max: Extended): Extended;
begin
  if Abs (Source)< 1e-10 then
  begin
    if Abs (Data)< 1e-10 then
      Result:= Sign (Data)* Sign (Source)
    else
      Result:= 0;

    if Max< Result then
      Result:= Max;
    Exit;
    
  end;

  Result:= Abs ((Source- Data)/ Source);
  if Max< Result then
    Result:= Max;

end;

{ TICLDescriptor }

function TICLDescriptor.CalculateMomentum(ICLRegion: TICLFeatureComponent): TMomentums;
var
  COMr,COMc: double;
  i: Integer;
  NewFCenterOfMass: TPoint;
  r,c: Double;

begin

  NewFCenterOfMass := ICLRegion.CenterOfMass.Copy;
  NewFCenterOfMass.Move (Round (ICLRegion.FPointCollection.Size * 0.5)
      , Round (ICLRegion.FPointCollection.Size * 0.5));

  COMc:= NewFCenterOfMass.c/ ICLRegion.FPointCollection.Size;
  COMr:= NewFCenterOfMass.r/ ICLRegion.FPointCollection.Size;

  Result.momentum11 := 0;
  Result.momentum20 := 0;
  Result.momentum02 := 0;
  for i:=0 to ICLRegion.PointsInComponent.Size-1 do
  begin
    c:= (ICLRegion.FPointCollection.Point [i].c + 0.5) / ICLRegion.FPointCollection.Size;
    r:= (ICLRegion.FPointCollection.Point [i].r + 0.5) / ICLRegion.FPointCollection.Size;

    Result.momentum11:=Result.momentum11+ (r- COMr)*(c- COMc);
    Result.momentum20:=Result.momentum20+ sqr (c- COMc);
    Result.momentum02:=Result.momentum02+ sqr (r- COMr);
    
  end;

end;

constructor TICLDescriptor.Create (ICLRegion: TICLFeatureComponent;
      ImageHeigth, ImageWidth: Integer);
var
  Momentoms: TMomentums;
  NewFCenterOfMass: TPoint;

begin
  inherited Create;


  FLabel:= ICLRegion.ICLLabel;
  FCompAreaRegardingImArea:=
         Round (ICLRegion.FPercentageOfPointsInComp);

  NewFCenterOfMass:= ICLRegion.CenterOfMass.Copy;
  NewFCenterOfMass.c:= NewFCenterOfMass.c + Round (ICLRegion.FPointCollection.Size * 0.5);
  NewFCenterOfMass.r:= NewFCenterOfMass.r + Round (ICLRegion.FPointCollection.Size * 0.5);
  FCOMCordRegardingImage:= NewFCenterOfMass.Copy.Scale
     (100/ (ImageWidth* ICLRegion.FPointCollection.Size),
       100/ (ImageHeigth* ICLRegion.FPointCollection.Size));

  FPointsInRegion:= ICLRegion.FPointCollection.Size;

  FComponentHeightRegImHeight:= (ICLRegion.MaxPoint.r- ICLRegion.MinPoint.r+ 1)/
           ImageHeigth;
  FComponentWidthRegImWidth:= (ICLRegion.MaxPoint.c- ICLRegion.MinPoint.c+ 1)/
               ImageWidth;

  Momentoms:= CalculateMomentum (ICLRegion);
  if (Abs (Momentoms.momentum20)< 1e-10) and (Abs (Momentoms.momentum02)< 1e-10) then
  begin
    //the region is just a point
    FElongation.Epsilon := 0;
    FElongation.Phi := 0;
    
  end
  else if Abs (Momentoms.momentum20) < 1e-10 then
  begin
    // the region is a one-pixel width column
    FElongation.Epsilon := 1;
    FElongation.Phi := pi / 2;
    
  end
  else if Abs (Momentoms.momentum02) < 1e-10 then
  begin
    // the region is a one-pixel width row
    FElongation.Epsilon := 1;
    FElongation.Phi := 0;

  end
  else
  begin
    FElongation.Epsilon:= (Sqr (Momentoms.momentum20 - Momentoms.momentum02)+
                            4* Sqr(Momentoms.momentum11)) /
                            Sqr (Momentoms.momentum20 + Momentoms.momentum02);

    if abs (Momentoms.momentum20 - Momentoms.momentum02)<1e-10 then
      FElongation.Phi:= Pi/ 2
    else
      FElongation.Phi:= 0.5 *ArcTan (2 * Momentoms.momentum11 /
              (Momentoms.momentum20 - Momentoms.momentum02));
              
  end;
  
  FElongation.Phi:= FElongation.Phi+ pi/ 2;
  FElongation.x:= FElongation.Epsilon*Sin(2* FElongation.Phi);
  FElongation.y:= FElongation.Epsilon*Cos(2* FElongation.Phi);
  FElongation.Phi := FElongation.Phi * (180 / pi);
  FElongation.Phi:= (FElongation.Phi+ 90)/ 180;


end;

constructor TICLDescriptor.Create;
begin
  inherited;

  FCOMCordRegardingImage:= nil;

end;

function TICLDescriptor.FindDistance (
  AnotherICLDescriptor: TICLDescriptor;
  WeightedArray: TWeightArray): Extended;
begin
                                                              
  if FLabel in [3, 6, 12, 9] then// Donbalan
   Result:= WeightedArray [6]* BalancedDiff (FComponentHeightRegImHeight,
      AnotherICLDescriptor.FComponentHeightRegImHeight, 1)
  else
   Result:= WeightedArray [7]* BalancedDiff (FCompAreaRegardingImArea,
      AnotherICLDescriptor.FCompAreaRegardingImArea, 1);

   if not (FLabel in [3, 6, 12, 9]) then//Donbalan
     Result:= Result+ WeightedArray [8]* (BalancedDiff (FCOMCordRegardingImage.r,
      AnotherICLDescriptor.FCOMCordRegardingImage.r, 1));


   Result:= Result+ WeightedArray [9]* (BalancedDiff (FCOMCordRegardingImage.r,
      AnotherICLDescriptor.FCOMCordRegardingImage.r, 1));

{
  if FLabel= 17 then
    Result:= WeightedArray [10]* Result;
}
end;

destructor TICLDescriptor.Destroy;
begin

  FCOMCordRegardingImage.Free;

  inherited;
  
end;

function IsPow2 (n: Integer): Boolean;
var
  i, j: Integer;

begin

  Result:= False;
  j:= 1;
    
  for i:= 1 to 31 do
  begin
    if n and j<> 0 then
    begin
      if Result then
      begin
        Result:= False;
        Exit;
          
      end;
      Result:= True;
        
    end;

    j:= j shl 1;
      
  end;

  Result:= True;
    
end;

procedure TICLDescriptor.FstSaveToFile(var OutputFile: TextFile;
      selected : array of Boolean;CountOfSubWordParams: Integer; ConstInc: Double;
      RegionCoef: array of Integer);
begin
  if FLabel= 17 then
  begin
    if selected [CountOfSubWordParams] then
    begin
      Writeln (OutputFile,(RegionCoef[0]* FComponentHeightRegImHeight)+ ConstInc:0:3);
//      Writeln (OutputFile,(RegionCoef[0]* FComponentWidthRegImWidth)+ ConstInc:0:3);
    end;
    if selected[CountOfSubWordParams+ 1] then
      Writeln (OutputFile, (RegionCoef[1]* Sqrt(FCompAreaRegardingImArea/ 100))+ ConstInc:0:3);
  end
  else
  begin
    if selected[CountOfSubWordParams] then
    begin
      Writeln (OutputFile, FComponentHeightRegImHeight+ ConstInc:0:3);
//      Writeln (OutputFile, FComponentWidthRegImWidth+ ConstInc:0:3);
    end;
    if selected[CountOfSubWordParams+ 1] then
      Writeln (OutputFile, Sqrt(FCompAreaRegardingImArea/ 100)+ ConstInc:0:3);

  end;

  if selected[CountOfSubWordParams+ 2] then
  begin
    Writeln(OutputFile,(FCOMCordRegardingImage.r / 100)+ ConstInc:0:3);
    Writeln(OutputFile,(FCOMCordRegardingImage.c / 100)+ ConstInc:0:3);
    
  end;
  
  if selected[CountOfSubWordParams+ 3] then
  begin
    Writeln(OutputFile, FElongation.Epsilon+ ConstInc:0:3);
    Writeln(OutputFile, FElongation.Phi+ ConstInc:0:3);
    
  end;
  
  if selected [CountOfSubWordParams+ 4] then
  begin
    Writeln (OutputFile,FElongation.x+ ConstInc:0:3);
    Writeln (OutputFile,FElongation.y+ ConstInc:0:3);

  end;
  
end;

function TICLDescriptor.IsMatchable (
  AnotherICLDescriptor: TICLDescriptor): Boolean;

begin

  if FLabel= Byte (Ord (iclHole)) then
  begin
    if AnotherICLDescriptor.FLabel in [0, 14, 13, 11, 7, 17] then
      Result:= True
    else
      Result:= False;

  end
  else if IsPow2 (AnotherICLDescriptor.FLabel xor FLabel) or
    (FLabel= AnotherICLDescriptor.FLabel) then
    Result:= True

  else if AnotherICLDescriptor.FLabel= Byte (Ord (iclHole)) then
  begin
    if FLabel in [0, 14, 13, 11, 7, 17] then
      Result:= True
    else
      Result:= False;
      
  end
  else
    Result:= False;


  Result:= Result and
  (
  (FCompAreaRegardingImArea<= 3* AnotherICLDescriptor.FCompAreaRegardingImArea) and
  (AnotherICLDescriptor.FCompAreaRegardingImArea<= 3* FCompAreaRegardingImArea))
  
end;

procedure TICLDescriptor.LoadFromFile (var InputFile: TextFile);
var
  S: String;
  
begin
  ReadLn (InputFile, FCompAreaRegardingImArea);
  ReadLn (InputFile, S);
  if FCOMCordRegardingImage<> nil then
    FCOMCordRegardingImage.Free;
  FCOMCordRegardingImage:= TPoint.Create (S);
  
  ReadLn (InputFile, FLabel);
  ReadLn (InputFile, FPointsInRegion);
  ReadLn (InputFile, FComponentHeightRegImHeight);
  ReadLn (InputFile, FComponentWidthRegImWidth);

end;

procedure TICLDescriptor.SaveToFile (var OutputFile: TextFile);
begin
  WriteLn (OutputFile, 'FCompAreaRegardingImArea= ', FCompAreaRegardingImArea);
  WriteLn (OutputFile, 'FCOMCordRegardingImage= ', FCOMCordRegardingImage.ToString);
  WriteLn (OutputFile, 'FLabel= ', FLabel);
  WriteLn (OutputFile, 'FPointsInRegion= ', FPointsInRegion);
  WriteLn (OutputFile, 'FComponentHeightRegImHeight= ', FComponentHeightRegImHeight);
  WriteLn (OutputFile, 'FComponentWidthRegImWidth= ', FComponentWidthRegImWidth);

end;

function TICLDescriptor.ToString: String;
begin
  Result:= IntToStr (FLabel)+ ': RelSize= '+ IntToStr (FCompAreaRegardingImArea)+ #$0A#$0D+
     ' COM='+ FCOMCordRegardingImage.ToString;

end;

procedure TICLDescriptor.SaveToStream(var OutputStream: TMyFileStream);
begin
  OutputStream.WriteLine ('FCompAreaRegardingImArea= '+ IntToStr (FCompAreaRegardingImArea));
  OutputStream.WriteLine ('FCOMCordRegardingImage= '+ FCOMCordRegardingImage.ToString);
  OutputStream.WriteLine ('FLabel= '+  IntToStr (Ord (FLabel)));
  OutputStream.WriteLine ('FPointsInRegion= '+ IntToStr (FPointsInRegion));
  OutputStream.WriteLine ('FComponentHeightRegImHeight= '+ FloatToStr (FComponentHeightRegImHeight));
  OutputStream.WriteLine ('FComponentWidthRegImWidth= '+ FloatToStr (FComponentWidthRegImWidth));
  OutputStream.WriteLine ('');

end;

{ TICLNeighborRegion }

constructor TICLNeighborRegion.Create(RegionID: Integer;
  Direction: TICLNeighberhoodDirection);
begin

end;

destructor TICLNeighborRegion.Destroy;
begin

  inherited;
end;

{ TICLNeighborRegionCollection }

procedure TICLNeighborRegionCollection.AddICLNeighbor(
  NewICLNeighbor: TICLNeighborRegion);
begin
  inherited Add (NewICLNeighbor);

end;

function TICLNeighborRegionCollection.GetICLNeighbor (
  Index: Integer): TICLNeighborRegion;
begin
  Result:= Member [Index] as TICLNeighborRegion;
  
end;


{ TICLDescriptorCollection }

procedure TICLDescriptorCollection.AddICLDescriptor(
  NewICLDescriptor: TICLDescriptor);
begin
  inherited Add (NewICLDescriptor);
  
end;

constructor TICLDescriptorCollection.Create;
begin
  inherited;

  //FCOMRelPos:= nil;
  FCOMRelPos := TRelPosInfo.Create;
  
end;

function TICLDescriptorCollection.FindDistanceDetailed (
  AnotherICLDescCol: TICLDescriptorCollection;
        WeightArray: TWeightArray): TDistanceInfo;
type
  TWeightedGraph= array of array of Extended;
  TLeftSidesMatchedWith= array of Integer;
   
  TMatchingDetails= record
    LeftSidesMatch: TLeftSidesMatchedWith;
    MatchingSize: Integer;

  end;

const
  MaxCost= 1.35;//0.65+ 2* 0.35;

{
 Based on http://www.public.iastate.edu/~ddoty/HungarianAlgorithm.html
}
  function FindMinWeightedMatching (Graph: TWeightedGraph): TLeftSidesMatchedWith;
  var
    RowCover, ColCover: array of Boolean;
    XSize, YSize: Integer;
    MinWeight: Extended;
    Mask: array of array of Byte;

    procedure Step1;
    var
      i, j: Integer;
      ExPtr: PExtended;

    begin
      for i:= 0 to XSize- 1 do
      begin
        MinWeight:= 1e100;
        ExPtr:= @Graph [i, 0];

        for j:= 0 to YSize- 1 do
        begin
          if ExPtr^< MinWeight then
            MinWeight:= ExPtr^;
          Inc (ExPtr);
          
        end;

        ExPtr:= @Graph [i, 0];
        for j:= 0 to YSize- 1 do
        begin
          ExPtr^:= Abs (ExPtr^- MinWeight);// To avoid negative weights due !!!
          Inc (ExPtr);

        end;

      end;

    end;

    procedure Step2;
    var
      r, c: Integer;
      ExPtr: PExtended;

    begin
      
      for r:= 0 to XSize- 1 do
        if not RowCover [r] then
        begin
          ExPtr:= @Graph [r, 0];
          for c:= 0 to YSize- 1 do
          begin
            if (ExPtr^< 1e-10) and (not ColCover [c]) then
            begin
              Mask [r, c]:= 1;
              RowCover [r]:= True;
              ColCover [c]:= True;
              Break;

            end;

            Inc (ExPtr);

          end;

        end;

      FillChar (RowCover [0], XSize* SizeOf (Boolean), 0);
      FillChar (ColCover [0], YSize* SizeOf (Boolean), 0);

    end;

    function Step3: Integer;
    var
      i, j: Integer;
      BPtr: PByte;
      
    begin
      Result:= 0;
      for i:= 0 to XSize- 1 do
      begin
        BPtr:= @Mask [i, 0];

        for j:= 0 to YSize- 1 do
        begin
          if BPtr^= 1 then
          begin
            ColCover [j]:= True;
            Break;
            
          end;
          Inc (BPtr);
          
        end;

      end;

      for j:= 0 to YSize- 1 do
        if ColCover [j] then
          Inc (Result);

    end;

    type
      TPoint= record
        r, c: Integer;
      end;

    var
      ZeroPoint: TPoint;

    function Step4: Boolean;

      function FindAZero: TPoint;
      var
        r, c: Integer;
        ExPtr: PExtended;

      begin
        Result.r:= -1;
        for r:= 0 to XSize- 1 do
        begin
          ExPtr:= @Graph [r, 0];
          Inc (ExPtr, YSize- 1);

          if not RowCover [r] then
            for c:= YSize- 1 downto 0 do
            begin
              if (not ColCover [c]) and (ExPtr^< 1e-10) then
              begin
                Result.r:= r;
                Result.c:= c;
                Exit;

              end;

              Dec (ExPtr);

            end;

        end;


      end;

      function HaveStarInRow (Row: Integer): Boolean;
      var
       c: Integer;
       BPtr: PByte;

      begin
        Result:= True;

        BPtr:= @Mask [Row, 0];

        for c:= 1 to YSize do
        begin
          if BPtr^= 1 then
            Exit;

          Inc (BPtr);

        end;

        Result:= False;

      end;

      function FindStarInRow (ARow: Integer): Integer;
      var
        BPtr: PByte;

      begin
        BPtr:= @Mask [ARow, 0];

        for Result:= 0 to YSize- 1 do
        begin
          if BPtr^= 1 then
            Exit;

          Inc (BPtr);

        end;

        Result:= -1;

      end;

    var
      c: Integer;
      ZeroEntry: TPoint;

    begin

      Result:= False;

      while True do
      begin
        ZeroEntry:= FindAZero;

        if ZeroEntry.r< 0 then
        begin
          Result:= True;
          Exit;
          
        end;

        Mask [ZeroEntry.r, ZeroEntry.c]:= 2;
        
        c:= FindStarInRow (ZeroEntry.r);
        if c< 0 then
        begin
          ZeroPoint:= ZeroEntry;
          Break;

        end
        else
        begin
          RowCover [ZeroEntry.r]:= True;	//Cover the star's row.
          ColCover [c]:= False;

        end;

      end;

    end;

    procedure Step5;

      function FindStarInCol (Column: Integer): Integer;
      begin
        for Result:= 0 to XSize- 1 do
          if Mask [Result, Column]= 1 then
            Exit;

        Result:= -1;
        
      end;

      function FindPrimeInRow (ARow: Integer): Integer;
      var
        BPtr: PByte;

      begin
        BPtr:= @Mask [ARow, 0];

        for Result:= 0 to XSize- 1 do
        begin
          if BPtr^= 2 then
            Exit;
            
          Inc (BPtr);

        end;

        Result:= -1;

      end;


    procedure ErasePrimes;
    var
      r, c: Integer;
      BPtr: PByte;

    begin
      for r:= 0 to XSize- 1 do
      begin
        BPtr:= @Mask [r, 0];
        for c:= 1 to YSize do
        begin
          if BPtr^= 2 then
            BPtr^:= 0;
          Inc (BPtr);
          
        end;

      end;

    end;

    var
      Count: Integer;
      Path: array of TPoint;

    procedure ConvertPath;
    var
      i: Integer;
    begin
      for i:= 0 to Count- 1 do
        if Mask [Path [i].r ,Path [i].c]= 1 then
          Mask [Path [i].r ,Path [i].c]:= 0
        else
          Mask [Path [i].r ,Path [i].c]:= 1;

    end;

    var
      ActivePoint: TPoint;
      r, c: Integer;

    begin
      SetLength (Path, XSize);
      Path [0]:= ZeroPoint;
      ActivePoint:= Path [0];
      Count:= 1;

      while True do
      begin
        r:= FindStarInCol (ActivePoint.c);
        if 0<= r then
        begin
          if Count= Length (Path) then
            SetLength (Path, Length (Path)+ XSize);

          Inc (Count);
          Path [Count- 1].r:= r;
          Path [Count- 1].c:= ActivePoint.c;
          ActivePoint:= Path [Count- 1];

        end
        else
          Break;

        c:= FindPrimeInRow (ActivePoint.r);
        Inc (Count);
        if Length (Path)< Count then
          SetLength (Path, Length (Path)+ XSize);

        Path [Count- 1].r:= ActivePoint.r;
        Path [Count- 1].c:= c;
        ActivePoint:= Path [Count- 1];

      end;

      ConvertPath;
      FillChar (RowCover [0], SizeOf (Boolean)* XSize, 0);
      FillChar (ColCover [0], SizeOf (Boolean)* YSize, 0);

      ErasePrimes;

    end;

    procedure Step6;
    var
      Smallest: Extended;
      ExPtr: PExtended;
      r, c: Integer;

    begin
      Smallest:= 1e100;

      for r:= 0 to XSize- 1 do
      begin
        if not RowCover [r] then
        begin
          ExPtr:= @Graph [r, 0];
          for c:= 0 to YSize- 1 do
          begin
            if not ColCover [c] then
              if ExPtr^< Smallest then
                Smallest:= ExPtr^;

            Inc (ExPtr);

          end;

        end;

      end;

      for r:= 0 to XSize- 1 do
        for c:= 0 to YSize- 1 do
        begin
          if RowCover [r] then
            Graph [r, c]:= Graph [r, c]+ Smallest;

          if not ColCover [c] then
            Graph [r, c]:= Graph [r, c]- Smallest;

        end;

    end;

  var
    r, c: Integer;

  begin
    XSize:= Length (Graph);
    YSize:= Length (Graph [0]);

    SetLength (RowCover, XSize);
    SetLength (ColCover, YSize);
    FillChar (RowCover [0], XSize* SizeOf (Boolean), 0);
    FillChar (ColCover [0], YSize* SizeOf (Boolean), 0);

    SetLength (Mask, XSize);

    for r:= 0 to XSize- 1 do
    begin
      SetLength (Mask [r], YSize);
      FillChar (Mask [r, 0], YSize* SizeOf (Byte), 0);

    end;

    Step1;
    Step2;

    while Step3<> YSize do
    begin
      if Step4 then
      begin
        
        repeat
          Step6
          
        until not Step4;

        Step5;
      end
      else
        Step5;

    end;

    SetLength (Result, XSize);
    FillChar (Result [0], XSize* SizeOf (Integer), 255);

    for r:= 0 to XSize- 1 do
    begin
      for c:= 0 to YSize- 1 do
        if Mask [r, c]= 1 then
        begin
          Result [r]:= c;
          Break;

        end;

    end;

    for r:= 0 to XSize- 1 do
      SetLength (Mask [r], 0);
    SetLength (Mask, 0);

    SetLength (RowCover, 0);
    SetLength (ColCover, 0);

  end;
const
  Inf= 1e3;

var
  MatchingInfo: TLeftSidesMatchedWith;
  w: Extended;
  i, j: Integer;
  AnotherSize: Integer;
  Ptr1, Ptr2: PObject;
  ExPtr1: PExtended;
  Graph: TWeightedGraph;

begin
  Result.Value:=
      0.6* (BalancedDiff (FImageWidth,
   AnotherICLDescCol.FImageWidth, 1)+
           BalancedDiff (FImageHeight,
   AnotherICLDescCol.FImageHeight, 1));

   Result.Value:= Result.Value+  0.2* BalancedDiff (FBlackPercOnBaseLine,
   AnotherICLDescCol.FBlackPercOnBaseLine, 1);

   Result.Value:= Result.Value+ 0.1* (BalancedDiff (FCOMRelPos.x,
        AnotherICLDescCol.FCOMRelPos.x, 1)+
           BalancedDiff (FCOMRelPos.y,
        AnotherICLDescCol.FCOMRelPos.y, 1));
  Result.Value:= 100* Result.Value;

  AnotherSize:= AnotherICLDescCol.Size;
  SetLength (Graph, Size+ AnotherSize);
  for i:= 0 to Size+ AnotherSize- 1 do
    SetLength (Graph [i], Size+ AnotherSize);

  Ptr1:= GetPointerToFirst;
  for i:= 0 to Size- 1 do
  begin
    ExPtr1:= @Graph [i, 0];
    Inc (ExPtr1, AnotherSize);
    w:= TICLDescriptor (Ptr1^).FCompAreaRegardingImArea;
    if TICLDescriptor (Ptr1^).FLabel= Byte (Ord (iclHole)) then
      w:= 2* w;

    for j:= AnotherSize to AnotherSize+ Size- 1 do
    begin
      ExPtr1^:= w* MaxCost;
      Inc (ExPtr1);

    end;

    Inc (Ptr1);

  end;

  Ptr2:= AnotherICLDescCol.GetPointerToFirst;
  for i:= Size to AnotherSize+ Size- 1 do
  begin
    ExPtr1:= @Graph [i, 0];
    
    w:= TICLDescriptor (Ptr2^).FCompAreaRegardingImArea;
    if TICLDescriptor (Ptr2^).FLabel= Byte (Ord (iclHole)) then
      w:= 2* w;

    for j:= 0 to AnotherSize- 1 do
    begin
      ExPtr1^:= w* MaxCost;
      Inc (ExPtr1);
      
    end;

    Inc (Ptr2);

  end;

  for i:= Size to AnotherSize+ Size- 1 do
  begin
    ExPtr1:= @Graph [i, 0];
    Inc (ExPtr1, AnotherSize);
    
    for j:= AnotherSize to Size+ AnotherICLDescCol.Size- 1 do
    begin
      ExPtr1^:= 0;
      Inc (ExPtr1);

    end;
    
  end;

  Ptr1:= GetPointerToFirst;
  for i:= 0 to Size- 1 do
  begin
    ExPtr1:= @Graph [i, 0];
    w:= TICLDescriptor (Ptr1^).FCompAreaRegardingImArea;

    Ptr2:= AnotherICLDescCol.GetPointerToFirst;

    for j:= 0 to AnotherSize- 1 do
    begin
      if TICLDescriptor (Ptr1^).IsMatchable ( TICLDescriptor (Ptr2^)) then
        ExPtr1^:= w*
          (TICLDescriptor (Ptr1^).FindDistance (TICLDescriptor (Ptr2^), WeightArray))
      else
        ExPtr1^:= Inf;//??!!

      Inc (ExPtr1);
      Inc (Ptr2);

    end;
    Inc (Ptr1);

  end;

(*$ifdef Debug_Matching_Details*)
  WriteLn ('Self:');
  WriteLn (Self.ToString);
  WriteLn ('Another:');
  WriteLn (AnotherICLDescCol.ToString);

  for i:= 0 to High (Graph) do
  begin
    for j:= 0 to High (Graph [i]) do
      Write (Graph [i, j]:0:4, ' ');
    WriteLn;

  end;
  WriteLn;
(*$endif*)
  MatchingInfo:= FindMinWeightedMatching (Graph);
(*$ifdef Debug_Matching_Details*)
  for i:= 0 to High (Graph) do
    Write (i, ':', MatchingInfo [i], ' ');

  WriteLn;
  Write (Result.Value:0:4, ' ');

(*$endif*)

  SetLength (Result.Mapped, Size+ AnotherSize);
  SetLength (Result.ScoreForMap, Size+ AnotherSize);
  Ptr1:= GetPointerToFirst;

  for i:= 0 to Size- 1 do
  begin
    j:= MatchingInfo [i];
    Result.Mapped [i]:= j;
    w:= TICLDescriptor (Ptr1^).FCompAreaRegardingImArea;
    if TICLDescriptor (Ptr1^).FLabel= Byte (Ord (iclHole)) then
      w:= 2* w;

    if j< AnotherSize then
    begin
      if TICLDescriptor (Ptr1^).IsMatchable (AnotherICLDescCol.ICLDescriptor [j]) then
      begin
        Result.Value:= Result.Value+
          w*
          TICLDescriptor (Ptr1^).FindDistance (AnotherICLDescCol.ICLDescriptor [j], WeightArray);

        Result.ScoreForMap [i]:= w*
          TICLDescriptor (Ptr1^).FindDistance (AnotherICLDescCol.ICLDescriptor [j], WeightArray);

      end
      else
      begin
        Result.Value:= Result.Value+ Inf;
        Result.ScoreForMap [i]:= Inf;

      end;

    end
    else
    begin
      Result.Value:= Result.Value+
        w* (MaxCost+ 1);
      Result.ScoreForMap [i]:= w* (MaxCost+ 1);
        
    end;
    
(*$ifdef Debug_Matching_Details*)
    Write (i, ':', Result.Value:0:4, ' ');
(*$endif*)

    Inc (Ptr1);

  end;

(*$ifdef Debug_Matching_Details*)
    Writeln;
    Writeln;
(*$endif*)
  for i:= Size to AnotherSize+ Size- 1 do
  begin
    j:= MatchingInfo [i];
    Result.Mapped [i]:= j;

    if j< AnotherSize then
    begin
      w:= AnotherICLDescCol.ICLDescriptor [j].FCompAreaRegardingImArea;
      if AnotherICLDescCol.ICLDescriptor [j].FLabel= Byte (Ord (iclHole)) then
        w:= 2* w;

      Result.Value:= Result.Value+
        w* (MaxCost+ 1);
      Result.ScoreForMap [i]:= w* (MaxCost+ 1);
        
    end
   else
     //Result:= Result+ 0;
      Result.ScoreForMap [i]:= 0

  end;

  SetLength (MatchingInfo, 0);
  for i:= 0 to High (Graph) do
    SetLength (Graph [i], 0);

  SetLength (Graph, 0);

end;

function TICLDescriptorCollection.FindDistance (
  AnotherICLDescCol: TICLDescriptorCollection;
        WeightArray: TWeightArray): Extended;
  
  function IsPow2 (n: Integer): Boolean;
  var
    i, j: Integer;

  begin

    Result:= False;
    j:= 1;
    
    for i:= 1 to 31 do
    begin
      if n and j<> 0 then
      begin
        if Result then
        begin
          Result:= False;
          Exit;
          
        end;
        Result:= True;
        
      end;

      j:= j shl 1;
      
    end;

    Result:= True;

  end;
  
type
  TWeightedGraph= array of array of Extended;
  TLeftSidesMatchedWith= array of Integer;
   
  TMatchingDetails= record
    LeftSidesMatch: TLeftSidesMatchedWith;
    MatchingSize: Integer;

  end;

const
  MaxCost= 1.35;//0.65+ 2* 0.35;

{
 Based on http://www.public.iastate.edu/~ddoty/HungarianAlgorithm.html
}
  function FindMinWeightedMatching (Graph: TWeightedGraph): TLeftSidesMatchedWith;
  var
    RowCover, ColCover: array of Boolean;
    XSize, YSize: Integer;
    MinWeight: Extended;
    Mask: array of array of Byte;

    procedure Step1;
    var
      i, j: Integer;
      ExPtr: PExtended;

    begin
      for i:= 0 to XSize- 1 do
      begin
        MinWeight:= 1e100;
        ExPtr:= @Graph [i, 0];
        
        for j:= 0 to YSize- 1 do
        begin
          if ExPtr^< MinWeight then
            MinWeight:= ExPtr^;
          Inc (ExPtr);
          
        end;

        ExPtr:= @Graph [i, 0];
        for j:= 0 to YSize- 1 do
        begin
          ExPtr^:= Abs (ExPtr^- MinWeight);// To avoid negative weights due !!!
          Inc (ExPtr);

        end;

      end;

    end;

    procedure Step2;
    var
      r, c: Integer;
      ExPtr: PExtended;

    begin
      
      for r:= 0 to XSize- 1 do
        if not RowCover [r] then
        begin
          ExPtr:= @Graph [r, 0];
          for c:= 0 to YSize- 1 do
          begin
            if (ExPtr^< 1e-10) and (not ColCover [c]) then
            begin
              Mask [r, c]:= 1;
              RowCover [r]:= True;
              ColCover [c]:= True;
              Break;

            end;

            Inc (ExPtr);

          end;

        end;

      FillChar (RowCover [0], XSize* SizeOf (Boolean), 0);
      FillChar (ColCover [0], YSize* SizeOf (Boolean), 0);

    end;

    function Step3: Integer;
    var
      i, j: Integer;
      BPtr: PByte;
      
    begin
      Result:= 0;
      for i:= 0 to XSize- 1 do
      begin
        BPtr:= @Mask [i, 0];

        for j:= 0 to YSize- 1 do
        begin
          if BPtr^= 1 then
          begin
            ColCover [j]:= True;
            Break;
            
          end;
          Inc (BPtr);
          
        end;

      end;

      for j:= 0 to YSize- 1 do
        if ColCover [j] then
          Inc (Result);

    end;

    type
      TPoint= record
        r, c: Integer;
      end;

    var
      ZeroPoint: TPoint;

    function Step4: Boolean;

      function FindAZero: TPoint;
      var
        r, c: Integer;
        ExPtr: PExtended;

      begin
        Result.r:= -1;
        for r:= 0 to XSize- 1 do
        begin
          ExPtr:= @Graph [r, 0];
          Inc (ExPtr, YSize- 1);

          if not RowCover [r] then
            for c:= YSize- 1 downto 0 do
            begin
              if (not ColCover [c]) and (ExPtr^< 1e-10) then
              begin
                Result.r:= r;
                Result.c:= c;
                Exit;

              end;

              Dec (ExPtr);

            end;

        end;


      end;

      function HaveStarInRow (Row: Integer): Boolean;
      var
       c: Integer;
       BPtr: PByte;

      begin
        Result:= True;

        BPtr:= @Mask [Row, 0];

        for c:= 1 to YSize do
        begin
          if BPtr^= 1 then
            Exit;

          Inc (BPtr);

        end;

        Result:= False;

      end;

      function FindStarInRow (ARow: Integer): Integer;
      var
        BPtr: PByte;

      begin
        BPtr:= @Mask [ARow, 0];

        for Result:= 0 to YSize- 1 do
        begin
          if BPtr^= 1 then
            Exit;

          Inc (BPtr);

        end;

        Result:= -1;

      end;

    var
      c: Integer;
      ZeroEntry: TPoint;

    begin

      Result:= False;

      while True do
      begin
        ZeroEntry:= FindAZero;

        if ZeroEntry.r< 0 then
        begin
          Result:= True;
          Exit;
          
        end;

        Mask [ZeroEntry.r, ZeroEntry.c]:= 2;

        c:= FindStarInRow (ZeroEntry.r);
        if c< 0 then
        begin
          ZeroPoint:= ZeroEntry;
          Break;

        end
        else
        begin
          RowCover [ZeroEntry.r]:= True;	//Cover the star's row.
          ColCover [c]:= False;

        end;

      end;

    end;

    procedure Step5;

      function FindStarInCol (Column: Integer): Integer;
      begin
        for Result:= 0 to XSize- 1 do
          if Mask [Result, Column]= 1 then
            Exit;

        Result:= -1;

      end;

      function FindPrimeInRow (ARow: Integer): Integer;
      var
        BPtr: PByte;

      begin
        BPtr:= @Mask [ARow, 0];

        for Result:= 0 to XSize- 1 do
        begin
          if BPtr^= 2 then
            Exit;
            
          Inc (BPtr);

        end;

        Result:= -1;

      end;


    procedure ErasePrimes;
    var
      r, c: Integer;
      BPtr: PByte;

    begin
      for r:= 0 to XSize- 1 do
      begin
        BPtr:= @Mask [r, 0];
        for c:= 1 to YSize do
        begin
          if BPtr^= 2 then
            BPtr^:= 0;
          Inc (BPtr);
          
        end;

      end;

    end;

    var
      Count: Integer;
      Path: array of TPoint;

    procedure ConvertPath;
    var
      i: Integer;
    begin
      for i:= 0 to Count- 1 do
        if Mask [Path [i].r ,Path [i].c]= 1 then
          Mask [Path [i].r ,Path [i].c]:= 0
        else
          Mask [Path [i].r ,Path [i].c]:= 1;

    end;

    var
      ActivePoint: TPoint;
      r, c: Integer;

    begin
      SetLength (Path, XSize);
      Path [0]:= ZeroPoint;
      ActivePoint:= Path [0];
      Count:= 1;

      while True do
      begin
        r:= FindStarInCol (ActivePoint.c);
        if 0<= r then
        begin
          if Count= Length (Path) then
            SetLength (Path, Length (Path)+ XSize);

          Inc (Count);
          Path [Count- 1].r:= r;
          Path [Count- 1].c:= ActivePoint.c;
          ActivePoint:= Path [Count- 1];

        end
        else
          Break;

        c:= FindPrimeInRow (ActivePoint.r);
        Inc (Count);
        if Length (Path)< Count then
          SetLength (Path, Length (Path)+ XSize);

        Path [Count- 1].r:= ActivePoint.r;
        Path [Count- 1].c:= c;
        ActivePoint:= Path [Count- 1];

      end;

      ConvertPath;
      FillChar (RowCover [0], SizeOf (Boolean)* XSize, 0);
      FillChar (ColCover [0], SizeOf (Boolean)* YSize, 0);

      ErasePrimes;

    end;

    procedure Step6;
    var
      Smallest: Extended;
      ExPtr: PExtended;
      r, c: Integer;

    begin
      Smallest:= 1e100;

      for r:= 0 to XSize- 1 do
      begin
        if not RowCover [r] then
        begin
          ExPtr:= @Graph [r, 0];
          for c:= 0 to YSize- 1 do
          begin
            if not ColCover [c] then
              if ExPtr^< Smallest then
                Smallest:= ExPtr^;

            Inc (ExPtr);

          end;

        end;

      end;

      for r:= 0 to XSize- 1 do
        for c:= 0 to YSize- 1 do
        begin
          if RowCover [r] then
            Graph [r, c]:= Graph [r, c]+ Smallest;

          if not ColCover [c] then
            Graph [r, c]:= Graph [r, c]- Smallest;

        end;

    end;

  var
    r, c: Integer;

  begin
    XSize:= Length (Graph);
    YSize:= Length (Graph [0]);

    SetLength (RowCover, XSize);
    SetLength (ColCover, YSize);
    FillChar (RowCover [0], XSize* SizeOf (Boolean), 0);
    FillChar (ColCover [0], YSize* SizeOf (Boolean), 0);

    SetLength (Mask, XSize);

    for r:= 0 to XSize- 1 do
    begin
      SetLength (Mask [r], YSize);
      FillChar (Mask [r, 0], YSize* SizeOf (Byte), 0);

    end;

    Step1;
    Step2;

    while Step3<> YSize do
    begin
      if Step4 then
      begin
        
        repeat
          Step6
          
        until not Step4;

        Step5;
      end
      else
        Step5;

    end;

    SetLength (Result, XSize);
    FillChar (Result [0], XSize* SizeOf (Integer), 255);

    for r:= 0 to XSize- 1 do
    begin
      for c:= 0 to YSize- 1 do
        if Mask [r, c]= 1 then
        begin
          Result [r]:= c;
          Break;

        end;

    end;

    for r:= 0 to XSize- 1 do
      SetLength (Mask [r], 0);
    SetLength (Mask, 0);

    SetLength (RowCover, 0);
    SetLength (ColCover, 0);

  end;

const
  Inf= 1e3;
  
var
  MatchingInfo: TLeftSidesMatchedWith;
  w: Extended;
  i, j: Integer;
  AnotherSize: Integer;
  Ptr1, Ptr2: PObject;
  ExPtr1: PExtended;
  Graph: TWeightedGraph;

begin
  Result:=
      WeightArray [0]* BalancedDiff (FImageWidth,
   AnotherICLDescCol.FImageWidth, 1)+
          WeightArray [1]* BalancedDiff (FImageHeight,
   AnotherICLDescCol.FImageHeight, 1);
   
(*$ifdef Debug_Matching_Details*)
   WriteLn ('Result= ', (100* Result):0:4);
(*$endif*)

   Result:= Result+  WeightArray [2]* BalancedDiff (FBlackPercOnBaseLine,
   AnotherICLDescCol.FBlackPercOnBaseLine, 1);
(*$ifdef Debug_Matching_Details*)
   WriteLn ('Result= ', (100* Result):0:4);
(*$endif*)

   Result:= Result+ WeightArray [3]* BalancedDiff (FCOMRelPos.x,
        AnotherICLDescCol.FCOMRelPos.x, 1)+
           WeightArray [4]* BalancedDiff (FCOMRelPos.y,
        AnotherICLDescCol.FCOMRelPos.y, 1);
(*$ifdef Debug_Matching_Details*)
   WriteLn ('Result= ', (100* Result):0:4);
(*$endif*)

  Result:= 100* Result;

  AnotherSize:= AnotherICLDescCol.Size;
  SetLength (Graph, Size+ AnotherSize);
  for i:= 0 to Size+ AnotherSize- 1 do
    SetLength (Graph [i], Size+ AnotherSize);

  Ptr1:= GetPointerToFirst;
  for i:= 0 to Size- 1 do
  begin
    ExPtr1:= @Graph [i, 0];
    Inc (ExPtr1, AnotherSize);
    w:= TICLDescriptor (Ptr1^).FCompAreaRegardingImArea;
    if TICLDescriptor (Ptr1^).FLabel= Byte (Ord (iclHole)) then
      w:= WeightArray [5]* w;

    for j:= AnotherSize to AnotherSize+ Size- 1 do
    begin
      ExPtr1^:= w* MaxCost;
      Inc (ExPtr1);

    end;

    Inc (Ptr1);
    
  end;

  Ptr2:= AnotherICLDescCol.GetPointerToFirst;
  for i:= Size to AnotherSize+ Size- 1 do
  begin
    ExPtr1:= @Graph [i, 0];
    w:= TICLDescriptor (Ptr2^).FCompAreaRegardingImArea;
    if TICLDescriptor (Ptr2^).FLabel= Byte (Ord (iclHole)) then
      w:= WeightArray [5]* w;

    for j:= 0 to AnotherSize- 1 do
    begin
      ExPtr1^:= w* MaxCost;
      Inc (ExPtr1);

    end;

    Inc (Ptr2);

  end;

  for i:= Size to AnotherSize+ Size- 1 do
  begin
    ExPtr1:= @Graph [i, 0];
    Inc (ExPtr1, AnotherSize);
    
    for j:= AnotherSize to Size+ AnotherICLDescCol.Size- 1 do
    begin
      ExPtr1^:= 0;
      Inc (ExPtr1);

    end;
    
  end;

  Ptr1:= GetPointerToFirst;
  for i:= 0 to Size- 1 do
  begin
    ExPtr1:= @Graph [i, 0];
    w:= TICLDescriptor (Ptr1^).FCompAreaRegardingImArea;

    Ptr2:= AnotherICLDescCol.GetPointerToFirst;

    for j:= 0 to AnotherSize- 1 do
    begin
      if TICLDescriptor (Ptr1^).IsMatchable (TICLDescriptor (Ptr2^)) then
        ExPtr1^:= w*
          (TICLDescriptor (Ptr1^).FindDistance (TICLDescriptor (Ptr2^), WeightArray))
      else
        ExPtr1^:= Inf;//??!!

      Inc (ExPtr1);
      Inc (Ptr2);

    end;
    Inc (Ptr1);

  end;

(*$ifdef Debug_Matching_Details*)
  WriteLn ('Self:');
  WriteLn (Self.ToString);
  WriteLn ('Another:');
  WriteLn (AnotherICLDescCol.ToString);

  for i:= 0 to High (Graph) do
  begin
    for j:= 0 to High (Graph [i]) do
      Write (Graph [i, j]:2:4, ' ');
    WriteLn;

  end;
  WriteLn;
(*$endif*)

  MatchingInfo:= FindMinWeightedMatching (Graph);
(*$ifdef Debug_Matching_Details*)
  for i:= 0 to High (Graph) do
    WriteLn (i, ':', MatchingInfo [i]);

  Write (Result:0:4, ' ');
  WriteLn;

(*$endif*)
  
  Ptr1:= GetPointerToFirst;
  for i:= 0 to Size- 1 do
  begin
    j:= MatchingInfo [i];
    w:= TICLDescriptor (Ptr1^).FCompAreaRegardingImArea;
    
    if TICLDescriptor (Ptr1^).FLabel= Byte (Ord (iclHole)) then
      w:= WeightArray [5]* w;

    if j< AnotherSize then
    begin
      if TICLDescriptor (Ptr1^).IsMatchable (AnotherICLDescCol.ICLDescriptor [j]) then
        Result:= Result+
          w*
          TICLDescriptor (Ptr1^).FindDistance (AnotherICLDescCol.ICLDescriptor [j], WeightArray)
      else
        Result:= Result+ Inf;

    end
    else
      Result:= Result+
        w* (MaxCost+ 1);
        
(*$ifdef Debug_Matching_Details*)
    Write (i, ':', Result:0:4, ' ');
(*$endif*)

    Inc (Ptr1);

  end;

(*$ifdef Debug_Matching_Details*)
    Writeln;
    Writeln;
(*$endif*)
  for i:= Size to AnotherSize+ Size- 1 do
  begin
    j:= MatchingInfo [i];

    if j< AnotherSize then
    begin
      w:= AnotherICLDescCol.ICLDescriptor [j].FCompAreaRegardingImArea;
      if AnotherICLDescCol.ICLDescriptor [j].FLabel= Byte (Ord (iclHole)) then
        w:= w* WeightArray [5];

      Result:= Result+
        w* (MaxCost+ 1);
        
    end;
    {else
      Result:= Result+ 0;}
(*$ifdef Debug_Matching_Details*)
    Write (i, ':', Result:0:4, ' ');
(*$endif*)

  end;

  SetLength (MatchingInfo, 0);
  for i:= 0 to High (Graph) do
    SetLength (Graph [i], 0);

  SetLength (Graph, 0);

end;

destructor TICLDescriptorCollection.Destroy;
begin
  if FCOMRelPos<> nil then
    FCOMRelPos.Free;
    
  inherited;
  
end;

function TICLDescriptorCollection.GetICLDescriptor(
  Index: Integer): TICLDescriptor;
begin
  Result:= Member [Index] as TICLDescriptor;
  
end;

function TICLDescriptorCollection.ToString: String;
var
  Ptr: PObject;
  i: Integer;

begin
  Result:= 'Image COM:'+ COMRelPos.ToString+ #13#10+
       'ImageWidth:'+ FloatToStr (FImageWidth)+ #13#10+
       'ImageHeigth:'+ FloatToStr (FImageHeight)
       + #13#10
       + #13#10;

  Ptr:= GetPointerToFirst;
  for i:= 0 to Size- 1 do
  begin
    Result:= Result+ IntToStr (i+ 1)+ ':'+ TICLDescriptor (Ptr^).ToString+ #13#10;
    Inc (Ptr);
    
  end;

end;

procedure TICLDescriptorCollection.SaveToFile (var OutputFile: TextFile);
var
  i: Integer;
  Ptr: PObject;
  
begin
  WriteLn (OutputFile, 'FImageWidth= ', FImageWidth);
  WriteLn (OutputFile, 'FImageHeight= ', FImageHeight);
  WriteLn (OutputFile, 'FBlackPercOnBaseLine= ', FBlackPercOnBaseLine);
  WriteLn (OutputFile, 'FCOMRelPos= ', FCOMRelPos.ToString);
  WriteLn (OutputFile, 'Size= ', Size);

  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    WriteLn (OutputFile, i, ':');
    TICLDescriptor (Ptr^).SaveToFile (OutputFile);
    Inc (Ptr);
    
  end;

  WriteLn (OutputFile);

end;

procedure TICLDescriptorCollection.LoadFromFile (var InputFile: TextFile);
var
  S: String;
  i: Integer;
  NewIclDescriptor: TICLDescriptor;
  
begin
  ReadLn (InputFile, FImageWidth);
  ReadLn (InputFile, FImageHeight);
  ReadLn (InputFile, FBlackPercOnBaseLine);
  ReadLn (InputFile, S);

  if FCOMRelPos<> nil then
    FCOMRelPos.Free;
  FCOMRelPos:= TRelPosInfo.Create (S);

  ReadLn (InputFile, i);

  while 0< i do
  begin
    NewIclDescriptor:= TICLDescriptor.Create;
    NewIclDescriptor.LoadFromFile (InputFile);
    Self.AddICLDescriptor (NewIclDescriptor);
    Dec (i);
    
  end;

  ReadLn (InputFile);

end;

procedure TICLDescriptorCollection.FstSaveToFile(var OutputFile: TextFile;
                Selected:array of Boolean; CountOfSubWordParams,
                MissingValue: Integer; ConstInc: Double;
                SubWordCoef, RegionCoef: array of Integer);
const
  NoUsefulLabels= [5, 7, 10, 11, 13, 14, 15, 16];
  OccurOnce= [3, 6, 9, 12];
  
var
  i, j, k, h, found: Integer;
  Tags: array of Integer;
  RegionFeatureLength :array [0..4] of Integer;
  
begin


  RegionFeatureLength [0] := 1;
  RegionFeatureLength [1] := 1;
  RegionFeatureLength [2] := 2;
  RegionFeatureLength [3] := 2;
  RegionFeatureLength [4] := 2;
  if Selected[0] then
  begin
    Writeln (OutputFile,(SubWordCoef[0]* FRelImageHeight)+ ConstInc :0:3);
//    Writeln (OutputFile, FRelImageWidth:0:3);
  end;
  if Selected[1] then
    Writeln (OutputFile, (SubWordCoef[1]* FImageBlackPixDensity / 100)
          + ConstInc:0:3);
  if Selected[2] then
  begin
    Writeln (OutputFile, (SubWordCoef[2]* FCOMRelPos.x)+ ConstInc:0:3);
    Writeln (OutputFile, (SubWordCoef[2]* FCOMRelPos.y )+ ConstInc:0:3);
  end;
  if Selected[3] then
    Writeln (OutputFile, (SubWordCoef[3]* FBlackPercUpperBaseLine)+ ConstInc:0:3);
  if Selected[4] then
    Writeln (OutputFile, (SubWordCoef[4]* FBlackPercOnBaseLine)+ ConstInc:0:3);
  if Selected[5] then
    Writeln (OutputFile, (SubWordCoef[5]* FBlackPercBelowBaseLine)+ ConstInc:0:3);
  SetLength (Tags, Size);
  for i:= 0 to Size- 1 do
    Tags [i]:= i;

  for i:= 0 to Size- 1 do
    for j:= i+ 1 to Size- 1 do
      if ICLDescriptor [Tags [i]].FCompAreaRegardingImArea<
        ICLDescriptor [Tags [j]].FCompAreaRegardingImArea then
      begin
        Tags [i]:= Tags [i] xor Tags [j];
        Tags [j]:= Tags [i] xor Tags [j];
        Tags [i]:= Tags [i] xor Tags [j];

      end;

  for i:= 0 to 17 do
  begin
    if CONST_USE_REGION_LABELS_FOR_DEBUG = True then
      Writeln(OutputFile,'region : ',i);
    found:= 0;

    if i in NoUsefulLabels then
      Continue;
    for j := 0 to  size -1 do
      if ICLDescriptor[Tags[j]].FLabel=i then
      begin
        inc (found);
        ICLDescriptor[Tags[j]].FstSaveToFile(OutputFile,Selected,
          CountOfSubWordParams, ConstInc, RegionCoef);
        if (found=2) then
          break;
      end;
      
    if i in OccurOnce then
    begin
      for j := found+ 1 to 1 do
        for k := CountOfSubWordParams to Length(Selected)  do
          if Selected [k] then
            for h := 1 to RegionFeatureLength[k- CountOfSubWordParams] do
              Writeln(OutputFile, MissingValue);
    end
    else
      for j := found+ 1 to 2 do
        for k := CountOfSubWordParams to Length(Selected)  do
          if Selected [k] then
            for h := 1 to RegionFeatureLength [k- CountOfSubWordParams] do
              Writeln(OutputFile, MissingValue);
              
  end;
//  Writeln(OutputFile);

end;


procedure TICLDescriptorCollection.SaveToStream(
  OutputStream: TMyFileStream);
var
  i: Integer;
  Ptr: PObject;
  
begin

  OutputStream.WriteLine (IntToStr (FImageWidth)+ ' ');
  OutputStream.WriteLine (IntToStr (FImageHeight)+ ' ');
  OutputStream.WriteLine (FloatToStr (FBlackPercOnBaseLine)+ ' ');
  OutputStream.WriteLine (FCOMRelPos.ToString+ ' ');
  OutputStream.WriteLine (IntToStr (Size));

  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    TICLDescriptor (Ptr^).SaveToStream (OutputStream);
    Inc (Ptr);

  end;

end;

procedure TICLDescriptorCollection.LoadFromStream (
  InputStream: TMyFileStream);
var
  i: Integer;
  Ptr: PObject;
  NewICLDescriptor: TICLDescriptor;
  
begin
  raise ENotImplementedYet.Create ('TICLDescriptorCollection.LoadFromStream');
  
  FImageWidth:= StrToInt (InputStream.ReadLine);
  FImageHeight:= StrToInt (InputStream.ReadLine);
  FBlackPercOnBaseLine:= StrToFloat (InputStream.ReadLine);
  FCOMRelPos:= TRelPosInfo.Create (InputStream.ReadLine);
  Allocate (StrToInt (InputStream.ReadLine));

  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    NewICLDescriptor:= TICLDescriptor.Create;
    Inc (Ptr);

  end;

end;

{ TICLClassDefinition }

procedure TICLClassDefinition.AddICLDescriptorCollection(
  NewICLDescriptorCollection: TICLDescriptorCollection);
begin
  inherited Add (NewICLDescriptorCollection);

end;

constructor TICLClassDefinition.Create;
begin
  inherited;
  
end;

constructor TICLClassDefinition.Create(ClassName: String;
    WeightArray: array of Extended);
var
  i: Integer;

begin
  inherited Create;

  FClassName:= ClassName;
  if Length (WeightArray)<> WeightArrayLen then
    raise EInvalidArgument.Create ('Weight array size is invalid!s');

  SetLength (FWeightArray, WeightArrayLen);
  for i:= 0 to WeightArrayLen- 1 do
    FWeightArray [i]:= WeightArray [i];

end;

function TICLClassDefinition.Evaluate (
  Query: TICLDescriptorCollection;
      WeightArray: TWeightArray): Extended;
var
  i: Integer;
  Ptr: PObject;
  Temp: Extended;

begin
  if WeightArray= nil then
    WeightArray:= FWeightArray;

  Result:= 1e100;
  Ptr:= GetPointerToFirst;

  for i:= 1 to Size do
  begin
    Temp:= TICLDescriptorCollection (Ptr^).FindDistance (Query, WeightArray);
    if Temp< Result then
      Result:= Temp;

    Inc (Ptr);

  end;
  
end;

function TICLClassDefinition.GetICLDescriptorCollection(
  Index: Integer): TICLDescriptorCollection;
begin
  Result:= Member [Index] as TICLDescriptorCollection;
  
end;

procedure TICLClassDefinition.LoadFromFile (FileName: String);
begin
  raise ENotImplemented.Create ('TICLClassDefinition.LoadFromFile');
  
end;

procedure TICLClassDefinition.SaveToFile (FileName: String);
begin
  raise ENotImplemented.Create ('TICLClassDefinition.SaveToFile');

end;

procedure TICLClassDefinition.LoadFromFile (var InputFile: TextFile);
var
  i: Integer;
  ICLDescriptor: TICLDescriptorCollection;
  Temp: String;

begin
  ReadLn (InputFile, Temp);
  FClassName:= Temp;
  Readln (InputFile, i);

  while 0< i do
  begin
    ICLDescriptor:= TICLDescriptorCollection.Create;
    ICLDescriptor.LoadFromFile (InputFile);
    AddICLDescriptorCollection (ICLDescriptor);
    Dec (i);
    
  end;

end;

procedure TICLClassDefinition.SaveToFile (var OutputFile: TextFile);
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  
  WriteLn (OutputFile, FClassName);
  WriteLn (OutputFile, Size);
  for i:= 1 to Size do
  begin
    TICLDescriptorCollection (Ptr^).SaveToFile (OutputFile);
    Inc (Ptr);

  end;

end;

{ TICLClassDefinitionCollection }

procedure TICLClassDefinitionCollection.AddClassDefinition (
  NewICLClassDefinition: TICLClassDefinition);
begin
  inherited Add (NewICLClassDefinition);

end;

constructor TICLClassDefinitionCollection.Create;
begin
  inherited;

end;

function TICLClassDefinitionCollection.FindClass (
  AQuery: TICLDescriptorCollection;
      WeightArray: TWeightArray): Integer;
var
  Min, Temp: Extended;
  i: Integer;
  Ptr: PObject;

begin
  if Size= 0 then
  begin
    Result:= -1;
    Exit;

  end;

  Ptr:= GetPointerToFirst;
  Min:= TICLClassDefinition (Ptr^).Evaluate (AQuery, WeightArray);
  Result:= 0;
  Inc (Ptr);

  for i:= 2 to Size do
  begin
    Temp:= TICLClassDefinition (Ptr^).Evaluate (AQuery, WeightArray);
    if Temp< Min then
    begin
      Min:= Temp;
      Result:= i- 1;
      
    end;

    Inc (Ptr);
    
  end;

end;

function TICLClassDefinitionCollection.GetICLClassDefiniont (
  Index: Integer): TICLClassDefinition;
begin
  Result:= Member [Index] as TICLClassDefinition;
  
end;

procedure TICLClassDefinitionCollection.LoadFromFile (FileName: String);
var
  i: Integer;
  InputFile: TextFile;
  NewClass: TICLClassDefinition;

begin
  AssignFile (InputFile, FileName);
  Reset (InputFile);

  ReadLn (inputFile, i);
  while 0< i do
  begin
    NewClass:= TICLClassDefinition.Create;
    NewClass.LoadFromFile (InputFile);
    AddClassDefinition (NewClass);
    Dec (i);

  end;

  if not Eof (InputFile) then
    raise Exception.Create ('Invalid File!');

  CloseFile (InputFile);

end;

procedure TICLClassDefinitionCollection.SaveToFile (FileName: String);
var
  i: Integer;
  Ptr: PObject;
  OutputFile: TextFile;

begin
  AssignFile (OutputFile, FileName);
  Rewrite (OutputFile);

  WriteLn (OutputFile, Size);
  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    TICLClassDefinition (Ptr^).SaveToFile (OutputFile);
    Inc (Ptr);
    
  end;
  
  CloseFile (OutputFile);

end;

{ TSubwordInfo }

constructor TSubwordInfo.Create;
begin
  inherited;

  FBaseLineHeightCoef:= 10;
  FBaseLineWidthCoef:= 10;


end;

destructor TSubwordInfo.Destroy;
begin

  inherited;
  
end;

procedure TSubwordInfo.LoadFromFile (Filename: String);
var
  InputFile: TextFile;

begin
  AssignFile (InputFile, Filename);
  Reset (InputFile);
  
  ReadLn (InputFile, FBaseLineHeight);
  ReadLn (InputFile, FThickness);
  
  CloseFile (InputFile);
  FBaseLineHeight:= 0;
  FThickness:= 0;
  
end;

{ TRelPosInfo }

constructor TRelPosInfo.Create;
begin
  inherited;
end;

constructor TRelPosInfo.Create(Str: String);
var
  XStr, YStr: String;

begin
  inherited Create;

  XStr:= System.Copy (Str, 0, Pos (':', Str)- 1);
  Delete (Str, 1, Pos (':', Str));
  YStr:= Str;

  x:= StrToInt (XStr);
  y:= StrToInt (YStr);
end;

function TRelPosInfo.ToString: string;
begin
  Result := FloatToStr (x) + ':' + FloatToStr (y);
end;

end.
