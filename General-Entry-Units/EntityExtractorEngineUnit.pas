unit EntityExtractorEngineUnit;

interface
uses
  UnitGeneralTypes, Graphics, TemplateUnit,
  FMLImage, GeometryUnit, SysUtils, SvmClientUnit,
  CollectionUnit;

type
  ECanNotEstimateSkewAndReplacement= class (Exception)
  public
    constructor Create;

  end;

  TSVMCapInfo= class (TObject)
  private
    FHandle: TSVMHandle;
    FCaption: WideString;
    
  public
    property Handle: TSVMHandle read FHandle;
    property Caption: WideString read FCaption;
      
    constructor Create (Handle: TSVMHandle; Cap: WideString);

  end;

  TSVMCapInfoCollection= class (TBaseCollection)
  private
    function GetSVMCapInfo (Index: Integer): TSVMCapInfo;
    
  public
    property SVMCapInfo [Index: Integer]: TSVMCapInfo read GetSVMCapInfo;

    procedure Add (NewSVMCapInfo: TSVMCapInfo);
    
    procedure Free;

  end;
  
  TEntityExtractorEngine= class (TObject)
  private
    MaxImageProperties,
    MinImageProperties,
    PointsInImage: TPoint;
    ServerHost: String;
    ServerPort: Integer;
    FSVMClient: TSVMClient;
    SVMCapInfoCol: TSVMCapInfoCollection;

    function FindSkewAndReplacementInfo (Image: TFMLImage;
       LocationCheckLabels: TLocationCheckCollection;
         const Threshold: Extended= 2/ 3): TSkewReplacementInfo;

  public
    constructor Create;
    procedure Free;
    procedure LoadSVMs (SVMInfoCollection: TSVMInfoCollection);

    function Process (TemplateInfo: TFormInfo;
          Bitmap: TBitmap): TFormInfo;

  end;

implementation

uses
  Classes, ExceptionUnit, Math, ComponentsUnit;

{ TEntityExtractorEngine }

constructor TEntityExtractorEngine.Create;
var
  InputFile: TextFile;
  S: String;

begin
  inherited;

  AssignFile (InputFile, 'EngineConfig.txt');
  Reset (InputFile);
  ReadLn (InputFile, S);
  ReadLn (InputFile, S);
  MinImageProperties:= TPoint.Create (S);
  ReadLn (InputFile, S);
  ReadLn (InputFile, S);
  MaxImageProperties:= TPoint.Create (S);
  ReadLn (InputFile, S);
  ReadLn (InputFile, S);
  PointsInImage:= TPoint.Create (S);

  ReadLn (InputFile, S);
  ReadLn (InputFile, ServerHost);
  ReadLn (InputFile, S);
  ReadLn (InputFile, ServerPort);
  CloseFile (InputFile);

  FSVMClient:= TSVMClient.Create (ServerHost, ServerPort);

end;

function TEntityExtractorEngine.FindSkewAndReplacementInfo(
  Image: TFMLImage; LocationCheckLabels: TLocationCheckCollection;
  const Threshold: Extended): TSkewReplacementInfo;
    
const
  AddC: array [1..8] of Integer=     (-1,  0, +1, +1, +1,  0, -1, -1);
  AddR: array [1..8] of Integer=     (-1, -1, -1,  0, +1, +1, +1,  0);
  TopRightScoreInc:
  array [1..8] of Integer=           ( 0, +1, +2, +1,  0, -1, -2, -1);
  BotRightScoreInc:
  array [1..8] of Integer=           (-2, -1,  0, +1, +2, +1,  0, -1);
  TopLeftScoreInc:
  array [1..8] of Integer=           ( 2, +1,  0, -1, -2, -1,  0, +1);

  function FindHorizontalLineIn (r, c: Integer): TLineInfo;
  var
    i: Integer;
    TopLeftSum, TopRightSum: Integer;
    Newr1, Newc1, Newr2, Newc2: Integer;
    ActiveSum,
    State: Integer;
    
  begin
    State:= 4;// to be set to UP when added by 5 in mod 8 .

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

  function FindHorizontalBoxIn (r, c: Integer): TBoxInfo;
  var
    i: Integer;
    TopLeftSum, BotRightSum,
    TopRightSum: Integer;
    Newr1, Newc1, Newr2, Newc2: Integer;
    ActiveSum,
    State: Integer;
    
  begin
    State:= 4;// to be set to UP when added by 5 in mod 8 .

    Result.rTopLeft:= r; Result.rBotRight:= r;
    Result.cTopLeft:= c; Result.cBotRight:= c;
    Result.cTopRight:= c; Result.cTopRight:= c;

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
    

{The following codes tries to find the Bot Right point of the black line on which the pixel r, c is placed }
{We define an score measure for each point on the line which is formulated as follows:
  c+ r}
{
  ActiveSum is used to avoid recalculating the formulas
}
      
    Newr1:= r; Newc1:= c;
    BotRightSum:= c+ r;

    ActiveSum:= BotRightSum;// Sum of Newr1 and Newc1
    while Image.Body [Newr1, Newc1]= BLACK do
    begin
      State:= (State+ 5) mod 8+ 1;

      for i:= 1 to 8 do
      begin
        Newr2:= Newr1+ AddR [State];
        Newc2:= Newc1+ AddC [State];

        if Image.Body [Newr2, Newc2]= BLACK then
        begin
          Newr1:= Newr2;
          Newc1:= Newc2;
          Inc (ActiveSum, BotRightScoreInc [State]);

          Break;

        end;

        State:= State mod 8+ 1;

      end;

      if BotRightSum< ActiveSum then
      begin
        BotRightSum:= ActiveSum;
        Result.rBotRight:= Newr1;
        Result.cBotRight:= Newc1;

      end;

      if ActiveSum+ 10{Thr1}< BotRightSum then//Should be revised
        Break;

    end;

  end;

var
  i: Integer;
  r, c: Integer;
  LineInfo: TLineInfo;
  BoxInfo: TBoxInfo;
  
begin
{
  This function tries to find a horizontal line whose length is at least equal to Threshold* ....
}
  
  for i:= 0 to LocationCheckLabels.Size- 1 do
    if LocationCheckLabels.ItemInfo [i].LocationType= elcLine then
    begin

      c:= (LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.x+
          LocationCheckLabels.ItemInfo [i].AnnouncedBottomRight.x) div 2;
          
      if LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.y* 2< Image.Row then
        for r:= 0 to Image.Row div 2 do
        begin
          if Image.Body [r, c]= BLACK then
          begin
            LineInfo:= FindHorizontalLineIn (r, c);
            
            if Threshold*
              (LocationCheckLabels.ItemInfo [i].AnnouncedBottomRight.x-
              LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.x)< LineInfo.Len then
            begin
              Result.Replacement:= TPoint.Create (LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.x-
                LineInfo.cTopLeft,
                       LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.y-
                        LineInfo.rTopLeft);
              Result.Skew:= TPoint.Create (LineInfo.cTopLeft- LineInfo.cTopLeft,
                            LineInfo.rTopLeft- LineInfo.rTopRight);
              Result.SkewSource:= TPoint.Create (LineInfo.cTopLeft,
                        LineInfo.rTopLeft);
              Exit;
               
            end;


          end;

        end
        
      else
        for r:= Image.Row downto Image.Row div 2 do
        begin
          if Image.Body [r, c]= BLACK then
          begin
            LineInfo:= FindHorizontalLineIn (r, c);
            if Threshold*
              (LocationCheckLabels.ItemInfo [i].AnnouncedBottomRight.x-
              LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.x)< LineInfo.Len then
            begin
              Result.Replacement:= TPoint.Create (LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.x-
                LineInfo.cTopLeft,
                       LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.y-
                        LineInfo.rTopLeft);
              Result.Skew:= TPoint.Create (LineInfo.cTopLeft- LineInfo.cTopRight,
                            LineInfo.rTopLeft- LineInfo.rTopRight);
              Exit;
               
            end;


          end;

        end;
    
    end
    else if LocationCheckLabels.ItemInfo [i].LocationType= elcRect then
    begin

      c:= (LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.x+
          LocationCheckLabels.ItemInfo [i].AnnouncedBottomRight.x) div 2;
          
      for r:= LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.y- 5{A Threshold} to
        LocationCheckLabels.ItemInfo [i].AnnouncedBottomRight.y do
      begin
        if Image.Body [r, c]= BLACK then
        begin
          BoxInfo:= FindHorizontalBoxIn (r, c);
            
          if (Threshold*
            (LocationCheckLabels.ItemInfo [i].AnnouncedBottomRight.x-
            LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.x)< BoxInfo.cBotRight- BoxInfo.cTopLeft)
            and (Threshold*
            (LocationCheckLabels.ItemInfo [i].AnnouncedBottomRight.y-
            LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.y)< BoxInfo.rBotRight- BoxInfo.rTopLeft) then
          begin
            Result.Replacement:= TPoint.Create (LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.x-
              BoxInfo.cTopLeft,
                     LocationCheckLabels.ItemInfo [i].AnnouncedTopLeft.y-
                      BoxInfo.rTopLeft);
            Result.Skew:= TPoint.Create (BoxInfo.cTopRight- BoxInfo.cTopLeft,
                          BoxInfo.rTopRight- BoxInfo.rTopLeft);
            Result.SkewSource:= TPoint.Create (BoxInfo.cTopLeft,
                      BoxInfo.rTopLeft);
            Exit;
               
          end;


        end;

      end;

    end;

  raise ECanNotEstimateSkewAndReplacement.Create;

end;

procedure TEntityExtractorEngine.Free;
begin

  inherited;

end;

function TEntityExtractorEngine.Process (TemplateInfo: TFormInfo;
  Bitmap: TBitmap): TFormInfo;

  procedure Classify (Image: TFMLImage; AnItemInfo: TItemInfo);
  begin
    case AnItemInfo.ItemType of
      itNumbers:
      ;
      
    end;

  end;

var
  Image: TFMLImage;

  function MergeIfNecessary (ComponentCollection: TComponentCollection;
        TargetCompCount: Integer): Boolean;
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
  
    while TargetCompCount< ComponentCollection.Count do
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

  procedure SortByX (var AComponentCollection: TComponentCollection);
  var
    i, j: Integer;
    CentersOfMass: array of TPoint;
    Tags: array of Integer;
    TempCollection: TComponentCollection;
    
  begin
    SetLength (CentersOfMass, AComponentCollection.Count);
    SetLength (Tags, AComponentCollection.Count);

    for i:= 0 to AComponentCollection.Count- 1 do
    begin
      Tags [i]:= i;
      CentersOfMass [i]:=
           AComponentCollection.Component [i].CenterOfMass.Copy.Scale
                    (1/ AComponentCollection.Component [i].Count);
    end;

    for i:= 0 to High (Tags) do
      for j:= i+ 1 to High (Tags) do
        if CentersOfMass [Tags [j]].x< CentersOfMass [Tags [i]].x then
        begin
          Tags [i]:= Tags [i] xor Tags [j];
          Tags [j]:= Tags [i] xor Tags [j];
          Tags [i]:= Tags [i] xor Tags [j];

        end;

    for i:= 0 to High (Tags) do
      CentersOfMass [i].Free;
    SetLength (CentersOfMass, 0);

    TempCollection:= TComponentCollection.Create;
    for i:= 0 to High (Tags) do
      TempCollection.AddComponent (AComponentCollection.Component [Tags [i]]);
    AComponentCollection.Free (False);
    AComponentCollection:= TempCollection;
    SetLength (Tags, 0);

  end;


  procedure ExtractImagesInFormInfo (AFieldSet: TFieldSet; AFormItemInfo: TFormItemInfo;
    SkewReplacementInfo: TSkewReplacementInfo);
  var
    ThisFormItemImage: TFMLImage;
    ThisFormItemImageCollection: TImageCollection;
    MinCompPoint, MaxCompPoint: TPoint;
    ResultedComponentCollection: TComponentCollection;
    i, j, k: Integer;
    TopLeftActiveRect, BotRightActiveRect: TPoint;
    ActiveRectWidth, ActiveRectHeight: Integer;
    ActiveSegComponentCollection: TComponentCollection;
    ActiveComponent: TComponent;
    Flag: Boolean;

  begin
    TopLeftActiveRect:= AFormItemInfo.AnnouncedTopLeft.Copy;
    BotRightActiveRect:= AFormItemInfo.AnnouncedBottomRight.Copy;

    TopLeftActiveRect.Move (SkewReplacementInfo.Replacement);
    BotRightActiveRect.Move (SkewReplacementInfo.Replacement);

    ActiveRectWidth:= BotRightActiveRect.x- TopLeftActiveRect.x;
    ActiveRectHeight:= BotRightActiveRect.y- TopLeftActiveRect.y;

    if 0< SkewReplacementInfo.Skew.y then
    begin
      TopLeftActiveRect.x:= TopLeftActiveRect.x- Round (ActiveRectHeight* SkewReplacementInfo.Skew.y/
      SkewReplacementInfo.Skew.x);//??!!
//   Skew.y  Skew.x
//  ActiveH    ?
      BotRightActiveRect.y:= BotRightActiveRect.y+ Round (ActiveRectWidth* SkewReplacementInfo.Skew.y/
      SkewReplacementInfo.Skew.x);
{      BotRightActiveRect.x:= TopLeftActiveRect.x+ Round (ActiveRectWidth* SkewReplacementInfo.Skew.y/
      SkewReplacementInfo.Skew.x);
}
    end
    else if SkewReplacementInfo.Skew.y< 0 then
    begin
      TopLeftActiveRect.y:= TopLeftActiveRect.y+ Round (ActiveRectWidth* SkewReplacementInfo.Skew.x/
      SkewReplacementInfo.Skew.y);

      BotRightActiveRect.x:= BotRightActiveRect.x+ Round (ActiveRectHeight* SkewReplacementInfo.Skew.y/
      SkewReplacementInfo.Skew.x);
{      BotRightActiveRect.y:= TopLeftActiveRect.y+ Round (ActiveRectWidth* SkewReplacementInfo.Skew.x/
      SkewReplacementInfo.Skew.y);
}
    end;
    
    ActiveSegComponentCollection:= Image.FindAllComponentsInBox (TopLeftActiveRect, BotRightActiveRect,
       False);

    ResultedComponentCollection:= TComponentCollection.Create;

    for i:= 0 to ActiveSegComponentCollection.Count- 1 do
    begin
      ActiveComponent:= ActiveSegComponentCollection.Component [i];

      if (ActiveComponent.Width< MaxImageProperties.x) and
         (MinImageProperties.x< ActiveComponent.Width) and
         (ActiveComponent.Height< MaxImageProperties.y) and
         (MinImageProperties.y< ActiveComponent.Height) and
         (ActiveComponent.Percentage< PointsInImage.y) and
         (PointsInImage.x< ActiveComponent.Percentage) then
      begin

        if AFormItemInfo.IsComponentMine (ActiveComponent,
                  TopLeftActiveRect) then
          ResultedComponentCollection.AddComponent (ActiveComponent)
        else
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

    end;
    ActiveSegComponentCollection.Free (False);

    MinCompPoint:= ResultedComponentCollection.MinPoint;
    MaxCompPoint:= ResultedComponentCollection.MaxPoint;

    AFormItemInfo.RealTopLeft.SetTo (TopLeftActiveRect);
    AFormItemInfo.RealTopLeft.Move (MinCompPoint);
    AFormItemInfo.RealBottomRight.SetTo (TopLeftActiveRect);
    AFormItemInfo.RealTopLeft.Move (MaxCompPoint);
    
    MinCompPoint.Free;
    MaxCompPoint.Free;

    case AFormItemInfo.ItemInfo.ItemType of
      itNumber:
      begin
        ThisFormItemImage:= TFMLImage.Create (ResultedComponentCollection);
        ThisFormItemImage.SaveAsText ('C:\1.txt');

        Classify (ThisFormItemImage, AFormItemInfo.ItemInfo);
        ThisFormItemImage.Free;
        
      end;
      itNumbers:
      begin
        MergeIfNecessary (ResultedComponentCollection,
           5);//AFormItemInfo.ItemInfo.Length);
        SortByX (ResultedComponentCollection);
        ThisFormItemImageCollection:= TImageCollection.Create;
        for i:= 0 to ResultedComponentCollection.Count- 1 do
          ThisFormItemImageCollection.AddImage (
             TFMLImage.Create (ResultedComponentCollection.Component [i]));

        for i:= 0 to ResultedComponentCollection.Count- 1 do
        begin                                                                     
          ThisFormItemImageCollection.Image [i].SaveAsText ('C:\'+ IntToStr (i)+ '.txt');
//          Classify (ThisFormItemImageCollection.Image [i], AFormItemInfo.ItemInfo [i]);
          
        end;

      end;

    end;

    ResultedComponentCollection.Free (True);

  end;

var
  ImagesInActiveSegment: TImageCollection;
  SkewReplacementInfo: TSkewReplacementInfo;
  ActiveFieldSet: TFieldSet;
  i, j, k: Integer;
  Flag: Boolean;

begin
  Result:= TemplateInfo.Copy;

  Image:= TFMLImage.Create;
  Image.LoadBitMap (Bitmap);
  try
    SkewReplacementInfo:= FindSkewAndReplacementInfo (Image,
      Result.LocationCheckCollection);
  except
    on e: ECanNotEstimateSkewAndReplacement do
    begin
      Result.ProcessedSuccessfully:= False;
      Exit;

    end;
  end;

  for i:= 0 to Result.FormItemCollection.Size- 1 do
  begin
    ActiveFieldSet:= Result.FormItemCollection.ItemInfo [i];

    for j:= 0 to ActiveFieldSet.Size- 1 do
      ExtractImagesInFormInfo (ActiveFieldSet, ActiveFieldSet.ItemInfo [j],
             SkewReplacementInfo);
             
  end;

  SkewReplacementInfo.Replacement.Free;
  SkewReplacementInfo.Skew.Free;
  Image.Free;


end;

{ ECanNotEstimateSkewOrReplacement }

constructor ECanNotEstimateSkewAndReplacement.Create;
begin
  inherited Create ('Can not estimate skew and Replacement!');
  
end;

procedure TEntityExtractorEngine.LoadSVMs(
  SVMInfoCollection: TSVMInfoCollection);
var
  SVMCapInfo: TSVMCapInfo;
  i, j: Integer;

begin

  for i:= 0 to SVMInfoCollection.Size- 1 do
  begin
    SVMCapInfo:= TSVMCapInfo.Create (
      FSVMClient.OpenSVM (SVMInfoCollection.SVMInfo [i].KernelName,
        SVMInfoCollection.SVMInfo [i].SVMName),
        SVMInfoCollection.SVMInfo [i].Caption);

    for j:= 0 to SVMCapInfoCol.Size- 1 do
      if SVMCapInfoCol.SVMCapInfo [j].Handle=
        SVMCapInfo.Handle then
      begin
        SVMCapInfo.Free;
        SVMCapInfo:= nil;
        Break;
        
      end;

    if SVMCapInfo<> nil then
      SVMCapInfoCol.Add (SVMCapInfo);

  end;


end;

{ TSVMInfo }

constructor TSVMCapInfo.Create (Handle: TSVMHandle; Cap: WideString);
begin
  inherited Create;
  
  FHandle:= Handle;
  FCaption:= Cap;
  
end;

{ TSVMCapInfoCollection }

procedure TSVMCapInfoCollection.Add(NewSVMCapInfo: TSVMCapInfo);
begin
  inherited Add (NewSVMCapInfo);
end;

procedure TSVMCapInfoCollection.Free;
var
  Ptr: ^TObject;
  i: Integer;

begin
  Ptr:= GetPointerToFirst;

  for i:= 1 to Size do
  begin
    TSVMCapInfo (Ptr^).Free;
    Inc (Ptr);

  end;

  inherited Free (False);
  
end;

function TSVMCapInfoCollection.GetSVMCapInfo (Index: Integer): TSVMCapInfo;
begin
  Result:= Member [Index] as TSVMCapInfo;

end;

end.



