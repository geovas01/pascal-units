unit LPRUnit;
(*$define LPRUnit_Debug*)
interface
uses
  FMLImage, HashUnit, CollectionUnit, Graphics,
  PlateSVMClientUnit, SysUtils, FeatureUnit;

type

  TSignOnPlate= class (TObject)
  private
    FTopLeft, FBotRight: TPoint;
    FComponentCollection: TComponentCollection;
    FBitmap: TBitmap;
    FText: WideString;
    FPoint: Extended;
    FPlace: Integer;
    FDigitImage: TFMLImage;

    procedure AddNewComponent (Component: TComponent);
    function GetBitmap: TBitmap;
    function GetText: WideString;
    function GetDigitImage: TFMLImage;

  public
    RecIndex: Integer;
    property ComponentCollection: TComponentCollection read FComponentCollection;
    property Bitmap: TBitmap read GetBitmap;
    property Text: WideString read GetText;
    property Point: Extended read FPoint;
    property Place: Integer read FPlace;
    property DigitImage: TFMLImage read GetDigitImage;

    constructor Create (Index: Integer);
    procedure Free;

    procedure LoadFromFile (var InputFile: TextFile);

    function IsComponentMine (Component: TComponent; OriginPos: TPoint= nil): Boolean;
    function ClearPoorColumnAndLines: Boolean;
//    procedure AddComponent (Component: TComponent);

    procedure Print (BaseFileName: String);
    function Recognize (SVMClient: TPlateSVMClient): String; overload;
    function Recognize (KohonenClient: TPlateKohonenClient; IsNumber: Boolean): WideString; overload;

  end;

  TPlateSingCollection= class (TBaseCollection)
  private
    FConfigFileName: String;
    FSVMClient: TPlateSVMClient;
    MinImageProperties, MaxImageProperties,
    PointsInImage: TPoint;
    FRecognitionClient: TPlateKohonenClient;
    FKohonenClient: TPlateKohonenClient;
    function GetSignOnPlate (Index: Integer): TSignOnPlate;
    procedure LoadFromConfigFile (FileName: String);

    function IsSuitableComponent (AComponent: TComponent): Boolean;
    procedure Sort;

    procedure MoveCenterOfMass (Delta: TPoint);

  public
    property SignOnPlate [Index: Integer]: TSignOnPlate read GetSignOnPlate;
    property SVMClient: TPlateSVMClient read FSVMClient;
    property KohonenClient: TPlateKohonenClient read FKohonenClient;

    procedure AddSignOnPlate (NewSignOnPlate: TSignOnPlate);

    constructor CreateSVM (ConfigFileName: String; SVMClient: TPlateSVMClient); overload;
    constructor Createkohonen (ConfigFileName: String; KohonenClient: TPlateKohonenClient); overload;
    constructor Create; overload;
    procedure Free (FreeObj: Boolean= True);
    procedure LoadBitmap (Bitmap: TBitmap); overload;
    procedure LoadBitmap (FileName: String); overload;
    procedure MergeIfNecessary;
    procedure RemoveExtraSigns (Width, Heigth: Integer);
    procedure UpdateYourSelf;

    procedure Recongnize;

  end;

implementation

uses ExceptionUnit, Math;

{ TSignOnPlate }

procedure TSignOnPlate.AddNewComponent (Component: TComponent);
var
  i, ReplacementIndex: Integer;
  ComponentsPtr,
  NextComponentsPtr: PComponent;

  
begin
  ComponentsPtr:= FComponentCollection.ComponentPointer;

  ReplacementIndex:= FComponentCollection.Count;
  for i:= 0 to FComponentCollection.Count- 1 do
  begin

    if Component.CenterOfMass.x/ Component.Count<
      ComponentsPtr^.CenterOfMass.x/ ComponentsPtr^.Count then
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

  if Component.MinX< FTopLeft.x then
    FTopLeft.x:= Component.MinX;
  if FBotRight.x< Component.MaxX then
    FBotRight.x:= Component.MaxX;
    
  if Component.MinY< FTopLeft.y then
    FTopLeft.y:= Component.MinY;
  if FBotRight.y< Component.MaxY  then
    FBotRight.y:= Component.MaxY;
    
end;

constructor TSignOnPlate.Create (Index: Integer);
begin
  inherited Create;

  FComponentCollection:= TComponentCollection.Create;
  FTopLeft:= TPoint.Create (MaxInt, MaxInt);
  FBotRight:= TPoint.Create (0, 0);
  FPlace:= Index;
  FBitmap:= nil;
  FDigitImage:= nil;
  FText:= '';
  
end;

procedure TSignOnPlate.Free;
begin
  FComponentCollection.Free;
  FTopLeft.Free;
  FBotRight.Free;
  if FBitmap<> nil then
    FBitmap.Free;

  if FDigitImage<> nil then
    FDigitImage.Free;
    
  inherited;
  
end;

function TSignOnPlate.GetBitmap: TBitmap;
begin
  if FBitmap= nil then
  begin
    Result:= DigitImage.GetAsBitmap;
    FBitmap:= Result;
    
  end
  else
    Result:= FBitmap;

end;

function TSignOnPlate.GetDigitImage: TFMLImage;
begin
  if FDigitImage= nil then
    FDigitImage:= TFMLImage.Create (ComponentCollection);

  Result:= FDigitImage;
  
end;

function TSignOnPlate.GetText: WideString;
begin
  Result:= FText;
  
end;

function TSignOnPlate.IsComponentMine (Component: TComponent;
  OriginPos: TPoint): Boolean;
var
  Point: TPoint;

begin
  Point:= Component.CenterOfMass.Copy;
  Point.Scale (1/ Component.Count);
  if OriginPos<> nil then
    Point.Move (OriginPos);

  if (Point.x< FBotRight.x) and (FTopLeft.x< Point.x) and
    (Point.y< FBotRight.y) and (FTopLeft.y< Point.y) then
  begin
    Result:= True;
    AddNewComponent (Component);

  end
  else
    Result:= False;

  Point.Free;

end;

procedure TSignOnPlate.LoadFromFile(var InputFile: TextFile);
begin
  raise ENotImplemented.Create ('');

end;

procedure TSignOnPlate.Print (BaseFileName: String);
begin
  raise ENotImplemented.Create ('');

end;

function TSignOnPlate.Recognize (SVMClient: TPlateSVMClient): String;
const
  Mask: String=
     '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  MaxIndex: Integer;
  DigitResult: TDoubleCollection;

begin
  FText:= '';

  DigitResult:= SVMClient.QueryOnSign (DigitImage.ExtractFeatures (35, 10));

  MaxIndex:= DigitResult.MaxIndex;
  FPoint:= DigitResult.Member [MaxIndex];
  FText:= Mask [MaxIndex+ 1];

  DigitResult.Free;
  DigitImage.Free;

  Result:= FText;

end;

function TSignOnPlate.ClearPoorColumnAndLines: Boolean;
var
  BlkPixInCols: TBlackPixelCountInColumns;
//  BlkPixInRows: TBlackPixelCountInRows;
  {r, }c: Integer;
  Sum: Integer;
  {RowAvg, }ColAvg: Extended;
  IntPtr: PInteger;
  
begin
  BlkPixInCols:= DigitImage.BlackPixelCountInColumns;
//  BlkPixInRows:= DigitImage.BlackPixelCountInRows;
  Result:= False;
{
  IntPtr:= @BlkPixInRows [0];
  Sum:= 0;
  for r:= 0 to DigitImage.Row- 1 do
  begin
    Inc (Sum, IntPtr^);
    Inc (IntPtr);

  end;
  RowAvg:= Sum/ DigitImage.Row;
}
  IntPtr:= @BlkPixInCols [0];
  Sum:= 0;
  for c:= 0 to DigitImage.Column- 1 do
  begin
    Inc (Sum, IntPtr^);
    Inc (IntPtr);

  end;
  ColAvg:= Sum/ DigitImage.Column;
{
  IntPtr:= @BlkPixInRows [0];
  for r:= 0 to DigitImage.Row- 1 do
  begin
    if (IntPtr^<= RowAvg/ 3)
      and (0< IntPtr^) then
    begin
      Result:= True;
      DigitImage.ClearLine (r);

    end;
    Inc (IntPtr);

  end;
}
  IntPtr:= @BlkPixInCols [0];
  for c:= 0 to DigitImage.Column- 1 do
  begin
    if (IntPtr^< ColAvg/ 3)
      and (0< IntPtr^) then
    begin
      Result:= True;
      DigitImage.ClearColumn (c);

    end;
    Inc (IntPtr);

  end;

end;

function TSignOnPlate.Recognize (
  KohonenClient: TPlateKohonenClient; IsNumber: Boolean): WideString;
const
  DigitsMask: array [18..26] of WideString=
     ('1', '2', '3', '4', '5', '6', '7', '8', '9');
  PersianLetterMask: array [1..17] of String=
     (
(*$include ..\LabelsDefinition.inc*)
     );
var
  MaxIndex: Integer;
  PersianCharIndex: Integer;

begin
  FText:= '';

  MaxIndex:= KohonenClient.AskQuery (DigitImage, IsNumber);
  RecIndex := MaxIndex;

  if IsNumber then
    FText:= DigitsMask [MaxIndex]
  else
  begin
    if MaxIndex= 27 then
      FText:= '*'
    else
    begin
      FText:= PersianLetterMask [MaxIndex];
    end;
  end;

  Result:= FText;

end;

{ TPlateSingCollection }

procedure TPlateSingCollection.AddSignOnPlate (NewSignOnPlate: TSignOnPlate);
begin
  inherited Add (NewSignOnPlate);
  
end;
var
  ClearCounter: Integer;
  
procedure TPlateSingCollection.UpdateYourSelf;
var
  ActiveIndex,
  i, j: Integer;
  PlateSign: TSignOnPlate;
  CompCol: TComponentCollection;
  PlateSignCol: TPlateSingCollection;
  NewSignOnPlate: TSignOnPlate;
  ActiveComponent: TComponent;
  FMLImage: TFMLImage;
  CompTopLeftPoint: TPoint;
  Max, Index: Integer;
  TLSearchArea, BRSearchArea: TPoint;
  k: Integer;
  
begin
  ActiveIndex:= 0;
  k:= 0;
  
  while ActiveIndex< Size do
  begin
    PlateSign:= SignOnPlate [ActiveIndex];
    if PlateSign.ClearPoorColumnAndLines then
    begin
(*$ifdef LPRUnit_DEBUG*)
      PlateSign.DigitImage.SaveAsBitmap ('C:\C'+ IntToStr (k)+ '.bmp');
      Inc (ClearCounter);
      
(*$endif*)
      Max:= -1;

      for i:= 0 to PlateSign.ComponentCollection.Count- 1 do
        if Max< PlateSign.ComponentCollection.Component [i].Count then
        begin
          Max:= PlateSign.ComponentCollection.Component [i].Count ;
          Index:= i;
          
        end;

      if (20< PlateSign.ComponentCollection.Component [Index].Width) and
         (15< PlateSign.ComponentCollection.Component [Index].Height) then
      begin
        CompTopLeftPoint:= PlateSign.ComponentCollection.Component [Index].
          GetMinimum;

        Self.Delete (ActiveIndex);
        TLSearchArea:= TPoint.Create (0, 0);
        BRSearchArea:= PlateSign.FBotRight.Copy.Move (
          -PlateSign.FTopLeft.x, -PlateSign.FTopLeft.y);
        
        PlateSignCol:= TPlateSingCollection.Createkohonen (FConfigFileName, nil);
        CompCol:= PlateSign.DigitImage.FindAllComponentsInBox
            (TLSearchArea, BRSearchArea, False);
        TLSearchArea.Free;
        BRSearchArea.Free;
      
        for j:= 0 to CompCol.Count- 1 do
        begin
          ActiveComponent:= CompCol.Component [j];
          if IsSuitableComponent (ActiveComponent) then
          begin

            NewSignOnPlate:= TSignOnPlate.Create (Self.Size+ 1);
            ActiveComponent.Move (CompTopLeftPoint);

            NewSignOnPlate.AddNewComponent (ActiveComponent);

            PlateSignCol.AddSignOnPlate (NewSignOnPlate);
      (*$ifdef LPRUnit_Debug*)
            NewSignOnPlate.Bitmap.SaveToFile ('C:\N'+ IntToStr (j)+ '.bmp');
      (*$endif*)

          end
          else
          begin
      (*$ifdef LPRUnit_Debug*)
            FMLImage:= TFMLImage.Create (ActiveComponent);
            FMLImage.SaveAsBitmap ('C:\A'+ IntToStr (j)+ '.bmp');
            FMLImage.Free;
      (*$endif*)

            ActiveComponent.Free;

          end;

        end;
        CompTopLeftPoint.Free;

        CompCol.Free (False);
  //      if PlateSignCol.Size<> 0 then
  //        PlateSignCol.UpdateYourSelf;
        for j:= 0 to PlateSignCol.Size- 1 do
          Self.AddSignOnPlate (PlateSignCol.SignOnPlate [j]);
        PlateSignCol.Free (False);

        Dec (ActiveIndex);

      end;

    end;

    Inc (ActiveIndex);
    Inc (k);

  end;


end;

constructor TPlateSingCollection.CreateSVM (ConfigFileName: String;
  SVMClient: TPlateSVMClient);

begin
  inherited Create;

  FSVMClient:= SVMClient;
  MinImageProperties:= TPoint.Create;
  MaxImageProperties:= TPoint.Create;
  PointsInImage:= TPoint.Create;

  FConfigFileName:= ConfigFileName;
  LoadFromConfigFile (ConfigFileName);

end;

procedure TPlateSingCollection.Free (FreeObj: Boolean= True);
var
  i: Integer;
  
begin
  MinImageProperties.Free;
  MaxImageProperties.Free;
  PointsInImage.Free;

  if FreeObj then
    for i:= 0 to Size- 1 do
      SignOnPlate [i].Free;

  inherited Free;
  
end;

function TPlateSingCollection.GetSignOnPlate (Index: Integer): TSignOnPlate;
begin
  Result:= Member [Index] as TSignOnPlate;
  
end;

procedure TPlateSingCollection.LoadBitmap (Bitmap: TBitmap);

  procedure DeleteVerticalBlackLine (FMLImage: TFMLImage; Percentage: Extended);
  var
    BlackPixelsInRow: TBlackPixelCountInRows;
    Count: Integer;
    Row, Column: Integer;
    r, c, MaxIndex: Integer;
    CurPointer: PInteger;

  begin
    BlackPixelsInRow:= FMLImage.BlackPixelCountInRows;
    MaxIndex:= 0;
    Row:= FMLImage.Row;
    Column:= FMLImage.Column;
    
    for r:= 0 to Row- 1 do
    begin
      Count:= 0;
      CurPointer:= FMLImage.ScanLine [r];

      for c:= 0 to Column- 1 do
      begin
        Inc (Count, CurPointer^);
        Inc (CurPointer);

      end;

      if Column* Percentage< Count then
        FMLImage.ClearLine (r)
      else
//        Break;

    end;
    Exit;
    
    for r:= Row- 1 downto 0 do
    begin
      Count:= 0;
      CurPointer:= FMLImage.ScanLine [r];

      for c:= 0 to Column- 1 do
      begin
        Inc (Count, CurPointer^);
        Inc (CurPointer);

      end;

      if Row* Percentage< Count then
        FMLImage.ClearLine (r)
      else
        Break;

    end;
  end;

  procedure DeleteHorizentalBlackLine (FMLImage: TFMLImage; Percentage: Extended);
  var
    Count: Integer;
    Row, Column: Integer;
    r, c, MaxIndex: Integer;
    CurPointer: PInteger;

  begin
    MaxIndex:= 0;
    Row:= FMLImage.Row;
    Column:= FMLImage.Column;
    
    for c:= 0 to Column- 1 do
    begin
      Count:= 0;

      for r:= 0 to Row- 1 do
      begin
        CurPointer:= FMLImage.ScanLine [r];
        Inc (CurPointer, c);

        Inc (Count, CurPointer^);

      end;

      if Row* Percentage< Count then
        FMLImage.ClearColumn (c)
      else
        Break;

    end;

    for c:= Column- 1 downto 0 do
    begin
      Count:= 0;

      for r:= 0 to Row- 1 do
      begin
        CurPointer:= FMLImage.ScanLine [r];
        Inc (CurPointer, c);
        
        Inc (Count, CurPointer^);

      end;

      if Row* Percentage< Count then
        FMLImage.ClearColumn (c)
      else
        Break;

    end;

  end;

  function FindPlateArea (Image: TFMLImage): TFMLImage;
  var
    BlackPixInRows: TBlackPixelCountInRows;
    Row, ColumnDiv2: Integer;
    MaxFallIndex,
    MaxRiseIndex: Integer;

    function FindIndexes (Image: TFMLImage): Integer;
    var
      r: Integer;
      RowPtr: PInteger;
      GoingUpward: Boolean;
      MaxWidth: Integer;
      LastFall: Integer;

    begin
      BlackPixInRows:= Image.BlackPixelCountInRows;
    
      RowPtr:= @BlackPixInRows [0];
      LastFall:= 0;
      MaxWidth:= 0;
      GoingUpward:= True;

      for r:= 0 to Row- 1 do
      begin
        if GoingUpward then
        begin
          if RowPtr^< ColumnDiv2 then
          begin
            LastFall:= r;
            GoingUpward:= False;

          end
        
        end
        else//Going Downward
          if ColumnDiv2< RowPtr^ then
          begin
            if MaxWidth<= r- LastFall then
            begin
              MaxWidth:= r- LastFall;
              MaxFallIndex:= LastFall;
              MaxRiseIndex:= r;
            
            end;
            GoingUpward:= True;
          
          end;

          Inc (RowPtr);
        
        end;

      if not GoingUpward then
      begin
        if MaxWidth< Row- LastFall- 1 then
        begin
          MaxWidth:= Row- LastFall;
          MaxFallIndex:= LastFall;
          MaxRiseIndex:= Row- 1;
        
        end;

      end;
      Result:= MaxWidth;
      
    end;
  var
    i: Integer;
    MaxRot, FallIndex, RiseIndex: Integer;
    Temp,
    MaxValue: Integer;
    NewImage: TFMLImage;
    TopLeft, BotRight: TPoint;

  begin
    MaxValue:= -1;
    NewImage:= Image;
    
{
    for i:= -5 to 5 do
    begin
      NewImage:= Image.Rotate (i);
      NewImage.SaveAsBitmap ('C:\RotatedImage'+ IntToStr (i)+ '.bmp');
}
      Row:= Image.Row;
      ColumnDiv2:= Image.Column* 2 div 3;
      Temp:= FindIndexes (NewImage);
      FallIndex:= MaxFallIndex;
      RiseIndex:= MaxRiseIndex;
{
      if MaxValue< Temp then
      begin
        MaxValue:= Temp;
        FallIndex:= MaxFallIndex;
        RiseIndex:= MaxRiseIndex;
        MaxRot:= i;

      end;
      NewImage.Free;

    end;
    NewImage:= Image.Rotate (MaxRot);
    Image.Free;
}
    
    TopLeft:= TPoint.Create (NewImage.Column div 10, FallIndex+ 1);//??!!
    BotRight:= TPoint.Create (NewImage.Column, RiseIndex- 1);
    
    Result:= NewImage.Copy (TopLeft, BotRight);
    
    TopLeft.Free;
    BotRight.Free;
    
  end;

var
  FMLImage: TFMLImage;
  TopLeft, BotRight: TPoint;
  NewComponentCollection,
  ComponentCollection: TComponentCollection;
  Max, Index,
  i, j: Integer;
  CompTopLeftPoint: TPoint;
  NewSubSignOnPlate,
  NewSignOnPlate: TSignOnPlate;
  PlateImage: TFMLImage;
  NewPlateSignCollection: TPlateSingCollection;
  TLSearchArea, BRSearchArea: TPoint;
  Width: Integer;

begin
  FMLImage:= TFMLImage.Create;
  FMLImage.LoadBitMap (Bitmap);
  
  PlateImage:= FindPlateArea (FMLImage);
(*$ifdef LPRUnit_Debug*) 
  PlateImage.SaveAsBitmap ('C:\PlateImage.bmp');
(*$endif*)
  Width:= PlateImage.Column;

  FMLImage.Free;

  FMLImage:= PlateImage.ConvertToBinary;

  FMLImage.SaveAsBitmap ('C:\MonoChrome.bmp');

  TopLeft:= TPoint.Create (0, 0);
  BotRight:= TPoint.Create (PlateImage.Column- 3, PlateImage.Row- 1);

  PlateImage.Free;

  ComponentCollection:= FMLImage.FindAllComponentsInBox (TopLeft, BotRight, False);

  TopLeft.Free;
  BotRight.Free;
  FMLImage.Free;

  for i:= 0 to ComponentCollection.Count- 1 do
  begin
    if IsSuitableComponent (ComponentCollection.Component [i]) then
    begin
      NewSignOnPlate:= TSignOnPlate.Create (Self.Size+ 1);
      NewSignOnPlate.AddNewComponent (ComponentCollection.Component [i]);
      Self.AddSignOnPlate (NewSignOnPlate);
(*$ifdef LPRUnit_Debug*)
      NewSignOnPlate.Bitmap.SaveToFile ('C:\'+ IntToStr (i)+ '.bmp');
(*$endif*)

    end
    else
    begin
(*$ifdef LPRUnit_Debug*)
      FMLImage:= TFMLImage.Create (ComponentCollection.Component [i]);
      FMLImage.SaveAsBitmap ('C:\A'+ IntToStr (i)+ '.bmp');
      FMLImage.Free;
(*$endif*)
     if (20< ComponentCollection.Component [i].Width)
      and (5< ComponentCollection.Component [i].Height) then
     begin

        NewSignOnPlate:= TSignOnPlate.Create (Self.Size+ 1);
        NewSignOnPlate.AddNewComponent (ComponentCollection.Component [i]);

        if NewSignOnPlate.ClearPoorColumnAndLines then
        begin
          Max:= -1;

          for j:= 0 to NewSignOnPlate.ComponentCollection.Count- 1 do
            if Max< NewSignOnPlate.ComponentCollection.Component [j].Count then
            begin
              Max:= NewSignOnPlate.ComponentCollection.Component [j].Count ;
              Index:= j;
          
            end;

          CompTopLeftPoint:= NewSignOnPlate.ComponentCollection.Component [Index].
            GetMinimum.Copy;

          TLSearchArea:= TPoint.Create (0, 0);
          BRSearchArea:= NewSignOnPlate.FBotRight.Copy.Move (
            -NewSignOnPlate.FTopLeft.x, -NewSignOnPlate.FTopLeft.y);
        
          NewComponentCollection:= NewSignOnPlate.DigitImage.FindAllComponentsInBox
              (TLSearchArea, BRSearchArea, False);
          TLSearchArea.Free;
          BRSearchArea.Free;

          for j:= 0 to NewComponentCollection.Count- 1 do
          begin
            if IsSuitableComponent (NewComponentCollection.Component [j]) then
            begin
              NewComponentCollection.Component [j].CenterOfMass.Scale (1/ NewComponentCollection.Component [j].Count).
              Move (CompTopLeftPoint).Scale (NewComponentCollection.Component [j].Count);

              NewSubSignOnPlate:= TSignOnPlate.Create (Self.Size+ 1);
              NewSubSignOnPlate.AddNewComponent (NewComponentCollection.Component [j]);
              AddSignOnPlate (NewSubSignOnPlate);
        (*$ifdef LPRUnit_Debug*)
              NewSignOnPlate.Bitmap.SaveToFile ('C:\N'+ IntToStr (j)+ '.bmp');
        (*$endif*)

            end
            else
            begin
        (*$ifdef LPRUnit_Debug*)
              FMLImage:= TFMLImage.Create (NewComponentCollection.Component [j]);
              FMLImage.SaveAsBitmap ('C:\A'+ IntToStr (j)+ '.bmp');
              FMLImage.Free;
        (*$endif*)

              NewComponentCollection.Component [j].Free;

            end;

          end;

          CompTopLeftPoint.Free;
          NewComponentCollection.Free (False);

        end;

        NewSignOnPlate.Free;
        
     end
     else
       ComponentCollection.Component [i].Free;
       
      
    end;

  end;

  ComponentCollection.Free (False);
  UpdateYourSelf;
  Sort;
  for i:= 0 to Self.Size- 1 do
    SignOnPlate [i].DigitImage.SaveAsBitmap ('C:\S'+ IntToStr (i)+ '.bmp');

  if 8< Size then
    RemoveExtraSigns (Width, Bitmap.Height);

(*$ifdef LPRUNIT_DEBUG*)
  for i:= 0 to Self.Size- 1 do
    SignOnPlate [i].DigitImage.SaveAsBitmap ('C:\F'+ IntToStr (i)+ '.bmp');
(*$endif*)

end;

function TPlateSingCollection.IsSuitableComponent (
  AComponent: TComponent): Boolean;
var
  CompWidth,
  CompHeight: Integer;
  Temp: Integer;
  
begin
  Result:= False;
{
MinImageProperties:= TPoint.Create;
MaxImageProperties:= TPoint.Create;
PointsInImage:= TPoint.Create;
}
  CompWidth:= AComponent.MaxX- AComponent.MinX;
  CompHeight:= AComponent.MaxY- AComponent.MinY;

  if {(CompWidth< MinImageProperties.x) or}
     (CompHeight< MinImageProperties.y) then
    Exit;
      
  if (MaxImageProperties.x< CompWidth) or
     (MaxImageProperties.y< CompHeight) then
    Exit;

  if 100* AComponent.Count< PointsInImage.x*
      (CompWidth* CompHeight) then
    Exit;

  Temp:= CompWidth* CompHeight;
  Temp:= Temp* PointsInImage.y;
  if (Temp< 100* AComponent.Count)
      and ({??!!Size Of 1}3< CompWidth) then
    Exit;

(*
  if (Abs ((CompHeight/ CompWidth)- 1)<  1/3)
  and (Abs ((CompWidth/ CompHeight)- 1)<  1/3) and
  (CompWidth< 15{??!!}) then
  else
  begin
    Result:= True;
    Exit;

  end;
*)
  Result:= True;
    
end;


procedure TPlateSingCollection.LoadBitmap (FileName: String);
var
  Bitmap: TBitmap;

begin
  Bitmap:= TBitmap.Create;
  Bitmap.LoadFromFile (FileName);
  LoadBitmap (Bitmap);
  
  Bitmap.Free;

end;

procedure TPlateSingCollection.LoadFromConfigFile (FileName: String);
var
  InputFile: TextFile;
  S: String;

begin
  AssignFile (InputFile, FileName);
  Reset (InputFile);

  Readln (InputFile, S);
  MinImageProperties.Free;
  MinImageProperties:= TPoint.Create (S);

  Readln (InputFile, S);
  MaxImageProperties.Free;
  MaxImageProperties:= TPoint.Create (S);

  Readln (InputFile, S);
  PointsInImage.Free;
  PointsInImage:= TPoint.Create (S);
  
  CloseFile (InputFile);

end;

procedure TPlateSingCollection.MergeIfNecessary;
var
  i, j: Integer;
  Min: Extended;
  MinIndex: Integer;

begin
  if 8< Size then
  begin
    for i:= 0 to Size- 1 do
      SignOnPlate [i].Recognize (FKohonenClient, i= 2);

    while 8< Size do
    begin
      Min:= SignOnPlate [0].Point;
      MinIndex:= 0;

      for i:= 1 to Size- 1 do
        if SignOnPlate [i].Point< Min then
        begin
          Min:= SignOnPlate [i].Point;
          MinIndex:= i;

        end;

      SignOnPlate [MinIndex].Free;
      Self.Delete (MinIndex);
      
    end;

  end;

end;

procedure TPlateSingCollection.Sort;
var
  i, j, Temp, Index, Max: Integer;
  Centers: array of TPoint;
  Counts: array of Integer;
  Sign: TSignOnPlate;
  Order: array of Integer;
  NewSignOnPlateCol: TPlateSingCollection;
  
begin
  SetLength (Centers, Size);
  SetLength (Order, Size);
  SetLength (Counts, Size);

  for i:= 0 to Size- 1 do
  begin
    Order [i]:= i;
    Sign:= SignOnPlate [i];
    Max:= -1;

    for j:= 0 to Sign.ComponentCollection.Count- 1 do
      if Max< Sign.ComponentCollection.Component [j].Count then
      begin
        Max:= Sign.ComponentCollection.Component [j].Count ;
        Index:= j;
        
      end;

    Centers [i]:= SignOnPlate [i].ComponentCollection.Component [Index].CenterOfMass.Copy;
    Centers [i].Scale (1/ Max);
    Counts [i]:= Max;

  end;

  for i:= 0 to Size- 1 do
    for j:= i+ 1 to Size- 1 do
      if Centers [Order [j]].x< Centers [Order [i]].x then
      begin
        Temp:= Order [i];
        Order [i]:= Order [j];
        Order [j]:= Temp;
                
      end;
      
  NewSignOnPlateCol:= TPlateSingCollection.Create;
  for i:= 0 to Size- 1 do
    NewSignOnPlateCol.AddSignOnPlate (SignOnPlate [Order [i]]);

  for i:= Size- 1 downto 0 do
    Delete (i);
  for i:= 0 to NewSignOnPlateCol.Size- 1 do
    Self.AddSignOnPlate (NewSignOnPlateCol.SignOnPlate [i]);

  for i:= 0 to Size- 1 do
  begin
    Sign:= SignOnPlate [i];
    Max:= -1;

    for j:= 0 to Sign.ComponentCollection.Count- 1 do
      if Max< Sign.ComponentCollection.Component [j].Count then
      begin
        Max:= Sign.ComponentCollection.Component [j].Count ;
        Index:= j;
        
      end;

    Centers [i]:= SignOnPlate [i].ComponentCollection.Component [Index].CenterOfMass.Copy;
    Centers [i].Scale (1/ Max);

  end;

  SetLength (Centers, 0);
  SetLength (Counts, 0);
  NewSignOnPlateCol.Free (False);
  
end;

constructor TPlateSingCollection.Create;
begin

  inherited Create;

  MinImageProperties:= TPoint.Create;
  MaxImageProperties:= TPoint.Create;
  PointsInImage:= TPoint.Create;
  
end;

procedure TPlateSingCollection.Recongnize;
var
  Index: Integer;
  i: Integer;

begin
  for i:= 0 to Size- 1 do
    SignOnPlate [i].FText:= SignOnPlate [i].Recognize (FKohonenClient, i<> 2);

end;

constructor TPlateSingCollection.Createkohonen(ConfigFileName: String;
  KohonenClient: TPlateKohonenClient);
begin
  inherited Create;

  FKohonenClient:= KohonenClient;
  MinImageProperties:= TPoint.Create;
  MaxImageProperties:= TPoint.Create;
  PointsInImage:= TPoint.Create;
  
  FConfigFileName:= ConfigFileName;
  LoadFromConfigFile (ConfigFileName);


end;

procedure TPlateSingCollection.MoveCenterOfMass (Delta: TPoint);
var
  i, j: Integer;

begin
  for i:= 0 to Size- 1 do
    for j:= 0 to SignOnPlate [i].FComponentCollection.Count- 1 do
      SignOnPlate [i].FComponentCollection.Component [j].CenterOfMass.Move (Delta);

end;

procedure TPlateSingCollection.RemoveExtraSigns (Width, Heigth: Integer);
var
  SignsCenter: array of Integer;
  RegionsCenter: array [0..7] of Integer;
  DistSignsAndCenters: array of array of Integer;
  
  CurrentCost,
  CurrentIndex,  BestTillNow: array [0..8] of Integer;
  Min: Integer;

  procedure FindBestAssignment (Index, SelectedCon, Cost: Integer);
  var
    i: Integer;
    
  begin
    if SelectedCon= 8 then
    begin
      if Cost< Min then
      begin
        Min:= Cost;
        BestTillNow:= CurrentIndex;

      end;

      CurrentIndex [8]:= 8;
      Exit;

    end;
    if (Size<= Index) {or (Size<= Index+ 7- SelectedCon)} then
      Exit;


    CurrentIndex [SelectedCon]:= Index;
    FindBestAssignment (Index+ 1, SelectedCon+ 1, Cost+
      DistSignsAndCenters [Index, SelectedCon]);

    FindBestAssignment (Index+ 1, SelectedCon, Cost);

  end;

var
  i, j, k: Integer;
  
begin
  SetLength (SignsCenter, Size);
  Width:= Width div 8;

  //??!! Should be read from Config File
  RegionsCenter [0]:= Width div 2;
  RegionsCenter [1]:= 3* Width div 2;
  RegionsCenter [2]:= 11* Width div 4;
  RegionsCenter [3]:= 4* Width; 
  RegionsCenter [4]:= 5* Width;
  RegionsCenter [5]:= 6* Width; 
  RegionsCenter [6]:= 51* Width div 8;
  RegionsCenter [7]:= 57* Width div 8; 

  SetLength (DistSignsAndCenters, Size);
  for i:= 0 to Size- 1 do
  begin
    SetLength (DistSignsAndCenters [i], 8);

    SignsCenter [i]:= SignOnPlate [i].ComponentCollection.MinPoint.x+
      (SignOnPlate [i].FBotRight.x- SignOnPlate [i].FTopLeft.x) div 2;

    for j:= 0 to 7 do
      DistSignsAndCenters [i][j]:=
        Abs (RegionsCenter [j]- SignsCenter [i]);

  end;

  Min:= MaxInt;
  FindBestAssignment (0, 0, 0);

  for i:= 0 to Size- 1 do
    SetLength (DistSignsAndCenters [i], 0);

  k:= 7;
  BestTillNow [8]:= Size;
  for i:= 7 downto 0 do
  begin
    for k:= BestTillNow [i+ 1]- 1 downto BestTillNow [i]+ 1 do
    begin
      SignOnPlate [k].Free;
      Self.Delete (k);

    end;

  end;

  for k:= BestTillNow [0]- 1 downto 0 do
  begin
    SignOnPlate [k].Free;
    Self.Delete (k);

  end;

  SetLength (DistSignsAndCenters, 0);
  SetLength (SignsCenter, 0);

end;

initialization
  ClearCounter:= 0;
  
end.

