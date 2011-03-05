unit PersianSubwordUnit;

interface
uses
  FMLImage, CollectionUnit, ExceptionUnit, GeometryUnit,
  Graphics, ComponentsUnit;

type
  TWordSegment= class (TComponentCollection)
  private
    FDotlessImage: TFMLImage;
    FImage: TFMLImage;
    FImageForRecognition: TFMLImage;

    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetDotlessImage: TFMLImage;
    function GetImage: TFMLImage;
    function GetImageForRecognition: TFMLImage;

  public
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property DotlessImage: TFMLImage read GetDotlessImage;
    property Image: TFMLImage read GetImage;
    property ImageForRecognition: TFMLImage read GetImageForRecognition;

    function IsComponentMine (AComponent: TComponent): Boolean;
    (*Tries to draw a line from the first pixel in each row (from left)d
          of the image and then rotate the image*)

    constructor Create;
    destructor Destroy; override;

  end;
  
  TWordSegmentCollection= class (TBaseCollection)
  private
    function GetWordSegment (Index: Integer): TWordSegment;
  public
    property WordSegment [Index: Integer]: TwordSegment read GetWordSegment;
    constructor Create (AComponentCollection: TComponentCollection);
    procedure PrepareForRecognition;

  end;


  TLine= class (TObject)
  private
    FAllSubwords: TWordSegmentCollection;
    function GetWordSegment(Index: Integer): TWordSegment;

  public
    property Subword [Index: Integer]: TWordSegment read GetWordSegment;
    property Subwords: TWordSegmentCollection read FAllSubwords;
    
    constructor Create (ABitmapFileName: String);
    destructor Destroy; override;
    
  end;

implementation

uses
  MyTypes, SysUtils, Math, StrUtils;


{ TPage }

constructor TLine.Create (ABitmapFilename: String);
(*$J+*)
const
    TopLeft: TPoint= nil;
    BotRight: TPoint= nil;
    DilateMask: TArrayofArrayofInt= nil;
(*$J-*)

const
  MinImageWitdth: Integer= 4;
  MinImageHeight: Integer= 10;

  procedure  Init;
  var
    i, j: Integer;
  
  begin
    TopLeft:= TPoint.Create;
    BotRight:= TPoint.Create;
    TopLeft.r:= 0; TopLeft.c:= 0;

    SetLength (DilateMask, 3);
    for i:= 0 to 2 do
    begin
      SetLength (DilateMask [i], 3);
      for j:= 0 to 2 do
        DilateMask [i][j]:= 1;

    end;

  end;

  function CompareComponents (Comp1, Comp2: TComponent): Boolean;
  begin
    Result:= Comp2.MinC< Comp1.MinC;

  end;

var
  Image,
  SegmentImage: TFMLImage;
  i: Integer;
  ActiveComponentCollection: TComponentCollection;
//  ActiveComponent: TComponent;
  Ptr: PObject;

begin
  inherited Create;

  if TopLeft= nil then
    Init;
  
  Image:= TFMLImage.Create;
  Image.LoadBitMap (ABitmapFileName);
//  Image.Dilate (DilateMask);
(*$IFDEF DM_SAVEIMAGES*)   
  Image.SaveAsBitmap (ExtractFilePath (ABitmapFileName)+
    '\Dilated-'+ ExtractFileName (ABitmapFileName));
(*$ENDIF*)
  
  BotRight.r:= Image.Row- 1;
  BotRight.c:= Image.Column- 1;

  ActiveComponentCollection:=
    Image.FindAllComponentsInBox (TopLeft, BotRight);
  Image.Free;
  ActiveComponentCollection.Sort (@CompareComponents);
  FAllSubwords:= TWordSegmentCollection.Create (ActiveComponentCollection);
  ActiveComponentCollection.Clear;
  ActiveComponentCollection.Free;
  
  Ptr:= FAllSubwords.GetPointerToFirst;
  Inc (Ptr, FAllSubwords.Size- 1);
  for i:= FAllSubwords.Size- 1 downto 0 do
  begin
    if (TWordSegment (Ptr^).Width< MinImageWitdth) and
      (TWordSegment (Ptr^).Height< MinImageHeight) then
      FAllSubwords.Delete (i);
    Dec (Ptr);
    
  end;
  
end;

destructor TLine.Destroy;
begin
  FAllSubwords.Free;
  
  inherited;
  
end;

{ TWordSegment }

constructor TWordSegment.Create;
begin
  inherited Create;
  
  FImage:= nil;
  FDotlessImage:= nil;
  FImageForRecognition:= nil;

end;

destructor TWordSegment.Destroy;
begin
  FImage.Free;
  FDotlessImage.Free;
  FImageForRecognition.Free;

  inherited;

end;

function TWordSegment.GetDotlessImage: TFMLImage;
(* A Dot is defined as follows:
  1- It is the smallest component of the image
  2- Most of its area is black
  3- It has a relativly small height and width
  4- It has a relativly big distance to the biggest component 
*)
(*$J+*)
const
  TempCompCol: TComponentCollection= nil;
  Visited: array of Boolean= nil;
  Mat: array of array of Boolean= nil;
  
(*$J-*)
const
  AdjThreshold: Integer= 3;

procedure FindConnectedCommponentTo (CompIndex: Integer;
  CompCol: TComponentCollection);
var
  i: Integer;

begin
  if Visited [CompIndex] then
    Exit;
  Visited [CompIndex]:= True;
  CompCol.Add (Component [CompIndex]);

  for i:= 0 to Size- 1 do
    if Mat [CompIndex, i] then
      FindConnectedCommponentTo (i, CompCol);

end;

var
  i, j: Integer;
  Ptr: PObject;
  Temp,
  MaxDist: Extended;
  MaxIndex,
  BiggestCompIndex,
  MaxCount: Integer;
  BiggestComponent: TComponent;
  

begin
{ TODO -oAmir -cPersianSubwordUnit : Add constant instead of 10%. (GetDotlessImage) }

  if FDotlessImage= nil then
  begin
    if TempCompCol= nil then//First run
    begin
      TempCompCol:= TComponentCollection.Create;
      SetLength (Visited, Size);
      SetLength (Mat, Size, Size);
      
    end;

    if Length (Visited)< Size then
    begin
      SetLength (Visited, Size);
      SetLength (Mat, Size, Size);
      
    end;
    
    FillChar (Visited [0], SizeOf (Visited), 0);


    MaxCount:= -1;      
    Ptr:= GetPointerToFirst;
    for i:= 1 to Size do
    begin
      if MaxCount< TComponent (Ptr^).Count then
      begin
        BiggestComponent:= TComponent (Ptr^);
        BiggestCompIndex:= i;
        MaxCount:= TComponent (Ptr^).Count;

      end;

      Inc (Ptr);
      
    end;
    Dec (BiggestCompIndex);

    for i:= 0 to Size- 1 do
      for j:= i+ 1 to Size- 1 do
        if Component [i].FindMinDistToComp (Component [j])<= AdjThreshold then// 5 pixel 
        begin
          Mat [i, j]:= True;
          Mat [j, i]:= True;

        end
        else
        begin
          Mat [i, j]:= False;
          Mat [j, i]:= False;

        end;

    for i:= 0 to Size- 1 do
      if (6* 6<Component [i].Height* Component [i].Width) then
        Mat [BiggestCompIndex, i]:= True;


    TempCompCol.Clear;
    FindConnectedCommponentTo (BiggestCompIndex, TempCompCol);
    FDotlessImage:= TFMLImage.Create (TempCompCol);
    TempCompCol.Clear;
    
  end;

  Result:= FDotlessImage;
  
end;

function TWordSegment.GetHeight: Integer;
begin
  Result:= MaxR- MinR+ 1;

end;

function TWordSegment.GetImage: TFMLImage;
begin
  if FImage= nil then
    FImage:= TFMLImage.Create (Self);

  Result:= FImage;

end;

function TWordSegment.GetImageForRecognition: TFMLImage;

  procedure Prepare;
  (*
    The line L(y=mx) must be find such that
      E= Sum_r (mr- c(r))^2 to be minimized. where
      c(r) returns the column of first blackp
      pixel in rth row. In case the row has not
      any black pixel, returns Column.

      The value of m which is minize the E is:
        E'=2Sum_r r(mr- c(r))
          =2Sum_r mr^2- 2Sum_r c(r)
          =m Row(Row+ 1)(2*Row+ 1)/3- 2Sum_r c(r)= 0
          ==>m= 6 Sum_r c(r)/ (Row(Row+ 1)(2*Row+ 1))


  *)
  (*$J+*)
  const
    TL: TPoint= nil;
    BR: TPoint= nil;
  (*$J-*)

  var
    SumCRL: Integer;
    PixPtr: PInteger;
    Found: Boolean;
    r, c: Integer;
    m: Extended;

  begin
    if TL= nil then
    begin
      TL:= TPoint.Create (0, 0);
      BR:= TPoint.Create;

    end;
    BR.r:= DotlessImage.Row- 1;
    BR.c:= FDotlessImage.Column- 1;

    FImageForRecognition:= DotlessImage.Copy (TL, BR);
    for r:= 0 to FImageForRecognition.Row- 1 do
    begin
      PixPtr:= FImageForRecognition.ScanLine [r];
      Found:= False;
      for c:= 1 to FImageForRecognition.Column do
      begin
        if PixPtr^= BLACK then
        begin
          Inc (SumCRL, c);
          Found:= True;
          Break;

        end;
        Inc (PixPtr);
        
      end;

      if not Found then
        Inc (SumCRL, FImageForRecognition.Column);

    end;

    r:= FImageForRecognition.Row;
    m:= 6* SumCRL/ (r* (r+ 1)* (2* r+ 1));
    FImageForRecognition.Rotate (90- Round (ArcTan (m)* 180/ Pi)); 

  end;

begin
  if FImageForRecognition= nil then
    Prepare;
    
  Result:= FImageForRecognition;


end;

function TWordSegment.GetWidth: Integer;
begin
  Result:= MaxC- MinC+ 1;

end;

function TWordSegment.IsComponentMine (AComponent: TComponent): Boolean;
begin
  Result:= False;
  
end;

{ TWordSegmentsCollection }

constructor TWordSegmentCollection.Create (
  AComponentCollection: TComponentCollection);
(*$J+*)
const
  Marked: array of Boolean= nil;
(*$J-*)
const
  AdjThreshold: Integer= 3;

procedure DFS (Subword: TWordSegment; Ptr: PObject;  Index: Integer);
  (*Returns true if two component share a common area align the x-axis*)

  function AreAdjanced (Comp1, Comp2: TComponent): Boolean;
  const
    MinOverlapPercentage= 20;
    MinDist= 2;
    
  begin
    Result:= False;
    if (Comp1.Width= 1) or (Comp1.Height= 1) or
    ((Comp2.Width= 1) or (Comp2.Height= 1)) then
      Exit;

{ TODO -oAmir -cPersianSubwordUnit : Add some constant instead of values. (AreAdjanced) }
    Result:= (Max (Comp1.MaxC- Comp2.MinC, Comp2.MaxC- Comp1.MinC)+ 1<
      (Comp1.Width+ Comp2.Width- (MinOverlapPercentage* Min (Comp1.Width, Comp2.Width)) div 100))//They have at least some pixele overlap
       or
      ((Max (Comp1.MaxC- Comp2.MinC, Comp2.MaxC- Comp1.MinC)<=
       Comp1.Width+ Comp2.Width) and
       (Comp1.FindMinHorizDistClosestPointsToComp (Comp2)<= MinDist));
       

    if not Result then
      Result:=
        ((Comp1.MinC- Comp2.MinC)* (Comp1.MaxC- Comp2.MinC)<= 0) and
          ((Comp1.MinC- Comp2.MaxC)* (Comp1.MaxC- Comp2.MaxC)<= 0)
          or
           ((Comp2.MinC- Comp1.MinC)* (Comp2.MaxC- Comp1.MinC)<= 0)
          and ((Comp2.MinC- Comp1.MaxC)* (Comp2.MaxC- Comp1.MaxC)<= 0);  
      
    if not Result then
      Result:= (10* Max (Comp1.MaxC- Comp2.MinC, Comp2.MaxC- Comp1.MinC)+ 1<
      9* (Comp1.Width+ Comp2.Width)) and (Comp1.FindMinDistToComp (Comp2)<= AdjThreshold);

  end;
  
var
  i: Integer;
  LeftMostNeighIndex,
  RightMostNeighIndex: Integer;
  LeftMostNeighPtr,
  RightMostNeighPtr: PObject;

begin
  Marked [Index]:= True;
  Subword.AddComponent (TComponent (Ptr^));
  
  LeftMostNeighIndex:= Index- 1;
  LeftMostNeighPtr:= Ptr;
  Dec (LeftMostNeighPtr);

  if -1< LeftMostNeighIndex then
    while AreAdjanced (TComponent (Ptr^), TComponent (LeftMostNeighPtr^)) do
    begin
      Dec (LeftMostNeighPtr);
      Dec (LeftMostNeighIndex);
      if LeftMostNeighIndex= -1 then
        Break;

    end;
  Inc (LeftMostNeighPtr);
  Inc (LeftMostNeighIndex);

  RightMostNeighIndex:= Index+ 1;
  RightMostNeighPtr:= Ptr;
  Inc (RightMostNeighPtr);
  if RightMostNeighIndex< AComponentCollection.Size then
    while AreAdjanced (TComponent (Ptr^), TComponent (RightMostNeighPtr^)) do
    begin
      Inc (RightMostNeighIndex);
      Inc (RightMostNeighPtr);
      if RightMostNeighIndex= AComponentCollection.Size then
        Break;

    end;
  Dec (RightMostNeighIndex);

  Ptr:= LeftMostNeighPtr;
  for i:= LeftMostNeighIndex to RightMostNeighIndex do
  begin
    if not Marked [i] then
      DFS (Subword, Ptr, i);
    Inc (Ptr);
    
  end;

end;

var
  i: Integer;
  Ptr: PObject;
  ActiveSubword: TWordSegment;
  
begin
  inherited Create;

  if Marked= nil then
    SetLength (Marked, AComponentCollection.Size)
  else if Length (Marked)< AComponentCollection.Size then
    SetLength (Marked, AComponentCollection.Size);
  FillChar (Marked [0], Sizeof (Boolean)* Length (Marked), 0);

  Ptr:= AComponentCollection.GetPointerToFirst;
  for i:= 0 to AComponentCollection.Size- 1 do
  begin
    if not Marked [i] then
    begin
      ActiveSubword:= TWordSegment.Create;
      DFS (ActiveSubword, Ptr, i);
      Add (ActiveSubword);
      
    end;
    Inc (Ptr);
    
  end;

//  PrepareForRecognition;
  
end;

function TLine.GetWordSegment(Index: Integer): TWordSegment;
begin
  Result:= FAllSubwords.Member [Index] as TWordSegment;
  
end;

function TWordSegmentCollection.GetWordSegment (
  Index: Integer): TWordSegment;
begin
  Result:= Member [Index] as TWordSegment;
  
end;

procedure TWordSegmentCollection.PrepareForRecognition;
var
  Ptr: PObject;
  i: Integer;

begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    TWordSegment (Ptr^).ImageForRecognition;
  end;

end;

end.
