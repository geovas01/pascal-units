unit ComponentsUnit;

interface
uses
  HashUnit, Graphics, SysUtils, GeometryUnit, CollectionUnit;

type
  TDirection= (dN= 0, dNE, dE, dSE, dS, dSW, dW, dNW);
  TRGB= record
    r, g, b: Byte;
    Color: TColor;
  end;


const
  BlackRGB: TRGB= (r: 0; g: 0; b: 0; Color: $000);

type
  THSI= record
    h, i: Integer;
    s: Extended;
  end;


  TMyPixel= class
  private
    FLocation: TPoint;
    FRGBColor: TRGB;
  public
    function GetHSIColor: THSI;
    procedure SetLocation (const Value: TPoint);
    property Location: TPoint read FLocation write SetLocation;
    property RGBColor: TRGB read FRGBColor write FRGBColor;
    property HSIColor: THSI read GetHSIColor;

    function ToString: String;
    constructor Create;overload;
    constructor Create (Point: TPoint; Color: TRGB);overload;
    destructor Destroy; override;
    
  end;

  THash= class
    private
      FHashFunctionType: THashFunctionType;

      procedure SetHashFunctionType(const Value: THashFunctionType);

      function HashData (const Argument: TPoint): Int64;overload;
      function HashData (const x, y: Integer): Int64;overload;
    public
      Data: array of array of TPoint;
      
      property HashFunctionType: THashFunctionType read FHashFunctionType write SetHashFunctionType;

      function IsExist (const Argument: TPoint): Boolean; overload;
      function IsExist (const r, c: Integer): Boolean; overload;

      function Insert (const Argument: TPoint): Boolean; overload;
      function Insert (const r, c: Integer): Boolean; overload;
      procedure SaveToFile (FileName: String= 'Hashlog.txt');

      constructor Create;overload;
      constructor Create (HashFuctionType: THashFunctionType);overload;
      procedure Free;
      procedure Clear;
  end;

  PComponent= ^TComponent;
  TComponent= class (TBaseCollection)
  private
//    Pixels: array of TMyPixel;
    FMaxR, FMaxC: Integer;
    FMinR, FMinC: Integer;

    SumOfR, SumOfG, SumOfB: Integer;
    FCollectionColor: TRGB;

    LeftestPoint, RightestPoint,
    TopestPoint, MostBottonPoint: Integer;
    FID: Integer;
    FCenterOfMass: TPoint;

    function GetIndex (r, c: Integer): Integer;
    function GetCount: Integer;
    procedure FindBoundraryPoints;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPercentage: Integer;
    function GetPixels (Index: Integer): TMyPixel;
    
  public
    HashedData: THash;
    property ID: Integer read FID;
    property Count: Integer read GetCount;
    property CollectionColor: TRGB read FCollectionColor;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Percentage: Integer read GetPercentage;
    property MinR: Integer read FMinR;
    property MaxR: Integer read FMaxR;
    property MinC: Integer read FMinC;
    property MaxC: Integer read FMaxC;
    property CenterOfMass: TPoint read FCenterOfMass;
    property Pixels [Index: Integer]: TMyPixel read GetPixels;

    constructor Create (ID: Integer= -1);
    destructor Destroy; override;

    function IsExists (x, y: Integer): Boolean;overload;
    function IsExists (Point: TPoint): Boolean;overload;
    procedure Add (r, c: Integer);overload;
    procedure Add (r, c: Integer; RGBColor: TRGB);overload;
    procedure Add (Point: TPoint; RGBColor: TRGB);overload;
    procedure Delete (r, c: Integer);overload;
    procedure Delete (Index: Integer);overload;
    function  GetPixel (Index: Integer): TMyPixel;overload;
    function  CountInSameLine: Extended;

    procedure SaveToFile (FileName: String);
    procedure AddToFile (var FileHandle: TextFile);

    function GetTopLeftPoint: TPoint;
    function GetTopRightPoint: TPoint;
    function GetDownLeftPoint: TPoint;
    function GetDownRightPoint: TPoint;

    function GetMinimum: TPoint;
    function GetMaximum: TPoint;
    function GetEffectiveLength: Integer;
    function GetEffectiveWidth: Integer;
    function GetArea: Integer;

    function ExtractContour: TComponent;
    procedure Merge (AnotherComponent: TComponent);

    (* This method can be implemented faster by finding the convex hall of
    the components*)
    function FindMinDistToComp (AnotherComponent: TComponent): Extended;
    (* This method can be implemented faster by finding the convex hall of
    the components*)
    function FindMinDistToPoint (APoint: TPoint): Extended;
    (*Returns the difference  between the rows of two points which are nearest to each other*)
    function FindMinHorizDistClosestPointsToComp (AnotherComponent: TComponent): Integer;

    procedure Move (Delta: TPoint); overload;
    procedure Move (Deltar, Deltac: Integer); overload;

    procedure Allocate (NewSize: Integer);
    
  end;

implementation

uses ExceptionUnit, MyTypes;

{ TMyPixel }

constructor TMyPixel.Create;
begin
  inherited;
  
  Location:= nil;
end;

constructor TMyPixel.Create (Point: TPoint; Color: TRGB);
begin
  inherited Create;
  
  Location:= Point;
  RGBColor:= Color;

end;

destructor TMyPixel.Destroy;
begin
  FLocation.Free;

  inherited;

end;

function TMyPixel.GetHSIColor: THSI;
begin
  raise ENotImplemented.Create ('');
//  Result:= Conver
end;

procedure TMyPixel.SetLocation (const Value: TPoint);
begin
  FLocation:= Value;
end;

function TMyPixel.ToString: String;
begin
  Result:= Location.ToString;//+...??!!

end;

{ TComponent }

procedure TComponent.Add (r, c: Integer; RGBColor: TRGB);
begin
  if IsExists (r, c) then
    Exit;

//  SetLength (Pixels, Length (Pixels)+ 1);
//  Pixels [High (Pixels)]:= TMyPixel.Create (TPoint.Create (r, c), RGBColor);
  inherited Add (TMyPixel.Create (TPoint.Create (r, c), RGBColor));

  FCenterOfMass.Move (r, c);

  HashedData.Insert (r, c);

  if r< MinR then
    FMinR:= r;
  if MaxR< r then
    FMaxR:= r;

  if c< Minc then
    FMinC:= c;
  if MaxC< c then
    FMaxC:= c;
    
  Inc (SumOfR, RGBColor.r);
  Inc (SumOfG, RGBColor.g);
  Inc (SumOfB, RGBColor.b);
  
  FCollectionColor.r:= SumOfR div Count;
  FCollectionColor.g:= SumOfG div Count;
  FCollectionColor.b:= SumOfB div Count;
  
end;

procedure TComponent.Add (Point: TPoint; RGBColor: TRGB);
begin
  Add (Point.r, Point.c, RGBColor);
  
end;

procedure TComponent.Add (r, c: Integer);
begin
  if IsExists (r, c) then
    Exit;
    
//  SetLength (Pixels, Length (Pixels)+ 1);
//  Pixels [High (Pixels)]:= TMyPixel.Create (TPoint.Create (r, c), BlackRGB);
  inherited Add (TMyPixel.Create (TPoint.Create (r, c), BlackRGB));  
  
  FCenterOfMass.Move (r, c);
  HashedData.Insert (r, C);

  if r< MinR then
    FMinR:= r;
  if MaxR< r then
    FMaxR:= r;

  if c< MinC then
    FMinC:= c;
  if MaxC< c then
    FMaxC:= c;
    
end;

procedure TComponent.AddToFile (var FileHandle: TextFile);
var
  i: Integer;
  
begin
  WriteLn (FileHandle, CollectionColor.r, ' ',
      CollectionColor.g, ' ',
      CollectionColor.b, ' ');
       
  for i:= 0 to Count- 1 do
    WriteLn (FileHandle, (Member [i] as TMyPixel) .ToString);
  
end;

function TComponent.CountInSameLine: Extended;
var
  SameLineCount,
  i: Integer;
begin
  SameLineCount:= 0;

  for i:= 0 to Count- 2 do
  begin
    if ((Member [i] as TMyPixel).FLocation.r- (Member [i+ 1] as TMyPixel).FLocation.r= 0) or
     ((Member [i] as TMyPixel).FLocation.c- (Member [i+ 1] as TMyPixel).FLocation.c= 0) then
      Inc (SameLineCount);
      
  end;

  if ((Member [Count- 1] as TMyPixel).FLocation.r- (Member [0] as TMyPixel).FLocation.r= 0) or
   ((Member [Count- 1] as TMyPixel).FLocation.r- (Member [0] as TMyPixel).FLocation.c= 0) then
    Inc (SameLineCount);

  Result:= SameLineCount/ Count; 
end;

constructor TComponent.Create (ID: Integer= -1);
begin
  inherited Create;
  
  FMaxC:= -1; FMaxR:= -1;
  FMinC:= 100000; FMinR:= 1000000;
  SumOfR:= 0;  SumOfG:= 0; SumOfB:= 0;

  TopestPoint:= -1;
  LeftestPoint:= -1;
  RightestPoint:= -1;
  MostBottonPoint:= -1;
  
  HashedData:= THash.Create;
  FID:= ID;
  FCenterOfMass:= TPoint.Create (0, 0);

end;

procedure TComponent.Delete (r, c: Integer);
var
  Index, i: Integer;
  
begin
  Index:= GetIndex (r, c);
  if Index<> -1 then
    inherited Delete (Index);

end;

procedure TComponent.Delete (Index: Integer);
begin
  if Index<> -1 then
    inherited Delete (Index);

end;

function TComponent.ExtractContour: TComponent;
const
  AdjancedPixelr: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
  AdjancedPixelc: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
  
var
  RGBColor: TRGB;
  r, c: Integer;
  StartDir: TDirection;
  i, j: Integer;
  
begin
  try
    c:= MinC; r:= MinR;
    
    Result:= TComponent.Create;
    for i:= FMinR to FMaxR do
      for j:= FMinC to FMaxC do
        if HashedData.IsExist (i, j) then
        begin
          r:= i;
          c:= j;
          Break;
          
        end;


    RGBColor.r:= 0; RGBColor.g:= 0; RGBColor.b:= 0;

    StartDir:= dE;

    while not Result.HashedData.IsExist (r, c) do
    begin
      Result.Add (r, c, RGBColor);
    
      StartDir:= TDirection ((Ord (StartDir)+ 5) mod 8);

      for i:= 0 to 7 do
      begin
        if HashedData.IsExist (r+ AdjancedPixelR [StartDir], c+ AdjancedPixelC [StartDir]) then
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

procedure TComponent.FindBoundraryPoints;
var
  Temp,
  MaxFind: Integer;
  Max, Min: TPoint;
  r, c: Integer;
  
begin
  Max:= GetMaximum;
  Min:= GetMinimum;

  if TopestPoint= -1 then
  begin
    MaxFind:= -1;
    for r:= Min.r to Max.r do
    begin
      Temp:= 0;
      for c:= Min.c to Max.c do
        if HashedData.IsExist (r, c) then
          Inc (Temp);

      if MaxFind< Temp then
      begin
        MaxFind:= Temp;
        TopestPoint:= r;
        
      end;
      if 2* (Max.c- Min.c)< 3* MaxFind then
        Break;
        
    end;
    
  end;

  if MostBottonPoint= -1 then
  begin
    MaxFind:= -1;
    
    for r:= Max.r downto Min.c do
    begin
      Temp:= 0;
      for c:= Min.c to Max.c do
        if HashedData.IsExist (r, c) then
          Inc (Temp);

      if MaxFind< Temp then
      begin
        MaxFind:= Temp;
        MostBottonPoint:= r;
        
      end;
      
      if 2* (Max.c- Min.c)< 3* MaxFind then
        Break;

    end;
    
  end;

  if LeftestPoint= -1 then
  begin
    MaxFind:= -1;
    for c:= Min.c to Max.c do
    begin
      Temp:= 0;
      for r:= Min.r to Max.r do
        if HashedData.IsExist (r, c) then
          Inc (Temp);

      if MaxFind< Temp then
      begin
        MaxFind:= Temp;
        LeftestPoint:= c;
        
      end;
      
      if 2* (Max.r- Min.r)< 3* MaxFind then
        Break;
        
    end;

  end;

  if RightestPoint= -1 then
  begin
    MaxFind:= -1;
    for c:= Max.c to Min.c do
    begin
      Temp:= 0;
      for r:= Min.r to Max.r do
        if HashedData.IsExist (r, c) then
          Inc (Temp);

      if MaxFind< Temp then
      begin
        MaxFind:= Temp;
        RightestPoint:= c;
        
      end;
      if 2* (Max.r- Min.r)< 3* MaxFind then
        Break;
        
    end;
    
  end;

  Max.Free;
  Min.Free;
  
end;

destructor TComponent.Destroy;
begin
  HashedData.Free;
  FCenterOfMass.Free;

  inherited;
  
end;

function TComponent.GetArea: Integer;
begin
  Result:= (MaxR- MinR+ 1)* (MaxC- MinC+ 1);
   
end;

function TComponent.GetCount: Integer;
begin
  Result:= Size;
  
end;

function TComponent.GetDownLeftPoint: TPoint;
begin
  FindBoundraryPoints;
  Result:= TPoint.Create (LeftestPoint, MostBottonPoint);
end;

function TComponent.GetDownRightPoint: TPoint;
begin
  FindBoundraryPoints;
  Result:= TPoint.Create (RightestPoint, MostBottonPoint);
  
end;

function TComponent.GetEffectiveLength: Integer;
var
  DiffX: array of Integer;
  TotalTilNow,
  CountDiv2,
  i: Integer;
  Min,
  Max: TPoint;
  
begin
  Min:= GetMinimum;
  Max:= GetMaximum;
  SetLength (DiffX, Max.c- Min.c+ 1);

  for i:= 0 to Max.c- Min.c do
    DiffX [i]:= 0;
  for i:= 0 to Count- 1 do
    Inc (DiffX [Pixels [i].Location.c- Min.c]);

  TotalTilNow:= 0;
  CountDiv2:= Count div 2;
  Result:= -1;
  for i:= 0 to Max.c- Min.c do
  begin
    Inc (TotalTilNow, DiffX [i]);
    if CountDiv2< TotalTilNow then
    begin
      Result:= Diffx [i];
      Break;
      
    end;
    
  end;

  Max.Free;
  Min.Free;
  SetLength (Diffx, 0);
  
end;

function TComponent.GetEffectiveWidth: Integer;
var
  DiffY: array of Integer;
  TotalTilNow,
  CountDiv2,
  i: Integer;
  Min,
  Max: TPoint;
begin
  Min:= GetMinimum;
  Max:= GetMaximum;
  SetLength (DiffY, Max.r- Min.r+ 1);

  for i:= 0 to Max.r- Min.r do
    DiffY [i]:= 0;
  for i:= 0 to Count- 1 do
    Inc (DiffY [Pixels [i].Location.r- Min.r]);

  TotalTilNow:= 0;
  CountDiv2:= Count div 2;
  Result:= -1;
  for i:= 0 to Max.r- Min.r do
  begin
    Inc (TotalTilNow, DiffY [i]);
    if CountDiv2< TotalTilNow then
    begin
      Result:= Diffy [i];
      Break;

    end;
    
  end;

  Max.Free;
  Min.Free;
  SetLength (Diffy, 0);
end;

function TComponent.GetHeight: Integer;
begin
  Result:= MaxR- MinR+ 1;
  
end;

function TComponent.GetIndex (r, c: Integer): Integer;
var
  i: Integer;
  
begin
  Result:= -1;
  for i:= 0 to Size- 1 do
    if (Pixels [i].Location.r= r) and
     (Pixels [i].Location.c= c) then
    begin
       Result:= i;
       Exit;
       
    end;
    
end;

function TComponent.GetMaximum: TPoint;
begin
  Result:= TPoint.Create (MaxR, MaxC);

end;

function TComponent.GetMinimum: TPoint;
begin
  Result:= TPoint.Create (MinR, MinC);
  
end;

function TComponent.GetPixel (Index: Integer): TMyPixel;
begin
  if Index<> -1 then
    Result:= Pixels [Index]
 else
   raise ERangeCheckError.Create ('GetPixel');
   
end;

function TComponent.GetTopLeftPoint: TPoint;
begin
  FindBoundraryPoints;
  Result:= TPoint.Create (LeftestPoint, TopestPoint);
  
end;

function TComponent.GetTopRightPoint: TPoint;
begin
  FindBoundraryPoints;
  Result:= TPoint.Create (RightestPoint, TopestPoint);
  
end;

function TComponent.IsExists (x, y: Integer): Boolean;
begin
//  Result:= GetIndex (x, y)<> -1;
  Result:= HashedData.IsExist (x, y);
{
  if Result<> (GetIndex (x, y)<> -1) then
  begin
    Result:= HashedData.IsExists (x, y);
    Result:= GetIndex (x, y)<> -1;
    Result:= not Result;
  end;
}

end;

function TComponent.GetWidth: Integer;
begin
  Result:= MaxC- MinC+ 1;
  
end;

function TComponent.IsExists (Point: TPoint): Boolean;
begin
  Result:= IsExists (Point.r, Point.c);
  
end;

function TComponent.GetPercentage: Integer;
begin
  Result:= 100* Count div (Width* Height);
  
end;

function TComponent.FindMinDistToComp (AnotherComponent: TComponent): Extended;
var
  i: Integer;
  Temp: Extended;

begin
  Result:= 1e10;
  
  if AnotherComponent.Count= 0 then
    Result:= 0
  else
    for i:= 0 to AnotherComponent.Count- 1 do
    begin
      Temp:= FindMinDistToPoint (AnotherComponent.Pixels [i].FLocation);
      
      if Temp< Result then
        Result:= Temp;

    end;

end;

function TComponent.FindMinDistToPoint (APoint: TPoint): Extended;
var
  i: Integer;
  Temp: Extended;

begin
  Result:= 1e10;

  if Self.Count= 0 then
    Result:= 0
  else
    for i:= 0 to Self.Count- 1 do
    begin
      Temp:= APoint.CalcDistance (Self.Pixels [i].Location);

      if Temp< Result then
        Result:= Temp;

    end;

end;

procedure TComponent.Merge (AnotherComponent: TComponent);
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

      Self.Add (Point.r, Point.c);

    end;

  end;

end;

procedure TComponent.Move (Delta: TPoint);
var
  i: Integer;
  ActiveMyPix: TMyPixel;

begin
{
  HashedData.Free;
  HashedData:= THash.Create;
}
  for i:= 0 to Count- 1 do
  begin
    ActiveMyPix:= Pixels [i];
    ActiveMyPix.FLocation.Move (Delta);
    HashedData.Insert (ActiveMyPix.FLocation.Copy);

  end;

  Inc (FMaxC, Delta.c);
  Inc (FMaxR, Delta.r);
  Inc (FMinC, Delta.c);
  Inc (FMinR, Delta.r);
  FCenterOfMass.Scale (1/ Count).Move (Delta).Scale (Count);

end;

procedure TComponent.SaveToFile (FileName: String);
var
  FileHandle: TextFile;
  
begin
  AssignFile (Filehandle, FileName);
  Rewrite (FileHandle);

  AddToFile (FileHandle);

  CloseFile (FileHandle);

end;

procedure TComponent.Move (Deltar, Deltac: Integer);
var
  i: Integer;
  ActiveMyPix: TMyPixel;
  Point: TPoint;
//  MyPixPtr: ^TMyPixel;

begin
{
  HashedData.Free;
  HashedData:= THash.Create;
}
  for i:= 0 to Count- 1 do
  begin
    Point:= Pixels [i].Location;
    Point.Move (Deltar, Deltac);

    HashedData.Insert (Point.Copy);

  end;

  Inc (FMaxC, Deltac);
  Inc (FMaxR, Deltar);
  Inc (FMinC, Deltac);
  Inc (FMinR, Deltar);
  FCenterOfMass.Scale (1/ Count).Move (Deltar, Deltac).Scale (Count);

end;

function TComponent.FindMinHorizDistClosestPointsToComp(
  AnotherComponent: TComponent): Integer;
var
  Ptr1, Ptr2: PObject;
  i, j, c1, c2, r1, r2: Integer;
  Temp, MinDist: Integer;
  SelMaxC, SelMinC,
  MinCOth, MaxCOth: Integer;

begin
  Result:= 10000;

  MaxCOth:= AnotherComponent.MaxC+ 2;
  MinCOth:= AnotherComponent.MinC- 2;
  SelMaxC:= MaxC+ 2;
  SelMinC:= MinC- 2;

  if AnotherComponent.Count= 0 then
    Result:= 0
  else
  begin
    Ptr1:= AnotherComponent.GetPointerToFirst;
    for i:= 0 to AnotherComponent.Count- 1 do
    begin
      c1:= TMyPixel (Ptr1^).Location.c;
      r1:= TMyPixel (Ptr1^).Location.r;
      Ptr2:= Self.GetPointerToFirst;

      if (SelMinC<= c1) and (c1<= SelMaxC) then
        for j:= 0 to Count- 1 do
        begin
          c2:= TMyPixel (Ptr2^).Location.c;

          if (MinCOth<= c2) and (c2<= MaxCOth) then
          begin
            r2:= TMyPixel (Ptr2^).Location.r;
            
            if Abs (r1- r2)< Result then
              Result:= Abs (r1- r2);

          end;
          
          Inc (Ptr2);

        end;

      Inc (Ptr1);
      
    end;
    
  end;
  
end;

function TComponent.GetPixels (Index: Integer): TMyPixel;
begin
  Result:= Member [Index] as TMyPixel;
  
end;

procedure TComponent.Allocate (NewSize: Integer);
begin
  inherited Allocate (NewSize);
end;

{ THash }

procedure THash.Clear;
var
  i, j: Integer;
  
begin
  for i:= 0 to High (Data) do
  begin
    for j:= 0 to High (Data [i]) do
      Data [i][j].Free;

    SetLength (Data [i], 0);

  end;
  SetLength (Data, 0);
  
end;

constructor THash.Create;
begin
  inherited;
  
  FHashFunctionType:= hftMod;
  SetLength (Data, HashFunctionMod);
  
end;

constructor THash.Create (HashFuctionType: THashFunctionType);
begin
  inherited Create;
  
  FHashFunctionType:= HashFuctionType;
  SetLength (Data, HashFunctionMod);
  
end;

procedure THash.Free;
begin
  Clear;

  inherited;
  
end;

function THash.HashData (const Argument: TPoint): Int64;
begin
  Result:= 0;

  case HashFunctionType of
    hftMod:
      Result:= (Argument.r+ Argument.c) mod HashFunctionMod;
      
  end;
end;

function THash.Insert (const Argument: TPoint): Boolean;
var
  Pos: Integer;
begin
  Result:= IsExist (Argument);
  if Result then
    Exit;

  Pos:= HashData (Argument);
  SetLength (Data [Pos], Length (Data [Pos])+ 1);
  Data [Pos][High (Data [Pos])]:= Argument;
  
end;

function THash.IsExist (const Argument: TPoint): Boolean;
var
  Pos, i: Integer;
begin
   Pos:= HashData (Argument);
   Result:= False;

   for i:= 0 to High (Data [Pos]) do
   begin

     if Data [Pos][i].IsSame (Argument) then
     begin
       Result:= True;
       Exit;

     end;
     
   end;

end;

function THash.Insert (const r, c: Integer): Boolean;
var
  Pos: Integer;
  
begin
  Result:= IsExist (r, c);
  if Result then
    Exit;

  Pos:= HashData (r, c);
  SetLength (Data [Pos], Length (Data [Pos])+ 1);
  Data [Pos][High (Data [Pos])]:= TPoint.Create (r, c);
  
end;

function THash.IsExist (const r, c: Integer): Boolean;
var
  Pos, i: Integer;
  Ptr: ^TPoint;
   
begin
   Pos:= HashData (r, c);
   Ptr:= @Data [Pos, 0];
   Result:= False;

   for i:= 0 to High (Data [Pos]) do
   begin

     if Ptr^.IsSame (r, c) then
     begin
       Result:= True;
       Exit;

     end;

     Inc (Ptr);

   end;

end;

procedure THash.SetHashFunctionType (const Value: THashFunctionType);
begin
  Clear;

  FHashFunctionType:= Value;
end;

function THash.HashData (const x, y: Integer): Int64;
begin
  Result:= 0;

  case HashFunctionType of
    hftMod:
      Result:= (x+ y) mod HashFunctionMod;
  end;

end;

procedure THash.SaveToFile (FileName: String);
var
  i, j: Integer;
  Output: TextFile;
begin
  AssignFile (Output, FileName);
  ReWrite (Output);
  for i:= 0 to High (Data) do
  begin
    for j:= 0 to High (Data [i]) do
      WriteLn (Output, i, ':', Data [i][j].ToString);
      
  end;

  CloseFile (Output);
end;

end.

