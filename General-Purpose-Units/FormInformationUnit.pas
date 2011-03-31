{﻿}unit FormInformationUnit;

interface
uses
  SysUtils, Graphics, Types, FMLImage, GeometryUnit,
  ComponentsUnit;
  
const
  BlackThr: Integer= 20;
  
type
  TArrBitmap= array of TBitmap;

  PointerArray= array of Pointer;
  
  TMyBitmap= class
  private
//{$UNSAFECODE ON$}
    Pixels: PointerArray;
//{$UNSAFECODE OFF$}
    IncValue: Integer;
    function ConvertToRGB (Color: TColor): TRGB;

  public
    Width: Integer;
    Height: Integer;
    function Body (x, y: Integer): TRGB;
//{$UNSAFECODE ON$}
    function GetPixels: Pointer;//unsafe;
//{$UNSAFECODE OFF$}
    constructor Create (Bitmap: TBitmap);
    procedure Free;
  end;

  EFormBoxRangeCheckError= class (Exception);

  TBox= class (TObject)
  private
    FUpperLeftPoint: TPoint;
    FUpperRightPoint: TPoint;
    FLowerLeftPoint: TPoint;
    FLowerRightPoint: TPoint;
    FTitle: WideString;

    procedure Complete;//Change the place of UpperLeftPoint and LowerRightPoint if needed
    procedure SetLowerRightPoint (const Value: TPoint);
    procedure SetUpperLeftPoint  (const Value: TPoint);
  public
    property UpperLeftPoint: TPoint read FUpperLeftPoint write SetUpperLeftPoint;
    property LowerRightPoint: TPoint read FLowerRightPoint write SetLowerRightPoint;
    property Title: WideString read FTitle;

    function Copy: TBox;
    procedure SaveToFile (var OutputFile: TextFile);
    procedure LoadFromFile (var InputFile: TextFile; LoadTitle: Boolean= False);
    function IsSame (Box: TBox): Boolean;
    function Rotate (Angle: Extended): TBox;
    function Move (Delta: TPoint): TBox;

    constructor Create;
    procedure Free;
  end;
  
  TBoxArray= array of TBox;
  
  TBoxData= class (TObject)
  private
    IsValid: Boolean;
//    FBox: TBox;
    FBoxArray: TBoxArray;
    FKind: TInputKind;
    FPostProcessorFileName: String;
    FTitle: WideString;
    FNumberOfElement: Integer;
    FRotateAngle: Extended;
    FSource: TPoint;

  public
//    property Box: TBox read FBox;
    property BoxArray: TBoxArray read FBoxArray;
    property NoOfElement: Integer read FNumberOfElement;

    property Kind: TInputKind read FKind;
    property PostProcessorFile: String read FPostProcessorFileName;
    property Title: WideString read FTitle;

    property Source: TPoint read FSource write FSource;
    property RotateAngle: Extended read FRotateAngle write FRotateAngle;
     
    function IsSame (Box: TBoxData): Boolean;
    function Copy: TBoxData;
    procedure LoadFromFile (var InputFile: TextFile);
    procedure SaveToFile (var OutputFile: TextFile);
    procedure DrawInBitmap (BitmapImage: TBitmap);
    function ExtractFromBitmap (BitmapImage: TBitmap): TImageCollection;

    constructor Create;overload;
    constructor Create (Box: TBox; InputKind: TInputKind);overload;
    procedure Free;
  end;

  TCompType= (ctStart, ctFirstName, ctLastName, ctCity, ctState, ctInteger, ctSingleChoice,
    ctEnd);
  TStringArray= array of String;
  TWideStringArray= array of WideString;
  TWideStringArrayOfArray= array of array of WideString;
  PColor= ^TColor;
  PByte= ^Byte;

  TDataComponent= class
  private
    FCount: Integer;
    Boxes: array of Integer;
    FCompType: TCompType;
    FName: String;
    function GetBoxByIndex (Index: Integer): Integer;
  public
    DataComponent: TCompType;
    property Count: Integer read FCount;
    property Box [Index: Integer]: Integer read GetBoxByIndex;
    property Name: String read FName;

    constructor Create (Name: String);
    procedure Free;

    procedure SaveToFile (var FileHandle: TextFile);
    procedure LoadFromFile (var FileHandle: TextFile);
    procedure AddBox (BoxIndex: Integer);
    procedure DeleteBox (Index: Integer);
  end;

  TField= class (TObject);
  TCheckBoxField= class (TObject);
  
  TFormsBox= class (TObject)
  private
    MinX, MaxX: Integer;
    MinY, Maxy: Integer;
    FWidth,
    FHeight: Integer;
    FAllBoxes: array of TBoxData;
    FIsLastChangeSaved: Boolean;
    FFileName: String;
    Components: array of TDataComponent;
    FFormID: Integer;
    FSaveResultsAsBitmaps: Boolean;
    DataPath: String;
    BitmapImage: TBitmap;

    function GetBoxes (Index: Integer): TBoxData;
    function GetBoxCount: Integer;
    procedure SetFileName (const Value: String);
    function GetHeight: Integer;
    function GetWidth: Integer;

//    function DeleteNoise (MyBitmapImage: TMyBitmap; NoiseColor: TRGB): TBitmap;

    function ConvertToHSI (Color: TColor): THSI;
    function ConvertToRGB (Color: TColor): TRGB;

    function FindComponent (MyBitmap: TMyBitmap; Box: TBox; HelpBarColor: TRGB; ContinueSearchOverBorders: Integer= 1): TComponentCollection;
    function FindHelpBar (MyBitmap: TMyBitmap; Box: TBox; HelpBarColor: TRGB; ContinueSearchOverBorders: Integer= 1): TComponentCollection;
    function GetNumberOfCheckBoxFeilds: Integer;
    function GetNumberOfNumeralFeilds: Integer;
    function GetNumberOfAlphabeticFeilds: Integer;
    function GetPostprocessingTypeOfAlphabeticFeilds: TStringArray;
    function GetFeildsTitles: TWideStringArray;
    function GetCheckBoxOptionTitles: TWideStringArrayOfArray;
    procedure SetSaveResultsAsBitmaps(const Value: Boolean);
    function  ExtractBox (BitmapImage: TBitmap; UseDialation: Boolean= True): TImageCollection;
    function GetAlphaNumericalFieldsArray(Index: Integer): TField;
    function GetCheckBoxFieldsArray(Index: Integer): TCheckBoxField;
    function FindSkewAndReplacenent (Bitmap: TBitmap): Boolean;

  public
    property Boxes [Index: Integer]: TBoxData read GetBoxes;
    property BoxNumber: Integer read GetBoxCount;
    property IsLastChangeSaved: Boolean read FIsLastChangeSaved;
    property FileName: String read FFileName write SetFileName;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property FormID: Integer read FFormID;
    property NumberOfCheckBoxFeilds: Integer read GetNumberOfCheckBoxFeilds;
    property NumberOfNumeralFeilds: Integer read GetNumberOfNumeralFeilds;
    property NumberOfAlphabeticFeilds: Integer read GetNumberOfAlphabeticFeilds;
		property PostprocessingTypeOfAlphabeticFeilds: TStringArray
              read GetPostprocessingTypeOfAlphabeticFeilds;
    property FeildsTitles: TWideStringArray read GetFeildsTitles;
    property CheckBoxOptionTitles: TWideStringArrayOfArray  read GetCheckBoxOptionTitles;
    property SaveResultsAsBitmaps: Boolean read FSaveResultsAsBitmaps write SetSaveResultsAsBitmaps;
    property AlphaNumericalFieldsArray [Index: Integer]: TField read GetAlphaNumericalFieldsArray;
    property CheckBoxFieldsArray [Index: Integer]: TCheckBoxField read GetCheckBoxFieldsArray;

    function  AddBox (Box: TBoxData): Boolean;
    procedure AddComponent (DataComponent: TDataComponent);
    function  DeleteBox (Index: Integer): Boolean;
    procedure SaveToFile (FileName: String);overload;
    procedure SaveToFile;overload;
    procedure LoadConfigFile (FileName: String);overload;
    procedure LoadFromFile;overload;
    
    function LoadTheForm (FileName: String): TBitmap;overload;
    function LoadTheForm (Bitmap: TBitmap): TBitmap;overload;
    
    procedure DrawInBitmap (BitmapImage: TBitmap);

    procedure ExtractAllBox;

    function  GenerateOutput (FormImage: TBitmap; Data: array of Integer): TBitmap;overload;

//    procedure GenerateHtml (Data: array of Integer);
    constructor Create; overload;
    constructor Create (DataPath: String); overload;
    procedure Free;
  end;


  function InputKindToString (Value: TInputKind): String;
  function ComponentTypeToString (Value: TCompType): String;
  function ColorIsNotWhite (Color: TColor): Boolean;
  function ColorsAreTheSame (Color1, Color2: TRGB): Boolean;
  function ColorsAreInSameRange (Color1, Color2: TRGB): Boolean;
const
  WidthSearchThreshld= 5;
  HeightarchThreshld= 5;
  function CheckForLine (x, y: Integer; BitmapImage: TBitmap;
  WidthCheck: Integer= WidthSearchThreshld;
  HeightCheck: Integer= HeightarchThreshld): Boolean;

  function FindTopRightPointInLine (x, y: Integer; BitmapImage: TBitmap): TPoint;
  function FindTopLeftPointInLine (x, y: Integer; BitmapImage: TBitmap): TPoint;
  function FindBottomRightPointInLine (x, y: Integer; BitmapImage: TBitmap): TPoint;
  function FindBottomLeftPointInLine (x, y: Integer; BitmapImage: TBitmap): TPoint;


implementation
uses
  Dialogs, Math{, Borland.Vcl.ComCtrls, Borland.Vcl.Grids, Borland.Vcl.Controls,};

function ComponentTypeToString (Value: TCompType): String;
begin
  case value of
    ctFirstName:
      Result:= 'First Name';
    ctLastName:
      Result:= 'Last Name';
    ctCity:
      Result:= 'City';
    ctState:
      Result:= 'State';
    ctInteger:
      Result:= 'Integer';
    ctSingleChoice:
      Result:= 'Single Choice Box';
  end;
end;
function InputKindToString (Value: TInputKind): String;
begin
  case Value of
    ikNumeral:
      Result:= 'Numercial';
    ikAlphabet:
      Result:= 'Alphabet';
    ikCheckBox:
      Result:= 'CheckBox';
    ikPicture:
      Result:= 'Picture';
    ikHelpBar:
      Result:= 'HelpBar';
  end;
end;

{ TBox }

procedure TBox.Complete;
var
  MaxY, MinY, MaxX, MinX: Integer;
begin
  try
    if (LowerRightPoint= nil) or (UpperLeftPoint= nil) then
      Exit;

    MinY:= LowerRightPoint.y;
    MaxY:= UpperLeftPoint.y;
    MaxX:= LowerRightPoint.x;
    MinX:= UpperLeftPoint.x;
    if MaxY< MinY then
    begin
      MinY:= MinY xor MaxY;
      MaxY:= MinY xor MaxY;
      MinY:= MinY xor MaxY;
    end;
    if MaxX< MinX then
    begin
      MinX:= MinX xor MaxX;
      MaxX:= MinX xor MaxX;
      MinX:= MinX xor MaxX;
    end;
    
    LowerRightPoint.x:= MaxX;
    LowerRightPoint.y:= MaxY;
    
    UpperLeftPoint.x:= MinX;
    UpperLeftPoint.y:= MinY;

  except
    on EPointIsNotInitialized do
      Exit;
  end;
end;

function TBox.Copy: TBox;
begin
  Result:= TBox.Create;
  Result.UpperLeftPoint:= Self.UpperLeftPoint;
  Result.LowerRightPoint:= Self.LowerRightPoint;
  Result.UpperLeftPoint:= Self.UpperLeftPoint;
  Result.LowerRightPoint:= Self.LowerRightPoint;
end;

constructor TBox.Create;
begin
  inherited;

  FTitle:= '';
  Self.UpperLeftPoint:= nil;
  Self.LowerRightPoint:= nil;
  Self.UpperLeftPoint:= nil;
  Self.LowerRightPoint:= nil;
end;

procedure TBox.Free;
begin
  if FLowerRightPoint<> nil then
    FLowerRightPoint.Free;
    
  if FUpperLeftPoint<> nil then
    FUpperLeftPoint.Free;

  if FLowerLeftPoint<> nil then
    FLowerLeftPoint.Free;
    
  if FUpperRightPoint<> nil then
    FUpperRightPoint.Free;
  inherited;
end;

function TBox.IsSame (Box: TBox): Boolean;
begin
  Result:= Self.UpperLeftPoint.IsSame (Box.UpperLeftPoint) and
           Self.LowerRightPoint.IsSame (Box.LowerRightPoint);
end;

procedure TBox.LoadFromFile(var InputFile: TextFile; LoadTitle: Boolean= False);
var
  Left, Top, Width, Heigth: Integer;
begin

  Readln (InputFile, Left);
  Readln (InputFile, Top);
  Readln (InputFile, Width);
  Readln (InputFile, Heigth);

  if LoadTitle then
    Readln (InputFile, FTitle);
    
  FUpperLeftPoint:= TPoint.Create (Left, Top);
  FLowerRightPoint:= TPoint.Create (Left+ Width, Top+ Heigth);
  
  Readln (InputFile);
end;

function TBox.Move (Delta: TPoint): TBox;
begin
  FLowerRightPoint.Move (Delta);
  FUpperLeftPoint.Move (Delta);
  FLowerLeftPoint.Move (Delta);
  FUpperRightPoint.Move (Delta);
  Result:= Self;
end;

function TBox.Rotate (Angle: Extended): TBox;
begin
  LowerRightPoint.Rotate (Angle);
  UpperLeftPoint.Rotate (Angle);
  Result:= Self;
end;

procedure TBox.SaveToFile (var OutputFile: TextFile);
begin
  WriteLn (OutputFile, UpperLeftPoint.ToString);
  WriteLn (OutputFile, LowerRightPoint.ToString);
end;

procedure TBox.SetLowerRightPoint (const Value: TPoint);
begin
  FLowerRightPoint:= Value;
  Complete;
end;

procedure TBox.SetUpperLeftPoint (const Value: TPoint);
begin
  FUpperLeftPoint:= Value;
  Complete;
end;

{ TBoxData }

function TBoxData.Copy: TBoxData;
begin
  raise Exception.Create ('Not Implemented Yet!');
  {
  Result:= TBoxData.Create;
//  Result.FBox:= Self.Box.Copy;
  Result.FKind:= Self.Kind;
  }
end;

constructor TBoxData.Create;
begin
  inherited;
  
  IsValid:= False;
  FPostProcessorFileName:= '';
  FTitle:= '';
  SetLength (FBoxArray, 0);
//  FBox:= TBox.Create;
  FSource:= nil;
  FRotateAngle:= 0;
end;

constructor TBoxData.Create (Box: TBox; InputKind: TInputKind);
begin
  inherited Create;
  
//  Self.FBox:= Box;
  FKind:= InputKind;
  FPostProcessorFileName:= '';
  FTitle:= '';
  FSource:= nil;
end;

procedure TBoxData.DrawInBitmap (BitmapImage: TBitmap);
begin
  raise Exception.Create ('Not Implemented Yet!');
  {
  BitmapImage.Canvas.MoveTo (Box.UpperLeftPoint.x, Box.UpperLeftPoint.y);
  BitmapImage.Canvas.LineTo (Box.UpperLeftPoint.x, Box.LowerRightPoint.y);
  BitmapImage.Canvas.LineTo (Box.LowerRightPoint.x, Box.LowerRightPoint.y);
  BitmapImage.Canvas.LineTo (Box.LowerRightPoint.x, Box.UpperLeftPoint.y);
  BitmapImage.Canvas.LineTo (Box.UpperLeftPoint.x, Box.UpperLeftPoint.y);
  }
end;

function TBoxData.ExtractFromBitmap (BitmapImage: TBitmap): TImageCollection;
const
  SearchAreaY: Integer= 10;
  SearchAreaX: Integer= 2;
  NumberOfSamples: Integer= 20;
  LeastAcceptance: Integer= 10;
  MinCountThreshld: Integer= 10;
  YPosPlace= 10;
  XPosPlace= 10;

  function BFS (Px, Py: Integer; MaxX, MaxY, MinX, MinY: Integer): TComponent;//unsafe;
  const
    AdjancedPixelY: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
    AdjancedPixelX: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
    
  var
    Dir: TDirection;
    LastPoint: TPoint;
    XOld, YOld,
    XNew, YNew: Integer;
    CurIndex: Integer;
    RGBWhite,
    RGBBlack: TRGB;
    RowPtr: PByte;

  begin
    RGBWhite.r:= $FF;RGBWhite.g:= $FF;RGBWhite.b:= $FF;
    RGBBlack.r:= $0 ;RGBBlack.g:= $0 ;RGBBlack.b:= $0;

    Result:= TComponent.Create;

    Result.Add (Px, Py, RGBBlack);

    CurIndex:= 0;

    while Result.Count> CurIndex do
    begin
      LastPoint:= Result.GetPixel (CurIndex).Location;
      XOld:= LastPoint.x; YOld:= LastPoint.y;

      for Dir:= dN to dNW do
      begin
        XNew:= XOld; YNew:= YOld;
        Inc (XNew, AdjancedPixelX [Dir]);
        Inc (YNew, AdjancedPixelY [Dir]);

        if (XNew< MaxX) and (MinX< XNew) and
           (YNew< MaxY) and (MinY< YNew) then
        begin

          RowPtr:= BitmapImage.ScanLine [yNew];
          Inc (RowPtr, 3* xNew);

          if ColorIsNotWhite (PColor (RowPtr)^) then
          begin
            Result.Add (xNew, yNew, RGBBlack);

            RowPtr^:= $FF;
            Inc (RowPtr);
            RowPtr^:= $FF;
            Inc (RowPtr);
            RowPtr^:= $FF;
          end;
        end;
      end;

      Inc (CurIndex);
    end;
  end;
  
var
  i, Index, Iter,
  x, y: Integer;

  GuestForPolarUpperLeft,
  GuestForPolarLowerRight: TPolarPoint;

  GuestForPointUpperLeft,
  GuestForPointLowerRight: TPoint;
  Angle: Extended;
  Ptr: PByte;
  UpperRightPoint,
  UpperLeftPoint,
  LowerRightPoint,
  LowerLeftPoint: TPoint;

  AcceptCounter,
  Width, Height: Integer;

  NewImage: TFMLImage;
  NewComponet: TComponent;
  ComponentCollection: TComponentCollection;
begin

  if Self.Kind<> ikCheckBox then
  begin
    Result:= TImageCollection.Create;
    Angle:= RotateAngle; 
    LowerRightPoint:= nil;
    LowerLeftPoint:= nil;
    UpperRightPoint:= nil;
    UpperLeftPoint:= nil;
  
    for i:= 0 to High (FBoxArray) do
    begin
      GuestForPointUpperLeft:= FBoxArray [i].FUpperLeftPoint.Copy;
      GuestForPointUpperLeft.Move (Source);

      GuestForPolarUpperLeft:= GuestForPointUpperLeft.ToPolar;
      GuestForPolarUpperLeft.Rotate (Angle);
      GuestForPointUpperLeft.Free;

      GuestForPointUpperLeft:= GuestForPolarUpperLeft.ToPoint;
      GuestForPolarUpperLeft.Free;

      GuestForPointLowerRight:= FBoxArray [i].FLowerRightPoint.Copy;
      GuestForPointLowerRight.Move (Source);

      GuestForPolarLowerRight:= GuestForPointLowerRight.ToPolar;
      GuestForPolarLowerRight.Rotate (Angle);
      GuestForPointLowerRight.Free;

      GuestForPointLowerRight:= GuestForPolarLowerRight.ToPoint;
      GuestForPolarLowerRight.Free;

      UpperRightPoint:= nil;
      UpperLeftPoint:= nil;

      try
        for y:= GuestForPointUpperLeft.y- SearchAreaY to
                GuestForPointUpperLeft.y+ SearchAreaY do
        begin
          Ptr:= BitmapImage.ScanLine [y];

          x:= (GuestForPointUpperLeft.x- SearchAreaX); 
          Inc (Ptr, 3* x);

          Inc (Ptr, 3* (GuestForPointLowerRight.x- GuestForPointUpperLeft.x) div 10);
          Inc (x, (GuestForPointLowerRight.x- GuestForPointUpperLeft.x) div 10);
          for Iter:= 1 to 9 do
          begin

            if ColorIsNotWhite (PColor (Ptr)^) then
              if CheckForLine (x, y, BitmapImage, 5, 2) then
              begin
                UpperRightPoint:= FindTopRightPointInLine (x, y, BitmapImage);
                UpperLeftPoint:= FindTopLeftPointInLine (x, y, BitmapImage);
                Break;
              end;

            Inc (Ptr, 3* (GuestForPointLowerRight.x- GuestForPointUpperLeft.x) div 10);
            Inc (x, (GuestForPointLowerRight.x- GuestForPointUpperLeft.x) div 10);
          end;

          if UpperRightPoint<> nil then
            Break;
        end;

        LowerRightPoint:= nil;
        LowerLeftPoint:= nil;

        for y:= GuestForPointLowerRight.y+ SearchAreaY downto
                GuestForPointLowerRight.y- SearchAreaY do
        begin
          Ptr:= BitmapImage.ScanLine [y];

          x:= (GuestForPointUpperLeft.x- SearchAreaX); 
          Inc (Ptr, 3* x);

          Inc (Ptr, 3* (GuestForPointLowerRight.x- GuestForPointUpperLeft.x) div 10);
          Inc (x, (GuestForPointLowerRight.x- GuestForPointUpperLeft.x) div 10);
        
          for Iter:= 1 to 9 do
          begin

            if ColorIsNotWhite (PColor (Ptr)^) then
              if CheckForLine (x, y, BitmapImage, 5, 2) then
              begin
                LowerRightPoint:= FindBottomRightPointInLine (x, y, BitmapImage);
                LowerLeftPoint:= FindBottomLeftPointInLine (x, y, BitmapImage);
                Break;
              end;

            Inc (Ptr, 3* (GuestForPointLowerRight.x- GuestForPointUpperLeft.x) div 10);
            Inc (x, (GuestForPointLowerRight.x- GuestForPointUpperLeft.x) div 10);
          end;

          if LowerRightPoint<> nil then
            Break;
        end;

        GuestForPointLowerRight.Free;
        GuestForPointUpperLeft.Free;
      except
        Result:= nil;
        Exit;
      end;
    
  {
      ShowMessage (UpperLeftPoint.ToString);
      ShowMessage (LowerRightPoint.ToString);
  }
      Width:= (UpperRightPoint.x- UpperLeftPoint.x) div NumberOfSamples;
      Height:= (LowerRightPoint.y- UpperLeftPoint.y) div NumberOfSamples;

      for y:= UpperLeftPoint.y to LowerRightPoint.y do
      begin
        AcceptCounter:= 0;
        x:= UpperLeftPoint.x+ Width div 2;

        for Index:= 0 to NumberOfSamples- 1 do
        begin
          if CheckForLine (x, y, BitmapImage, Width div 2, 1) then
            Inc (AcceptCounter);
          Inc (x, Width);
        end;

        if LeastAcceptance< AcceptCounter then
        begin
          UpperRightPoint.y:= y;
          UpperLeftPoint.y:= y;
        end
        else
          Break;
      end;

      for y:= LowerRightPoint.y downto UpperRightPoint.y do
      begin
        AcceptCounter:= 0;
        x:= UpperLeftPoint.x+ Width div 2;

        for Index:= 0 to NumberOfSamples- 1 do
        begin
          if CheckForLine (x, y, BitmapImage, Width div 2, 1) then
            Inc (AcceptCounter);
          Inc (x, Width);
        end;

        if LeastAcceptance< AcceptCounter then
        begin
          LowerRightPoint.y:= y;
          LowerLeftPoint.y:= y;
        end
        else
          Break;
      end;

      for x:= UpperLeftPoint.x to LowerRightPoint.x do
      begin
        AcceptCounter:= 0;
        y:= UpperLeftPoint.y+ Height div 2;

        for Index:= 0 to NumberOfSamples- 1 do
        begin
          if CheckForLine (x, y, BitmapImage, 0, Height div 2) then
            Inc (AcceptCounter);
          Inc (y, Height);
        end;

        if LeastAcceptance< AcceptCounter then
        begin
          LowerLeftPoint.x:= x;
          UpperLeftPoint.x:= x;
        end
        else
          Break;
      end;

      for x:= LowerRightPoint.x downto UpperLeftPoint.x do
      begin
        AcceptCounter:= 0;
        y:= UpperLeftPoint.y+ Height div 2;

        for Index:= 0 to NumberOfSamples- 1 do
        begin
          if CheckForLine (x, y, BitmapImage, 0, Height div 2) then
            Inc (AcceptCounter);
          Inc (y, Height);
        end;

        if LeastAcceptance< AcceptCounter then
        begin
          LowerRightPoint.x:= x;
          UpperRightPoint.x:= x;
        end
        else
          Break;
      end;

      ComponentCollection:= TComponentCollection.Create;

      for y:= UpperLeftPoint.y+ YPosPlace to LowerRightPoint.y- YPosPlace do
      begin
        Ptr:= BitmapImage.ScanLine [y];
        Inc (Ptr, 3* (LowerLeftPoint.x+ XPosPlace));

        for x:= LowerLeftPoint.x+ XPosPlace to LowerRightPoint.x- XPosPlace do
        begin
          if ColorIsNotWhite (PColor (Ptr)^) then
          begin
            NewComponet:= BFS (x, y, LowerRightPoint.x, LowerRightPoint.y,
                                UpperLeftPoint.x, UpperLeftPoint.y);
            if MinCountThreshld< NewComponet.Count then
              ComponentCollection.AddComponent (NewComponet)
            else
              NewComponet.Free;
          end;
          Inc (Ptr, 3);
        end;
      end;

      ComponentCollection.RemoveInvalidComponents;
      Result.AddImage (TFMLImage.Create (ComponentCollection));
      ComponentCollection.Free;
    end;
  end
  else
  begin
//??!!
      NewImage:= TFMLImage.Create;
      NewImage.Row:= LowerRightPoint.y- 2* YPosPlace- UpperLeftPoint.y+ 1;  
      NewImage.Column:= LowerRightPoint.x- 2* XPosPlace- UpperLeftPoint.x+ 1;  

      for y:= UpperLeftPoint.y+ YPosPlace to LowerRightPoint.y- YPosPlace do
      begin
        Ptr:= BitmapImage.ScanLine [y];
        Inc (Ptr, 3* (LowerLeftPoint.x+ XPosPlace));

        for x:= LowerLeftPoint.x+ XPosPlace to LowerRightPoint.x- XPosPlace do
        begin
          if ColorIsNotWhite (PColor (Ptr)^) then
            NewImage.SetPixelBlack (y, x);
          Inc (Ptr, 3);
        end;
      end;

  end;
end;

procedure TBoxData.Free;
var
  i: Integer;
begin
  if Length (FBoxArray)<> 0 then
    for i:= 0 to High (FBoxArray) do
      FBoxArray [i].Free;
  SetLength (FBoxArray, 0);

  if FSource<> nil then
    FSource.Free;
    
  inherited;
end;

function TBoxData.IsSame (Box: TBoxData): Boolean;
begin
  raise Exception.Create ('Not Implemented Yet!');
  {
  Result:= Self.Box.IsSame (Box.Box);
  }
end;

procedure TBoxData.LoadFromFile (var InputFile: TextFile);

  function StrToInputKind (S: String): TInputKind;
  begin
    S:= UpperCase (S);
    if S= UpperCase ('Numeral') then
      Result:= ikNumeral
    else if S= UpperCase ('Alphabetic') then
      Result:= ikAlphabet
    else if S= UpperCase ('CheckBox') then
      Result:= ikCheckBox
    else if S= UpperCase ('Picture') then
      Result:= ikPicture
    else if S= UpperCase ('HelpBar') then
      Result:= ikHelpBar
    else
      Result:= ikNumeral;
  end;
  
var
  S: WideString;
  i: Integer;
  Top, Left,
  Width, Height,
  SpaceWidth: Integer;
begin
  ReadLn (InputFile, S);
  ReadLn (InputFile, S);
  
  ReadLn (InputFile, S);
  FKind:= StrToInputKind (S);

  ReadLn (InputFile, S);
  Readln (InputFile, FPostProcessorFileName);
  
  ReadLn (InputFile, S);
  Readln (InputFile, FTitle);

  ReadLn (InputFile, S);
  Readln (InputFile, FNumberOfElement);
  SetLength (FBoxArray, FNumberOfElement);

  ReadLn (InputFile, S);
  if FKind<> ikCheckBox then
  begin
    Readln (InputFile, Left);
    Readln (InputFile, Top);
    Readln (InputFile, Width);
    Readln (InputFile, Height);
    Readln (InputFile, SpaceWidth);
    
    for i:= 0 to FNumberOfElement- 1 do
    begin
      FBoxArray [i]:= TBox.Create;
      FBoxArray [i].FTitle:= Self.Title+ IntToStr (i);

      FBoxArray [i].FUpperLeftPoint:=
               TPoint.Create (Left+ i* (SpaceWidth+ Width), Top);
      FBoxArray [i].FLowerRightPoint:=
               TPoint.Create (Left+ Width+ i* (SpaceWidth+ Width), Top+ Height);
    end;
  end
  else
    for i:= 0 to FNumberOfElement- 1 do
    begin
      FBoxArray [i]:= TBox.Create;
      FBoxArray [i].LoadFromFile (InputFile, True);
    end;
  ReadLn (InputFile, S);
end;

procedure TBoxData.SaveToFile (var OutputFile: TextFile);
begin
  raise Exception.Create ('Not Implemented Yet!');
  {
  WriteLn (OutputFile, InputKindToString (Kind));
  Box.SaveToFile (OutputFile);
  WriteLn (OutputFile);
  }
end;

{ TFormsBox }

function TFormsBox.AddBox (Box: TBoxData): Boolean;
begin
  SetLength (FAllBoxes, BoxNumber+ 1);
  FAllBoxes [BoxNumber- 1]:= Box;
  FIsLastChangeSaved:= False;
  Result:= True;
//  Box.Box.Complete;

end;

constructor TFormsBox.Create;
begin
  inherited;

  SetLength (FAllBoxes, 0);
  FIsLastChangeSaved:= True;
  FileName:= '';
  FWidth:= -1; MaxX:= 0; MinX:= 100000;
  FHeight:= -1; MaxY:= 0; MinY:= 100000;
  DataPath:= '';
end;

procedure TFormsBox.Free;
var
  i: Integer;
begin
  for i:= 0 to BoxNumber- 1 do
    FAllBoxes [i].Free;
  for i:= 0 to High (Components) do
    Components [i].Free;

  SetLength (FAllBoxes, 0);
  SetLength (Components, 0);
  
  inherited;
end;

function TFormsBox.GetBoxCount: Integer;
begin
  Result:= Length (FAllBoxes);
end;

function TFormsBox.GetBoxes (Index: Integer): TBoxData;
begin
  if (BoxNumber<= Index) or (Index< 0)then
    raise EFormBoxRangeCheckError.Create ('Range Check Error!');
  Result:= FAllBoxes [Index];
end;

procedure TFormsBox.SaveToFile (FileName: String);
var
  i: Integer;
  OutputFile: TextFile;
begin
  Self.FileName:= FileName;
  AssignFile (OutputFile, FileName);
  Rewrite (OutputFile);
  
  for i:= 0 to BoxNumber- 1 do
    FAllBoxes [i].SaveToFile (OutputFile);
    
  WriteLn;
  WriteLn (OutputFile, 'Components');
  for i:= 0 to High (Components) do
    Components [i].SaveToFile (OutputFile);

  CloseFile (OutputFile);
end;

procedure TFormsBox.LoadConfigFile (FileName: String);

  function WideStrReadLn (var InputFile: TextFile): WideString;
  var
    LastCh,
    Ch: Char;
    
  begin
    Result:= '';
    LastCh:= #0;

    while not EoLn (InputFile) do
    begin
      Read (InputFile, Ch);
      if (Ch= #10) and (LastCh= #13) then
        Exit;

      Result:= Result+ Ch;
      LastCh:= Ch; 
    end;
  end;

var
  InputFile: TextFile;
  WideStr: WideString;
  Box: TBoxData;

begin
  Self.FileName:= FileName;

  AssignFile (InputFile, FileName);
  Reset (InputFile);

  ReadLn (InputFile, WideStr);
  Readln (InputFile, FFormID);

  while not Eof (InputFile) do
  begin
    Box:= TBoxData.Create;
    Box.LoadFromFile (InputFile);

    Self.AddBox (Box);
  end;
  
  CloseFile (InputFile);
end;

procedure TFormsBox.SaveToFile;
var
  SaveDialog: TSaveDialog;
begin
  if FileName= '' then
  begin
    SaveDialog:= TSaveDialog.Create (nil);
    SaveDialog.DefaultExt:= '.txt';
    SaveDialog.Filter:= 'Text Files|*.txt|All Files|*.*';
    if SaveDialog.Execute then
      FileName:= SaveDialog.FileName
    else
    begin
      SaveDialog.Free;
      Exit;
    end;
    
    SaveDialog.Free;
  end;

  SaveToFile (FileName);
end;

procedure TFormsBox.SetFileName (const Value: String);
begin
  FFileName:= Value;
end;

procedure TFormsBox.LoadFromFile;
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog:= TOpenDialog.Create (nil);
  OpenDialog.DefaultExt:= '.txt';
  OpenDialog.Filter:= 'Text Files|*.txt|All Files|*.*';

  if OpenDialog.Execute then
  begin
    Self.FileName:= OpenDialog.FileName;
    Self.LoadConfigFile (OpenDialog.FileName);
    Self.FIsLastChangeSaved:= True;
  end;

end;

function TFormsBox.GetWidth: Integer;
{
var
  i: Integer;
  }
begin
  raise Exception.Create ('Not Implemented Yet!');
{
  if FWidth= -1 then
  begin
    if BoxNumber> 0 then
      MaxX:= FAllBoxes [0].Box.LowerRightPoint.x
    else
    begin
      Result:= 0;
      Exit;
    end;

    for i:= 0 to BoxNumber- 1 do
    begin
      if MaxX< FAllBoxes [i].FBox.LowerRightPoint.x then
        MaxX:= FAllBoxes [i].FBox.LowerRightPoint.x;
    end;
    FWidth:= MaxX;
  end;
  Result:= FWidth;
  }
end;

function TFormsBox.GetHeight: Integer;
{
var
  i: Integer;
  }
begin
  raise Exception.Create ('Not Implemented Yet!');
{
  if FHeight= -1 then
  begin
    if BoxNumber> 0 then
      MaxY:= FAllBoxes [0].Box.LowerRightPoint.y
    else
    begin
      Result:= 0;
      Exit;
    end;

    for i:= 0 to BoxNumber- 1 do
    begin
      if MaxY< FAllBoxes [i].FBox.LowerRightPoint.y then
        MaxY:= FAllBoxes [i].FBox.LowerRightPoint.y;
    end;
    FHeight:= MaxY;
  end;
  Result:= FHeight;
  }
end;

procedure TFormsBox.DrawInBitmap (BitmapImage: TBitmap);
var
  i: Integer;
begin
  BitmapImage.Width:= Width;
  BitmapImage.Height:= Height;

  for i:= 0 to BoxNumber- 1 do
    FAllBoxes [i].DrawInBitmap (BitmapImage);  
end;

function TFormsBox.ExtractBox (BitmapImage: TBitmap; UseDialation: Boolean): TImageCollection;
const
  AdjancedPixelY: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
  AdjancedPixelX: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);

var
  ImageCollection: TImageCollection;
  i: Integer;

//  ComponentCollection: TComponentCollection;
  MyBitmap: TMyBitmap;
  Box: TBoxData;
begin
{??!!
  if BitmapImage.PixelFormat<> pf24Bit then
    raise EInvalidImage.Create ('Invalid Image In Find Component');
}
   { TODO -oAmir -cPreProcessor :   Add Some code to not allow to segment the characters }
  Result:= TImageCollection.Create;

  MyBitmap:= TMyBitmap.Create (BitmapImage);

  for i:= 0 to High (FAllBoxes) do
  begin
    Box:= FAllBoxes [i];

    if Box.Kind= ikCheckBox then
    begin
      ImageCollection:= Box.ExtractFromBitmap (BitmapImage);
    end
    else if Box.Kind in [ikNumeral, ikAlphabet] then
    begin
      try

        ImageCollection:= Box.ExtractFromBitmap (BitmapImage);

        if UseDialation then
          ImageCollection.Dilate;
        ImageCollection.SaveFilesAsBitmap (IntToStr (i));
      except
        on E: Exception do
          ShowMessage (E.Message+ IntToStr (i));
      end;
    end;
  end;

  MyBitmap.Free;
  Result.Free;
end;

function TFormsBox.DeleteBox (Index: Integer): Boolean;
var
  i: Integer;
begin
  if (Index< 0) or (Index>= BoxNumber) then
    raise ERangeError.Create ('Range Check Error');
    
  Boxes [Index].Free;
  for i:= Index+ 1 to BoxNumber- 1 do
    FAllBoxes [i- 1]:= Boxes [i];
  SetLength (FAllBoxes, BoxNumber- 1);
  Result:= True;
end;

{procedure TFormsBox.GenerateHtml (Data: array of Integer);
begin

end;
}

function TFormsBox.GenerateOutput (FormImage: TBitmap;
  Data: array of Integer): TBitmap;
const
  Digits: array [0..9] of String= ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
  PersianAlphabet: array [0..31] of String= ('ا', 'ب', 'پ', 'ت', 'ث', 'ج', 'چ', 'ح', 'خ', 'د',
      'ذ', 'ر', 'ز', 'ژ', 'س', 'ش', 'ص', 'ض', 'ط', 'ظ', 'ع', 'غ', 'ف', 'ق', 'ک', 'گ', 'ل', 'م', 'ن', 'و', 'ه', 'ی');
{
var
  Box: TBox;
  i, j: Integer;
  }
begin
  raise Exception.Create ('Not Implemented Yet!');
{
  if Length (Data)<> BoxNumber- 1 then
    raise Exception.Create ('Invalid number of entry');

  j:= -1;
  i:= 0;

  while i< BoxNumber- 1 do
  begin
    Inc (j);
    if Boxes [j].Kind= ikHelpBar then
      Continue;
    if Data [i]<= -1 then
      Continue;

    case Boxes [j].Kind of
      ikNumeral:
      begin
        Box:= Boxes [j].Box;
        if (9< Data [i]) then
          raise Exception.Create ('Number '+ IntToStr (Data [i])+ ' in Box '+ IntToStr (i));
        FormImage.Canvas.TextOut ((Box.UpperLeftPoint.x+ Box.LowerRightPoint.x) div 2,
                Box.UpperLeftPoint.y+ 2, Digits [Data [i]]);
      end;
      ikAlphabet:
      begin
        Box:= Boxes [j].Box;
        if (31< Data [i]) then
          raise Exception.Create ('Number '+ IntToStr (Data [i])+ ' in Box '+ IntToStr (i));
        FormImage.Canvas.TextOut ((Box.UpperLeftPoint.x+ Box.LowerRightPoint.x) div 2,
                Box.UpperLeftPoint.y+ 2, PersianAlphabet [Data [i]]);
      end;
    end;

    Inc (i);
  end;

  Result:= FormImage;
  }
end;
{
function TFormsBox.DeleteNoise (MyBitmapImage: TMyBitmap;
          NoiseColor: TRGB): TBitmap;
var
  x, y: Integer;
begin
  Result:= TBitmap.Create;
  Result.Width:= MyBitmapImage.Width;
  Result.Height:= MyBitmapImage.Height;

  for y:= 0 to MyBitmapImage.Height- 1 do
    for x:= 0 to MyBitmapImage.Width- 1 do
      if ColorsAreTheSame (NoiseColor, MyBitmapImage.Body (x, y)) then
        Result.Canvas.Pixels [x, y]:= clWhite
      else
        Result.Canvas.Pixels [x, y]:= MyBitmapImage.Body (x, y).Color;
end;
}

function TFormsBox.ConvertToHSI (Color: TColor): THSI;
var
  Teta: Extended;
  r, g, b: Integer;
begin
  r:= Color and $FF;
  g:= (Color and $FF00) shr 8;
  b:= (Color and $FF0000) shr 16;

  if (r<> g) or (g<> b) then
  begin
    Teta:= ArcCos ( (r- 0.5* (g+ b))/
                    Sqrt ((r- g)* (r- g)+ (r- b)* (g- b))
                    );
    if r<= g then
      Result.h:= Round (Teta* 180/ Pi)
    else
      Result.h:= 360- Round (Teta* 180/ Pi);
  end
  else
    Result.h:= -1;

  if (r<> 0) or (g<> 0) or (b<> 0) then
    Result.s:= 1- 3/ (r+ g+ b) *  Math.Min (Math.Min (r, g), b)
  else
    Result.s:= 0.0;

  Result.i:= (r+ g+ b) div 3;
end;

function ColorsAreInSameRange (Color1, Color2: TRGB): Boolean;
const
  Thr: Integer= 50;
begin
  Result:= (Abs (Color1.r- Color2.r)< Thr) and
           (Abs (Color1.g- Color2.g)< Thr) and
           (Abs (Color1.b- Color2.b)< Thr);
end;

function ColorsAreTheSame (Color1, Color2: TRGB): Boolean;
const
  Thr: Integer= 50;
begin
  Result:= Sqrt (Sqr (Color1.r- Color2.r)+
          Sqr (Color1.g- Color2.g)+
          Sqr (Color1.b- Color2.b))< Thr;
end;

function TFormsBox.ConvertToRGB (Color: TColor): TRGB;
begin
  Result.r:= Color and $FF;
  Result.g:= (Color and $FF00) shr 8;
  Result.b:= (Color and $FF0000) shr 16;
end;

//{$UNSAFECODE ON$}
function TFormsBox.FindHelpBar (MyBitmap: TMyBitmap; Box: TBox; HelpBarColor: TRGB; ContinueSearchOverBorders: Integer= 1): TComponentCollection;//unsafe;
type
  PByte= ^Byte;
var
  RowPtrs: PointerArray;

  const
    AdjancedPixelY: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
    AdjancedPixelX: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);

  function FindContour (StartPointColor: TRGB; Px, Py: Integer): TComponent;//unsafe;
  var
    Found: Boolean;
    i: Integer;
    LastDir,
    Dir: TDirection;
  begin
    Result:= TComponent.Create;
    LastDir:= dE;

    while not Result.IsExists (Px, Py) do
    begin                         
       Result.Add (Px, Py, StartPointColor);
       Found:= False;
       Dir:= TDirection ((Integer (LastDir)+ 5) mod 8);
       for i:= 0 to 7 do
       begin
         if ColorsAreTheSame (MyBitmap.Body (Px+ AdjancedPixelX [Dir], Py+ AdjancedPixely [Dir]), StartPointColor) then
         begin
           Inc (Px, AdjancedPixelX [Dir]);
           Inc (Py, AdjancedPixelY [Dir]);
           Found:= True;
           LastDir:= Dir;

           Break;
         end;
         Dir:= TDirection ((Integer (Dir)+ 1) mod 8);
       end;
           
       if not Found then
       begin
         Result:= nil;
         Exit;
       end;
    end;

  end;

  function BFS (StartPointColor: TRGB; Px, Py: Integer; var IsExceeded: Boolean): TComponent;//unsafe;
  var
    Dir: TDirection;
    LastPoint: TPoint;
    xOld, yOld,
    xNew, yNew: Integer;
//    ImageWidth, ImageHeight,
    CurIndex: Integer;
    RGBWhite,
    NewPointColor: TRGB;
//    LastPtr,
    RowPtr: PByte;
  begin
    RGBWhite.r:= $FF;RGBWhite.g:= $FF;RGBWhite.b:= $FF;
    IsExceeded:= False;

    Result:= TComponent.Create;

    Result.Add (Px, Py, StartPointColor);

    CurIndex:= 0;
//    ImageWidth:= MyBitmap.Width;
//    ImageHeight:= MyBitmap.Height;

    while Result.Count> CurIndex do
    begin
      LastPoint:= Result.GetPixel (CurIndex).Location;
      xOld:= LastPoint.x; yOld:= LastPoint.y;
      StartPointColor:= Result.CollectionColor;

      if ColorsAreTheSame (StartPointColor, RGBWhite) then
      begin
        Result.Free;
        Exit;
      end;

      for Dir:= dN to dNW do
      begin
        xNew:= xOld; yNew:= yOld;
        Inc (xNew, AdjancedPixelX [Dir]);
        Inc (yNew, AdjancedPixelY [Dir]);

        RowPtr:= RowPtrs [yNew];
        Inc (RowPtr, 3* xNew);

        NewPointColor.b:= RowPtr^;
        Inc (RowPtr);
        NewPointColor.g:= RowPtr^;
        Inc (RowPtr);
        NewPointColor.r:= RowPtr^;

        if ColorsAreTheSame (StartPointColor, NewPointColor) and (not Result.IsExists (xNew, yNew)) then
        begin
          if ContinueSearchOverBorders= 1 then
          begin
            if (2* xNew< 3* Box.UpperLeftPoint.x- Box.LowerRightPoint.x) or
               (3* Box.LowerRightPoint.x- Box.UpperLeftPoint.x< 2* xNew) or
               (3* Box.LowerRightPoint.y- Box.UpperLeftPoint.y< 2* yNew) or
               (2* yNew< 3* Box.UpperLeftPoint.y- Box.LowerRightPoint.y) then
            begin
              IsExceeded:= True;
              Continue;
            end;
          end
          else if ContinueSearchOverBorders= 0 then
          begin

            if (xNew< Box.UpperLeftPoint.x) or
               (Box.LowerRightPoint.x< xNew) or
               (Box.LowerRightPoint.y< yNew) or
               (yNew< Box.UpperLeftPoint.y) then
              Continue;

          end;

          if (xNew< Box.UpperLeftPoint.x) or
             (Box.LowerRightPoint.x< xNew) or
             (Box.LowerRightPoint.y< yNew) or
             (yNew< Box.UpperLeftPoint.y) then
            if not ColorsAreTheSame (NewPointColor, Result.CollectionColor) then
              Continue;
              
          Result.Add (xNew, yNew, NewPointColor)
        end;
      end;

      Inc (CurIndex);
    end;
  end;

const
  WidthThr: Integer= 2;//Change it in remove all invalid componentes, too
  HeightThr: Integer= 2;
  xStep: Integer= 1;

var
  yStep: Integer;
//  Ptr4Cleaning,
  RowPtr: PByte;

  RGBWhite,
  RGBColor: TRGB;
//  IsExceeded: Boolean;

  MinPoint, MaxPoint: TPoint;

//  i: Integer;
//  xCounter,
//  yCounter,
  x, y: Integer;
  ComponentsPixels: TComponent;
begin
  RowPtrs:= MyBitmap.Pixels;

  Result:= TComponentCollection.Create;

  RGBWhite:= ConvertToRGB ($FFFFFF);

  x:= MyBitmap.Width div 2;


  yStep:= 1;//(Box.FLowerRightPoint.y- Box.FUpperLeftPoint.y) div 2;

  y:= 50;
  ComponentsPixels:= nil;
  
  while y< MyBitmap.Height- 1  do
  begin
    y:= y+ yStep;

    RowPtr:= RowPtrs [y];
    Inc (RowPtr, 3* x);

    RGBColor.b:= RowPtr^;
    Inc (RowPtr);
    RGBColor.g:= RowPtr^;
    Inc (RowPtr);
    RGBColor.r:= RowPtr^;
    RGBColor.Color:= RGBColor.b* $10000+ RGBColor.g* $100+ RGBColor.r;

    if not ColorsAreTheSame (RGBWhite, RGBColor) then
    begin
//Should be improved
     ComponentsPixels:= FindContour (RGBColor, x, y);
     if ComponentsPixels= nil then
       Continue;

      if ComponentsPixels.Count< 2* (Box.FLowerRightPoint.x- Box.FLowerLeftPoint.x) then
      begin
        ComponentsPixels.Free;
        Continue;
      end;

      if (ComponentsPixels.GetEffectiveLength< HeightThr) or
         (ComponentsPixels.GetEffectiveWidth< WidthThr) then
      begin
        ComponentsPixels.Free;
        Continue;
      end;

      Result.AddComponent (ComponentsPixels);
      Break;
    end;
  end;

  if ComponentsPixels=nil then
  begin
    ShowMessage ('Can not Find HelpBar!!');
    Exit;
  end;

  MinPoint:= Result.MinPoint;
  MaxPoint:= Result.MaxPoint;

  for y:= MinPoint.y+ 10 to MaxPoint.y- 10 do
  begin
    RowPtr:= RowPtrs [y];
    Inc (RowPtr, 3* MinPoint.x);
    for x:= (MinPoint.x+ MaxPoint.x) div 2- 5 to (MinPoint.x+ MaxPoint.x) div 2+ 5 do
    begin
      RGBColor.b:= RowPtr^;
      Inc (RowPtr);
      RGBColor.g:= RowPtr^;
      Inc (RowPtr);
      RGBColor.r:= RowPtr^;
      Inc (RowPtr);

      ComponentsPixels.Add (x, y, RGBColor);
    end;
  end;
  MinPoint.Free;
  MaxPoint.Free;

end;

function TFormsBox.FindComponent (MyBitmap: TMyBitmap; Box: TBox; HelpBarColor: TRGB;
      ContinueSearchOverBorders: Integer): TComponentCollection;//unsafe;
type
  PByte= ^Byte;
var
  RowPtrs: PointerArray;
  IgnoredComponentCollection: TComponentCollection;

  function ColorIsWhite (Color: TRGB): Boolean;
  const
    Thr1: Integer= 10;
    Thr2: Integer= 100;
    Thr3: Integer= 150;
  var
    Count: Integer;
  begin
    if (Abs (Color.r- Color.g)< Thr1) and  (Abs (Color.r- Color.b)< Thr1) and
       (Abs (Color.g- Color.b)< Thr1) then
       if Thr2< Color.r then
       begin
         Result:= True;
         Exit;
       end;
       
    Count:= 0;
    if (Thr3< Color.r) then
      Inc (Count);
    if (Thr3< Color.g) then
      Inc (Count);
    if (Thr3< Color.b) then
      Inc (Count);
    Result:= 2<= Count;
  end;

  function ColorIsBlack (Color: TRGB): Boolean;
  const
    Thr1: Integer= 10;
    Thr2: Integer= 50;
    Thr3: Integer= 30;
  var
    Count: Integer;
  begin
    if (Abs (Color.r- Color.g)< Thr1) and  (Abs (Color.r- Color.b)< Thr1) and
       (Abs (Color.g- Color.b)< Thr1) then
       if Color.r< Thr2 then
       begin
         Result:= True;
         Exit;
       end;
         
    Count:= 0;
    if (Color.r< Thr3) then
      Inc (Count);
    if (Color.g< Thr3) then
      Inc (Count);
    if (Color.b< Thr3) then
      Inc (Count);
    Result:= 2<= Count;
  end;

  function BFS (StartPointColor: TRGB; Px, Py: Integer; var IsExceeded: Boolean): TComponent;//unsafe;
  const
    AdjancedPixelY: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
    AdjancedPixelX: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
  var
    Dir: TDirection;
    LastPoint: TPoint;
    xOld, yOld,
    xNew, yNew: Integer;
    CurIndex: Integer;
    RGBWhite,
    NewPointColor: TRGB;
    RowPtr: PByte;
  begin
    RGBWhite.r:= $FF;RGBWhite.g:= $FF;RGBWhite.b:= $FF;
    IsExceeded:= False;

    Result:= TComponent.Create;

    Result.Add (Px, Py, StartPointColor);

    CurIndex:= 0;

    while Result.Count> CurIndex do
    begin
      LastPoint:= Result.GetPixel (CurIndex).Location;
      xOld:= LastPoint.x; yOld:= LastPoint.y;
      StartPointColor:= Result.CollectionColor;

      for Dir:= dN to dNW do
      begin
        xNew:= xOld; yNew:= yOld;
        Inc (xNew, AdjancedPixelX [Dir]);
        Inc (yNew, AdjancedPixelY [Dir]);

        RowPtr:= RowPtrs [yNew];
        Inc (RowPtr, 3* xNew);

        NewPointColor.b:= RowPtr^;
        Inc (RowPtr);
        NewPointColor.g:= RowPtr^;
        Inc (RowPtr);
        NewPointColor.r:= RowPtr^;

        if IgnoredComponentCollection.IsExists (xNew, yNew) then
          Continue;

        if (not ColorsAreTheSame (HelpBarColor, NewPointColor) and not ColorsAreTheSame (RGBWhite, NewPointColor) and
              not ColorIsWhite (NewPointColor) and not ColorIsBlack (NewPointColor))
                 and (not Result.IsExists (xNew, yNew)) then
        begin

          if ContinueSearchOverBorders= 1 then
          begin
            if (2* xNew< 3* Box.UpperLeftPoint.x- Box.LowerRightPoint.x) or
               (3* Box.LowerRightPoint.x- Box.UpperLeftPoint.x< 2* xNew) or
               (3* Box.LowerRightPoint.y- Box.UpperLeftPoint.y< 2* yNew) or
               (2* yNew< 3* Box.UpperLeftPoint.y- Box.LowerRightPoint.y) then
            begin
              IsExceeded:= True;
              Continue;
            end;
          end
          else if ContinueSearchOverBorders= 0 then
          begin

            if (xNew< Box.UpperLeftPoint.x) or
               (Box.LowerRightPoint.x< xNew) or
               (Box.LowerRightPoint.y< yNew) or
               (yNew< Box.UpperLeftPoint.y) then
              Continue;
          end;


          if (xNew< Box.UpperLeftPoint.x) or
             (Box.LowerRightPoint.x< xNew) or
             (Box.LowerRightPoint.y< yNew) or
             (yNew< Box.UpperLeftPoint.y) then
            if not ColorsAreTheSame (NewPointColor, Result.CollectionColor) then
              Continue;
          Result.Add (xNew, yNew, NewPointColor)
        end;
      end;

      Inc (CurIndex);
//      Result.HashedData.SaveToFile (IntToStr (CurIndex)+ '.txt');///??!!
    end;
  end;
{
const
  WidthThr: Integer= 2;//Change it in remove all invalid componentes, too
  HeightThr: Integer= 2;
  yStep: Integer= 1;
  xStep: Integer= 1;
  PixelsInSameLineThr= 0.80;
var
  Ptr4Cleaning,
  RowPtr: PByte;

  RGBWhite,
  RGBBlack,
  RGBColor: TRGB;
  IsExceeded: Boolean;

  MinPoint, MaxPoint: TPoint;

//  TempImage: TFMLImage;

  i: Integer;
//  xCounter,
  yCounter,
  x, y: Integer;
  PixelCollection,
  ComponentsPixels: TComponent;
  }
begin
{
  RGBBlack.r:= $0; RGBBlack.g:= $0; RGBBlack.b:= $0;
  
  RowPtrs:= MyBitmap.Pixels;

  Result:= TComponentCollection.Create;

  RGBWhite:= ConvertToRGB ($FFFFFF);

  IgnoredComponentCollection:= TComponentCollection.Create;

  for yCounter:= 0 to (Box.LowerRightPoint.y- Box.UpperRightPoint.y+ yStep- 1) div yStep do
  begin
    y:= yCounter* yStep+ Box.UpperLeftPoint.y;
    RowPtr:= RowPtrs [y];
//    Inc (RowPtr, 3* Box.UpperLeftPoint.x);
    Inc (RowPtr, 3* (Box.UpperLeftPoint.x));

    x:= Box.UpperLeftPoint.x- xStep;

    while x< Box.LowerRightPoint.x do
    begin
      x:= x+ xStep;
      Inc (RowPtr, 3* (xStep- 1));

      RGBColor.b:= RowPtr^;
      Inc (RowPtr);
      RGBColor.g:= RowPtr^;
      Inc (RowPtr);
      RGBColor.r:= RowPtr^;
      Inc (RowPtr);
      RGBColor.Color:= RGBColor.b* $10000+ RGBColor.g* $100+ RGBColor.r;

      if (not ColorsAreTheSame (RGBWhite, RGBColor) and not ColorsAreTheSame (HelpBarColor, RGBColor)) then
      begin
        if IgnoredComponentCollection.IsExists (x, y) then
          Continue;
        ComponentsPixels:= BFS (RGBColor, x, y, IsExceeded);

        if (not IsExceeded) or (ContinueSearchOverBorders= 2) then
        begin
          for i:= 0 to ComponentsPixels.Count- 1 do
          begin
            Ptr4Cleaning:= RowPtrs [ComponentsPixels.GetPixel (i).Location.y];
            Inc (Ptr4Cleaning, 3* ComponentsPixels.GetPixel (i).Location.x);
            Ptr4Cleaning^:= $FF;
            Inc (Ptr4Cleaning);
            Ptr4Cleaning^:= $FF;
            Inc (Ptr4Cleaning);
            Ptr4Cleaning^:= $FF;
          end;
        end
        else
        begin
          IgnoredComponentCollection.AddComponent (ComponentsPixels);
          Continue;
        end;

        if ColorsAreTheSame (ComponentsPixels.CollectionColor, HelpBarColor) or
           ColorsAreTheSame (ComponentsPixels.CollectionColor, RGBWhite) then
        begin
          ComponentsPixels.Free;
          Continue;
        end;

        MinPoint:= ComponentsPixels.GetMinimum;
        MaxPoint:= ComponentsPixels.GetMaximum;

        if (MaxPoint.x- MinPoint.x< WidthThr) or (MaxPoint.y- MinPoint.y< HeightThr) then
        begin
          MinPoint.Free;
          MaxPoint.Free;
          ComponentsPixels.Free;

          Continue;
        end;
        MinPoint.Free;
        MaxPoint.Free;

        if (ComponentsPixels.GetEffectiveLength< HeightThr) or
           (ComponentsPixels.GetEffectiveWidth< WidthThr) or
           (10* ComponentsPixels.Count< ComponentsPixels.GetArea)then
        begin
          ComponentsPixels.Free;
          Continue;
        end;

        PixelCollection:= ComponentsPixels.ExtractContour;

        if (PixelsInSameLineThr< PixelCollection.CountInSameLine) and
           ColorsAreTheSame (PixelCollection.CollectionColor, RGBBlack) and
           ((ComponentsPixels.GetEffectiveLength< 4) or
             (ComponentsPixels.GetEffectiveWidth< 4)) then
        begin
          ComponentsPixels.Free;
          PixelCollection.Free;
          Continue;
        end;
        PixelCollection.Free;


        Result.AddComponent (ComponentsPixels);

      end;

    end;
  end;

  IgnoredComponentCollection.Free;
{
  CloseFile (FileHandle);
}
//  Result.RemoveInvalidComponents;
  Result:= nil;
end;

//{$UNSAFECODE OFF$}

{ TMyBitmap }

//{$UNSAFECODE ON}

function TMyBitmap.Body (x, y: Integer): TRGB;//unsafe;
var
  Address: ^Byte;
//  Color: TColor;
begin
  Address:= Pointer (Pixels [y]);
  Inc (Address, 3* x);
  Result.b:= Address^;
  Inc (Address);
  Result.g:= Address^;
  Inc (Address);
  Result.r:= Address^;
end;
//{$UNSAFECODE OFF}

function TMyBitmap.ConvertToRGB (Color: TColor): TRGB;
begin
  Result.Color:= Color;
  Result.r:= Color and $FF;
  Result.g:= (Color and $FF00) shr 8;
  Result.b:= (Color and $FF0000) shr 16;
end;

//{$UNSAFECODE ON}

constructor TMyBitmap.Create (Bitmap: TBitmap);//unsafe;
var
  y: Integer;
begin
  inherited Create;
  SetLength (Pixels, Bitmap.Height);
  Width:= Bitmap.Width;
  Height:= Bitmap.Height;

  case Bitmap.PixelFormat of
    pfDevice:
      IncValue:= 3;
    pf24bit:
      IncValue:= 3;
    pf32bit:
      IncValue:= 4
    else
      IncValue:= 3;
  end;

  for y:= 0 to Height- 1 do
    Pixels [y]:= Bitmap.ScanLine [y];//.ToPointer;
end;
//{$UNSAFECODE OFF}

procedure TMyBitmap.Free;
begin
  inherited;
  SetLength (Pixels, 0);
end;

//{$UNSAFECODE ON}

function TMyBitmap.GetPixels: Pointer;//unsafe;
begin
  Result:= Pixels;
end;
//{$UNSAFECODE OFF}

procedure TFormsBox.AddComponent (DataComponent: TDataComponent);
begin
  SetLength (Components, Length (Components)+ 1);
  Components [High (Components)]:= DataComponent;
end;


function TFormsBox.GetNumberOfCheckBoxFeilds: Integer;
var
  i: Integer;
begin
  Result:= 0;
  
  for i:= 0 to High (FAllBoxes) do
    if FAllBoxes [i].FKind= ikCheckBox then
      Inc (Result);
end;

function TFormsBox.GetNumberOfNumeralFeilds: Integer;
var
  i: Integer;
begin
  Result:= 0;
  
  for i:= 0 to High (FAllBoxes) do
    if FAllBoxes [i].FKind= ikNumeral then
      Inc (Result);
end;

function TFormsBox.GetNumberOfAlphabeticFeilds: Integer;
var
  i: Integer;
begin
  Result:= 0;
  
  for i:= 0 to High (FAllBoxes) do
    if FAllBoxes [i].FKind= ikAlphabet then
      Inc (Result);
end;

function TFormsBox.GetPostprocessingTypeOfAlphabeticFeilds: TStringArray;
var
  i: Integer;
begin
  SetLength (Result, 0);

  for i:= 0 to High (FAllBoxes) do
    if FAllBoxes [i].Kind= ikAlphabet then
      if FAllBoxes [i].PostProcessorFile<> '' then
      begin
        SetLength (Result, Length (Result)+ 1);
        Result [High (Result)]:= FAllBoxes [i].PostProcessorFile;
      end;
end;

function TFormsBox.GetFeildsTitles: TWideStringArray;
var
  i: Integer;
begin
  SetLength (Result, 0);

  for i:= 0 to High (FAllBoxes) do
    if FAllBoxes [i].Kind in [ikAlphabet, ikNumeral] then
    begin
      SetLength (Result, Length (Result)+ 1);
      Result [High (Result)]:= FAllBoxes [i].FTitle;
    end;
end;

function TFormsBox.GetCheckBoxOptionTitles: TWideStringArrayOfArray;
{
var
  i, j: Integer;
}  
begin
  raise Exception.Create ('Not Implemented Yet!');
{
  SetLength (Result, 0);

  for i:= 0 to High (FAllBoxes) do
    if FAllBoxes [i].Kind= ikCheckBox then
    begin
      SetLength (Result, Length (Result)+ 1);
      for j:= 0 to FAllBoxes [i]. do

      Result [High (Result)]:= FAllBoxes [i].FTitle;
    end;
}
end;

procedure TFormsBox.SetSaveResultsAsBitmaps(const Value: Boolean);
begin
  FSaveResultsAsBitmaps:= Value;
end;

function ColorIsNotWhite (Color: TColor): Boolean;
const
  WhitenessThreshld: Integer= 3000;
begin
  Result:= Sqr ((Color and $FF)- 255)+ Sqr (((Color and $FF00) shr 8)- 255)
     + Sqr (((Color and $FF0000) shr 16)- 255)> WhitenessThreshld;
end;

function CheckForLine (x, y: Integer; BitmapImage: TBitmap;
  WidthCheck: Integer= WidthSearchThreshld;
  HeightCheck: Integer= HeightarchThreshld): Boolean;
var
  Ptr: PByte;
  x1, y1: Integer;

begin

  Result:= True;

  for y1:= y to y+ HeightCheck- 1 do
  begin
    Ptr:= BitmapImage.ScanLine [y1];
    Inc (Ptr, 3* (x- WidthCheck));

    for x1:= x- WidthCheck to  x+ WidthCheck do
    begin
      if not ColorIsNotWhite (PColor (Ptr)^) then
      begin
        Result:= False;
        if not ColorIsNotWhite (PColor (Ptr)^) then
          Result:= False;
        Exit;
      end;
      Inc (Ptr, 3);
    end;
  end;

end;

function FindBottomRightPointInLine (x, y: Integer; BitmapImage: TBitmap): TPoint;
const
  SuccessiveUpMoveThreshold= 5;

  AdjancedPixelY: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
  AdjancedPixelX: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
var
  VisitHistory: array [0..SuccessiveUpMoveThreshold] of TPoint;
//  Ptr: PByte;
  LastDir,
  Dir: TDirection;
  SuccessiveUpMove: Integer;
  i: Integer;

begin

  for i:= 0 to SuccessiveUpMoveThreshold do
    VisitHistory [i]:= TPoint.Create (0, 0);

  Dir:= dS;
  while ColorIsNotWhite (BitmapImage.Canvas.Pixels [x+ AdjancedPixelX [Dir],
                   y+ AdjancedPixelY [Dir]]) do
  begin
    Inc (x, AdjancedPixelX [Dir]);
    Inc (y, AdjancedPixelY [Dir]);
  end;
    
  SuccessiveUpMove:= 0;
  LastDir:= dS;
  LastDir:= TDirection ((Ord (LastDir)+ 5) mod 8);

  while SuccessiveUpMove< SuccessiveUpMoveThreshold do
  begin
    Dir:= TDirection ((Ord (LastDir)+ 3) mod 8);

    for i:= 0 to 7 do
    begin
      if ColorIsNotWhite (BitmapImage.Canvas.Pixels [x+ AdjancedPixelX [Dir],
                   y+ AdjancedPixelY [Dir]]) then
      begin
        Inc (x, AdjancedPixelX [Dir]); 
        Inc (y, AdjancedPixelY [Dir]);
        LastDir:= Dir;
        Break; 
      end;
        
      if Dir= dN then
        Dir:= dNW
      else
        Dec (Dir);
    end;

    if LastDir in [dNE, dN, dNW] then
    begin
      VisitHistory [SuccessiveUpMove].x:= x;  
      VisitHistory [SuccessiveUpMove].y:= y;  
      Inc (SuccessiveUpMove);
    end
    else
      SuccessiveUpMove:= 0;
  end;
     
  for i:= 1 to SuccessiveUpMoveThreshold do
    VisitHistory [i].Free;
  Result:= VisitHistory [0];
end;

function FindBottomLeftPointInLine (x, y: Integer; BitmapImage: TBitmap): TPoint;
const
  SuccessiveUpMoveThreshold= 5;

  AdjancedPixelY: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
  AdjancedPixelX: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
var
  VisitHistory: array [0..SuccessiveUpMoveThreshold] of TPoint;
  LastDir,
  Dir: TDirection;
  SuccessiveUpMove: Integer;
  i: Integer;

begin

  for i:= 0 to SuccessiveUpMoveThreshold do
    VisitHistory [i]:= TPoint.Create (0, 0);

  Dir:= dS;
  while ColorIsNotWhite (BitmapImage.Canvas.Pixels [x+ AdjancedPixelX [Dir],
                   y+ AdjancedPixelY [Dir]]) do
  begin
    Inc (x, AdjancedPixelX [Dir]);
    Inc (y, AdjancedPixelY [Dir]);
  end;
    
  SuccessiveUpMove:= 0;
  LastDir:= dS;
  LastDir:= TDirection ((Ord (LastDir)+ 3) mod 8);

  while SuccessiveUpMove< SuccessiveUpMoveThreshold do
  begin
    Dir:= TDirection ((Ord (LastDir)+ 5) mod 8);

    for i:= 0 to 7 do
    begin
      if ColorIsNotWhite (BitmapImage.Canvas.Pixels [x+ AdjancedPixelX [Dir],
                   y+ AdjancedPixelY [Dir]]) then
      begin
        Inc (x, AdjancedPixelX [Dir]); 
        Inc (y, AdjancedPixelY [Dir]);
        LastDir:= Dir;
        Break; 
      end;
        
      if Dir= dNW then
        Dir:= dN
      else
        Inc (Dir);
    end;

    if LastDir in [dNE, dN, dNW] then
    begin
      VisitHistory [SuccessiveUpMove].x:= x;
      VisitHistory [SuccessiveUpMove].y:= y;  
      Inc (SuccessiveUpMove);
    end
    else
      SuccessiveUpMove:= 0;
  end;
     
  for i:= 1 to SuccessiveUpMoveThreshold do
    VisitHistory [i].Free;
  Result:= VisitHistory [0];
end;

function FindTopRightPointInLine (x, y: Integer; BitmapImage: TBitmap): TPoint;
const
  SuccessiveDownMoveThreshold= 5;

  AdjancedPixelY: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
  AdjancedPixelX: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
var
  VisitHistory: array [0..SuccessiveDownMoveThreshold] of TPoint;
//  Ptr: PByte;
  LastDir,
  Dir: TDirection;
  SuccessiveDownMove: Integer;
  i: Integer;

begin

  for i:= 0 to SuccessiveDownMoveThreshold do
    VisitHistory [i]:= TPoint.Create (0, 0);

  Dir:= dN;
  while ColorIsNotWhite (BitmapImage.Canvas.Pixels [x+ AdjancedPixelX [Dir],
                   y+ AdjancedPixelY [Dir]]) do
  begin
    Inc (x, AdjancedPixelX [Dir]);
    Inc (y, AdjancedPixelY [Dir]);
  end;
    
  SuccessiveDownMove:= 0;
  LastDir:= dN;
  Inc (LastDir, 3);

  while SuccessiveDownMove< SuccessiveDownMoveThreshold do
  begin
    Dir:= TDirection ((Ord (LastDir)+ 5) mod 8);

    for i:= 0 to 7 do
    begin
      if ColorIsNotWhite (BitmapImage.Canvas.Pixels [x+ AdjancedPixelX [Dir],
                   y+ AdjancedPixelY [Dir]]) then
      begin
        Inc (x, AdjancedPixelX [Dir]); 
        Inc (y, AdjancedPixelY [Dir]);
        LastDir:= Dir;
        Break; 
      end;
        
      if Dir= dNW then
        Dir:= dN
      else
        Inc (Dir);
    end;

    if LastDir in [dSE, dS, dSW] then
    begin
      VisitHistory [SuccessiveDownMove].x:= x;  
      VisitHistory [SuccessiveDownMove].y:= y;  
      Inc (SuccessiveDownMove);
    end
    else
      SuccessiveDownMove:= 0;
  end;
     
  for i:= 1 to SuccessiveDownMoveThreshold do
    VisitHistory [i].Free;
  Result:= VisitHistory [0];
end;

function FindTopLeftPointInLine (x, y: Integer; BitmapImage: TBitmap): TPoint;
const
  SuccessiveDownMoveThreshold= 5;

  AdjancedPixelY: array [dN..dNW] of Integer= (-1, -1,  0, +1, +1, +1,  0, -1);
  AdjancedPixelX: array [dN..dNW] of Integer= ( 0, +1, +1, +1,  0, -1, -1, -1);
var
  VisitHistory: array [0..SuccessiveDownMoveThreshold] of TPoint;
//  Ptr: PColor;
  LastDir,
  Dir: TDirection;
  SuccessiveDownMove: Integer;
  i: Integer;

begin

  for i:= 0 to SuccessiveDownMoveThreshold do
    VisitHistory [i]:= TPoint.Create (0, 0);

  Dir:= dN;
  while ColorIsNotWhite (BitmapImage.Canvas.Pixels [x+ AdjancedPixelX [Dir],
                   y+ AdjancedPixelY [Dir]]) do
  begin
    Inc (x, AdjancedPixelX [Dir]);
    Inc (y, AdjancedPixelY [Dir]);
  end;
    
  SuccessiveDownMove:= 0;
  LastDir:= dN;
  LastDir:= TDirection ((Ord (LastDir)+ 5) mod 8);

  while SuccessiveDownMove< SuccessiveDownMoveThreshold do
  begin
    Dir:= TDirection ((Ord (LastDir)+ 3) mod 8);

    for i:= 0 to 7 do
    begin
      if ColorIsNotWhite (BitmapImage.Canvas.Pixels [x+ AdjancedPixelX [Dir],
                   y+ AdjancedPixelY [Dir]]) then
      begin
        Inc (x, AdjancedPixelX [Dir]); 
        Inc (y, AdjancedPixelY [Dir]);
        LastDir:= Dir;
        Break; 
      end;
        
      if Dir= dN then
        Dir:= dNW
      else
        Dec (Dir);
    end;

    if LastDir in [dSE, dS, dSW] then
    begin
      VisitHistory [SuccessiveDownMove].x:= x;  
      VisitHistory [SuccessiveDownMove].y:= y;  
      Inc (SuccessiveDownMove);
    end
    else
      SuccessiveDownMove:= 0;
  end;
     
  for i:= 1 to SuccessiveDownMoveThreshold do
    VisitHistory [i].Free;
  Result:= VisitHistory [0];
end;

function TFormsBox.FindSkewAndReplacenent (Bitmap: TBitmap): Boolean;

  procedure SaveSkewReplacenent (TopRightPoint, TopLeftPoint: TPoint);
  var
    i: Integer;
    DeltaX, DeltaY: Integer;
//    SkewAngle: Extended;
    Box: TBoxData;
    
  begin
    DeltaX:= TopRightPoint.x- TopLeftPoint.x;
    DeltaY:= TopRightPoint.y- TopLeftPoint.y;

    for i:= 0 to High (FAllBoxes) do
    begin
      Box:= FAllBoxes [i];

      Box.Source:= TopLeftPoint.Copy;
      Box.FRotateAngle:= CalcArcTan (DeltaX, DeltaY);
    end;
  end;

var
  x, y: Integer;
  Ptr: PByte;
  TopRightPoint, TopLeftPoint: TPoint;

const
  StartSearchForY: Integer= 180;
  EndSearchForY: Integer= 240;
begin

  Result:= True;
  try
    for y:= StartSearchForY to EndSearchForY do
    begin
      Ptr:= Bitmap.ScanLine [y];

      Inc (Ptr, 3* (Bitmap.Width div 3));
      for x:= Bitmap.Width div 3 to 2* Bitmap.Width div 3 do
      begin

        if ColorIsNotWhite (PColor (Ptr)^) then
          if CheckForLine (x, y, Bitmap) then
          begin
            TopRightPoint:= FindTopRightPointInLine (x, y, Bitmap);
            TopLeftPoint:= FindTopLeftPointInLine (x, y, Bitmap);

            SaveSkewReplacenent (TopRightPoint, TopLeftPoint);

            TopRightPoint.Free;
            TopLeftPoint.Free;
            Exit;
          end;

        Inc (Ptr, 3);
      end;

    end;
  except
    Result:= False;
    Exit;
  end;

  Result:= True;
end;

procedure TFormsBox.ExtractAllBox;
begin
  Self.ExtractBox (BitmapImage);  
end;

constructor TFormsBox.Create (DataPath: String);
begin
  inherited Create;

  Self.DataPath:= DataPath;
end;

function TFormsBox.GetAlphaNumericalFieldsArray (Index: Integer): TField;
begin
  Result:= nil;
end;

function TFormsBox.GetCheckBoxFieldsArray (Index: Integer): TCheckBoxField;
begin
  Result:= nil;
end;

function TFormsBox.LoadTheForm (FileName: String): TBitmap;
begin
  if BitmapImage<> nil then
    BitmapImage.Free;

  BitmapImage:= TBitmap.Create;
  BitmapImage.LoadFromFile (FileName);

  Self.FindSkewAndReplacenent (BitmapImage);
end;

function TFormsBox.LoadTheForm (Bitmap: TBitmap): TBitmap;
begin
  if BitmapImage<> nil then
    BitmapImage.Free;

  BitmapImage:= Bitmap;
  Self.FindSkewAndReplacenent (BitmapImage);
end;

{ TDataComponent }

procedure TDataComponent.AddBox (BoxIndex: Integer);
begin
  Inc (FCount);
  SetLength (Boxes, FCount);
  Boxes [FCount- 1]:= BoxIndex;
end;

constructor TDataComponent.Create (Name: String);
begin
  inherited Create;
  FName:= Name;
end;

procedure TDataComponent.DeleteBox (Index: Integer);
var
  i: Integer;
begin
  if (Index< 0) or (FCount< Index) then
    raise Exception.Create ('Range Check Error!');

  for i:= Index+ 1 to FCount- 1 do
    Boxes [i- 1]:= Boxes [i];
  SetLength (Boxes, FCount- 1);
end;

procedure TDataComponent.Free;
begin
  SetLength (Boxes, 0);
  inherited;
end;

function TDataComponent.GetBoxByIndex (Index: Integer): Integer;
begin
  if (Index< 0) or (FCount- 1< Index) then
    raise Exception.Create ('Range Check Error!');
  Result:= Boxes [Index];
end;

procedure TDataComponent.LoadFromFile (var FileHandle: TextFile);
var
  CompTypeCounter: TCompType;
  CompTypeString: String;
  STemp: String;
  Index: Integer;
begin
  ReadLn (FileHandle, CompTypeString);
  CompTypeString:= UpperCase (CompTypeString);

  DataComponent:= ctStart;
  for CompTypeCounter:= ctStart to ctEnd do
    if UpperCase (ComponentTypeToString (CompTypeCounter))= CompTypeString then
    begin
      DataComponent:= CompTypeCounter;
      Break;
    end;
  if DataComponent= ctStart then
    ShowMessage ('Error While loading');

  ReadLn (FileHandle, STemp);
  STemp:= STemp+ ' ';
  while Length (STemp)> 0 do
  begin
    while Length (STemp)> 0 do
      if STemp [1]= ' ' then
        Delete (STemp, 1, 1)
      else
        Break;

    if STemp= '' then
      Break;

    Index:= StrToInt (Copy (STemp, 1, Pos (' ', STemp)- 1));
    Delete (STemp, 1, Pos (' ', STemp));
    Self.AddBox (Index);
  end;
end;

procedure TDataComponent.SaveToFile (var FileHandle: TextFile);
var
  i: Integer;
begin
  Writeln (FileHandle, Name);
  Writeln (FileHandle, ComponentTypeToString (DataComponent));

  for i:= 0 to High (Boxes) do
    Write (FileHandle, Boxes [i], ' ');
  WriteLn (FileHandle);
  WriteLn (FileHandle);
end;

initialization
//  CompCount:= 0;
end.
