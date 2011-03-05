unit GeneralTypesUnit;

interface

uses Forms, Types, CollectionUnit, GeometryUnit, Classes, SysUtils,
  Dialogs, Graphics, Math, MyTypes, PostProcessorUnit;

const
  MAX_ALPHABETS_IN_ANY_LANGUAGE= 50;
  MAX_DIGITS_IN_ANY_LANGUAGE= 10;
  NEW_LINE= #13#10;
  TAB_CHAR= #9;
  MAX_PROCESSED_FORMS_IN_A_SESSION= 100;

type
  EInvalidConfigFile= class (Exception)
  end;
  EInvalidName= class (Exception);
  TFormTypeInfo= class
    public
      Caption: string;
      DefinitionFile, SampleImage: string;
      procedure Clear;
  end;

  TFormTypeInfoCollection= class (TBaseCollection)
  private
    function GetFormTypeInfo(Index: Integer): TFormTypeInfo;
  public
    property FormTypeInfo [Index: Integer]: TFormTypeInfo read GetFormTypeInfo;

    procedure Add (NewFormTypeInfo: TFormTypeInfo);

  end;

  EItemType= (itNone= 0, itNumber, itNumbers, itLetter, itCheck);
  ELanguageID= (lngNone= 0, lngPersian, lngEnglish);
  ELetterCase= (elNone= 0, elMode1, elMode2, elMode3, elMode4);
  ELocationCheckType= (elcNone= 0, elcLine, elcRect, elcCircle);
  EProcessedFormImageMode= (eimBitmap, eimFile);
  EnumList= (elItemType= 0, elLanguageID, elLetterCase, elLocationCheckType);

  {
    It explains the details of a location check component in the reference image.
      The Location check component may be a Line or a rectangle which is specified
        by LocationType.
  }

  TLocationCheck= class
  private
    FLocationType: ELocationCheckType;
    FAnnouncedTop, FAnnouncedLeft, FAnnouncedRight, FAnnouncedBottom: Integer;
    FRealWidth: Integer;
    FRealHeight: Integer;
    FAnnouncedHeight: Integer;
    FAnnouncedWidth: Integer;
    FRealBotLeft: TPoint;
    FRealTopRight: TPoint;
    FRealTopLeft: TPoint;
    FRealBotRight: TPoint;

    procedure SetRealBotLeft (const Value: TPoint);
    procedure SetRealBotRight (const Value: TPoint);
    procedure SetRealTopLeft (const Value: TPoint);
    procedure SetRealTopRight (const Value: TPoint);

  public
    property LocationType: ELocationCheckType read FLocationType write FLocationType;

    property RealTopLeft: TPoint  read FRealTopLeft write SetRealTopLeft;
    property RealTopRight: TPoint read FRealTopRight write SetRealTopRight;
    property RealBotRight: TPoint read FRealBotRight write SetRealBotRight;
    property RealBotLeft: TPoint read FRealBotLeft write SetRealBotLeft;
    property RealWidth: Integer read FRealWidth write FRealWidth;
    property RealHeight: Integer read FRealHeight write FRealHeight;

    property AnnouncedWidth: Integer read FAnnouncedWidth;
    property AnnouncedHeight: Integer read FAnnouncedHeight;
    property AnnouncedTop: Integer read FAnnouncedTop write FAnnouncedTop;
    property AnnouncedBottom: Integer read FAnnouncedBottom write FAnnouncedBottom;
    property AnnouncedLeft: Integer read FAnnouncedLeft write FAnnouncedLeft;
    property AnnouncedRight: Integer read FAnnouncedRight write FAnnouncedRight;


    function ToString: String;
    function Copy: TLocationCheck;
    function LoadFromFile (var InputFile: TextFile): Boolean; 
    
    constructor Create (LocationCheckType: ELocationCheckType);
    destructor Destroy; override;
      
  end;

  {
    A Collection of TLocationCheck
  }
  
  TLocationCheckCollection= class (TBaseCollection)
    private
      function Get (Index: Integer): TLocationCheck;

    public
      property LocationCheck [Index: Integer]: TLocationCheck read Get;
      procedure Add (Item: TLocationCheck);

      function ToString: String;
      function Copy: TLocationCheckCollection;
      
  end;

  {
    A TItemInfo constains a cell's general information.
  }
  TItemInfo= class
  private
  public
    LanguageID: ELanguageID;
    SVMCollectionID: String;
    HasRange: Boolean;
    ItemType: EItemType;
    //itNumber
    RangeNumber: String [10]; //FFTTFFTTFT for 0..9, for example
    //itNumbers
    Length: Integer;
    RangeNumbers: TStringList;
    //itLetter
    LetterCase: ELetterCase;
    RangeLetters: String [MAX_ALPHABETS_IN_ANY_LANGUAGE];

    procedure SetValue (itInfo: TItemInfo);
    function ToString: String;
    function Copy: TItemInfo;
    constructor Create;
    destructor Destroy; override;
      
  end;

  TFieldSet= class;
  
  {
    It holds the information about a single cell.
    Its Value, RealTopLeft and RealBottomRight must be filled by recognizer
      (the two latter are filled with Announced Values)
  }
  
  TFormItemInfo= class
    private
      FItemInfo: TItemInfo;
      FValue: String;
      FCaption: String; //WideString;
      FRealTopLeft, FRealBottomRight: TPoint;
      FAnnouncedTopLeft, FAnnouncedBottomRight: TPoint;
      FInheritsParentProperties: Boolean;

      constructor SimpleCreate;
      function  GetItemType: EItemType;

    public
      property  ItemInfo: TItemInfo read FItemInfo;
      property  ItemType: EItemType read GetItemType;
      property	Value: String read FValue write FValue;
      property  RealTopLeft: TPoint read FRealTopLeft;
      property  RealBottomRight: TPoint read FRealBottomRight;
      property  AnnouncedTopLeft: TPoint read FAnnouncedTopLeft;
      property  AnnouncedBottomRight: TPoint read FAnnouncedBottomRight;

      function  Copy: TFormItemInfo;
      function  ToString: String;
      function GetValue: WideString;

      constructor Create; overload;
      constructor Create (itInfo: TItemInfo); overload;
      destructor Destroy; override;

      procedure LoadFromFile (var InputFile: TextFile; Parent: TFieldSet);
      
  end;

  {
    A Collection of TFormItemInfo.
      Like lastname which may have many letter entry.
       
  }
  
  TFieldSet= class (TBaseCollection)
  private
    FFieldSetInfo: TItemInfo;
    FHasInfo: Boolean;
    FCaption: WideString;
    FPostProcessor: String;
    FCorrectedValue: WideString;
    FPostProcessorWord: TPostProcessorWord;

    constructor SimpleCreate;
    function  Get (Index: Integer): TFormItemInfo;

  public
    procedure SetFieldSetInfo (fsInfo: TItemInfo);
    property HasInfo: Boolean read FHasInfo;
    property FieldSetInfo: TItemInfo read FFieldSetInfo;
    property ItemInfo [Index: Integer]: TFormItemInfo read Get;
    property Caption: WideString read FCaption write FCaption;
    property PostProcessor: String read FPostProcessor write FPostProcessor;
    property CorrectedValue: WideString read FCorrectedValue write FCorrectedValue;
    property PostProcessorWord: TPostProcessorWord read FPostProcessorWord;

    procedure AddCharacterToPostWord (var Data: TIntegerArray);
    procedure Add (item: TFormItemInfo); // inherited;
    function  ToString: String;
    function  GetRect: TRect;
    function  GetValue: String; overload;
    function  GetValue (var Value: String; var Error: String): Boolean; overload;

    function Copy: TFieldSet;
    constructor Create;
    destructor Destroy; override;

  end;

  {
    A Collection of TFeildSet
  }
  TFormItemCollection= class (TBaseCollection)
  private
    function Get (Index: Integer): TFieldSet;
  public
    property FieldSet [Index: Integer]: TFieldSet read Get;
    procedure Add (FieldSet: TFieldSet); // inherited;
    function ToString: String;
    function Copy: TFormItemCollection;

  end;

  {
    Information about SVM 
  }
  
  TSVMInfo= class
    private
    public
      SVMName, KernelName: String;
      Caption: WideString;
      Index: Integer;
      
      function Copy: TSVMInfo;
      
  end;

  {
    A Collection of TSVMInfo.
      It is used as type in the configfile.
  }
  
  TSVMSetInfo= class (TBaseCollection)
    private
      FCollectionID: String;
      FNumberOfSVMs: Integer;
      FFileName: String;
      function GetSVMInfo (Index: Integer): TSVMInfo;
      
    public
      property SVMInfo [Index: Integer]: TSVMInfo read GetSVMInfo;
      property CollectionID: string read FCollectionID write FCollectionID;
      procedure Add (NewSVMInfo: TSVMInfo);
      function  LoadSVMInfosFromFile (Path: String): Boolean;
      function  Copy: TSVMSetInfo;
      function  ToString: String;
      
  end;

  {
    A Collection of TSVMSet
  }
  TSVMSetCollection= class (TBaseCollection)
    private
      function GetSVMSetInfo (Index: Integer): TSVMSetInfo;
      function GetSVMSetByName (Name: String): TSVMSetInfo;
      function GetSVMSetIndexByName (Name: String): Integer;

    public
      property SVMSet [Index: Integer]: TSVMSetInfo read GetSVMSetInfo;
      property SVMSetByName [Name: String]: TSVMSetInfo read GetSVMSetByName;
      property SVMSetIndexByName [Name: String]: Integer read GetSVMSetIndexByName;

      procedure Add (NewSVMSet: TSVMSetInfo);
      function  Copy: TSVMSetCollection;
      function  ToString: String;
      
  end;

  TPostProcessorInfo= class (TObject)
  private
    FDictionaryName: String;
    
  public
    property DictionaryName: String read FDictionaryName;

    constructor Create; overload;
    constructor Create (DicName: String); overload;

  end;

  TPostProcessorInfoCollection= class (TBaseCollection)
  private
    function GetPostProcessorInfo (Index: Integer): TPostProcessorInfo;
    
  public
    property PostProcessorInfo [Index: Integer]: TPostProcessorInfo
           read GetPostProcessorInfo;
    
  end;

  {
    All form information is stored in this class.
      It can load and save itself in a file.
  }
  
  TFormInfo= class
    private
      FFormItemCollection: TFormItemCollection;
      FLocationCheckCollection: TLocationCheckCollection;
      FSVMInfoCollection: TSVMSetCollection;
      FPostProcessorInfoCollection: TPostProcessorInfoCollection;

    public
      property FormItemCollection: TFormItemCollection read FFormItemCollection;
      property LocationCheckCollection: TLocationCheckCollection
              read FLocationCheckCollection; 
      property SVMInfoCollection: TSVMSetCollection
              read fSVMInfoCollection;
      property PostProcessorInfoCollection: TPostProcessorInfoCollection
              read FPostProcessorInfoCollection;
              
      function ToString: String;
      function LoadFromTextFile (FileName: String): Boolean;
      function LoadFromBinaryFile (FileName: String): Boolean;
      procedure SaveToTextFile (FileName: String);
      procedure SaveToBinaryFile (FileName: String);
      function  Copy: TFormInfo;
      constructor Create;
      destructor Destroy; override;
      
  end;

  {

  }
  
  TProcessedFormInfo= class
  private
  public
    FormInfo: TFormInfo;
    FormImage: TBitmap;
    FormFileName: String;
    ImageMode: EProcessedFormImageMode;
    Visited: Boolean;
    constructor Create;
    destructor Destroy; override;

  end;

  TProcessedFormInfoCollection= class (TBaseCollection)
  private
    function GetProcessedFormInfo(Index: Integer): TProcessedFormInfo;
  public
    property ProcessedFormInfo [Index: Integer]: TProcessedFormInfo
        read GetProcessedFormInfo;

    procedure Add (NewProcessedFormInfo: TProcessedFormInfo);
    
  end;
const
  EnumCaptions: array [0..3] of array [0..4] of String =
    (
    ('None', 'Number', 'Numbers', 'Letters', 'Check'),
    ('None', 'Persian', 'English', 'NONE', 'NONE'),
    ('None', 'Big', 'Small', 'Mode3', 'Mode4'),
    ('None', 'Line', 'Rect', 'NONE', 'NONE'));
  ItemTypeSize= 5;
  LanguageIDSize= 3;
  LetterCaseSize= 3;
  LocationChecksSize= 1;

var
  SelectedFormID: Integer;
  SelectedFormInfo: TFormInfo;

implementation

uses
  WideStringUnit;

function GetFirstSegment (var Str: String):String; overload;
var
  Index: Integer;
  
begin
  while Str [1]= #9 do
    Delete (Str, 1, 1);
    
  Index:= Pos (#9, str);
  if (Index= 0) then
  begin
    Result:= Str;
    Str:= '';

  end
  else
  begin
    Result:= Copy (str, 1, Index - 1);
    Delete (str, 1, Index)
    
  end;
end;

function GetFirstSegment (var Str: WideString): WideString; overload;
var
  Index: Integer;

begin
  while Str [1]= #9 do
    WideStrDelete (Str, 1, 1);
  Index:= WideStrPos (#9, Str);
  
  if Index= 0 then
  begin
    Result:= Str;
    Str:= '';
    
  end
  else
  begin
    Result:= WideStrCopy (Str, 1, Index - 1);
    WideStrDelete (Str, 1, Index);
    
  end;
  
end;

function ReadPointInfo (TmpStr: String):TPoint;
begin
  Result:= TPoint.Create;
  GetFirstSegment (TmpStr);
  Result.r:= StrToInt (GetFirstSegment (TmpStr));
  Result.c:= StrToInt (GetFirstSegment (TmpStr));

end;

function ReadIntegerInfo (var TmpStr: String): Integer;
begin
  GetFirstSegment (TmpStr);
  Result:= StrToInt (GetFirstSegment (TmpStr));

end;

function BooleanToString (inp: Boolean): String;
begin
  if inp= true then
    Result:= 'True'
  else
    Result:= 'False';
end;

{ TFormItemInfo }

function TFormItemInfo.Copy: TFormItemInfo;
begin
  Result:= TFormItemInfo.SimpleCreate;
  Result.FItemInfo:= Self.FItemInfo.Copy;
  Result.FValue:= Self.FValue;
  Result.FCaption:= Self.FCaption;
  Result.FRealTopLeft:= Self.FRealTopLeft.Copy;
  Result.FRealBottomRight:= Self.FRealBottomRight.Copy;
  Result.FAnnouncedTopLeft:= Self.FAnnouncedTopLeft.Copy;
  Result.FAnnouncedBottomRight:= Self.FAnnouncedBottomRight.Copy;
  Result.FInheritsParentProperties:= Self.FInheritsParentProperties;

end;

constructor TFormItemInfo.Create (itInfo: TItemInfo);
begin
  FItemInfo:= itInfo.Copy;
  FRealTopLeft:= TPoint.Create;
  FRealBottomRight:= TPoint.Create;
  FAnnouncedTopLeft:= TPoint.Create;
  FAnnouncedBottomRight:= TPoint.Create;
  
end;

constructor TFormItemInfo.Create;
begin
  inherited;
  
  FItemInfo:= TItemInfo.Create;
  FRealTopLeft:= TPoint.Create;
  FRealBottomRight:= TPoint.Create;
  FAnnouncedTopLeft:= TPoint.Create;
  FAnnouncedBottomRight:= TPoint.Create;
  
end;

procedure TFormItemCollection.Add (FieldSet: TFieldSet);
begin
  inherited Add (FieldSet);
  
end;

function  TFormItemCollection.Get (Index: Integer): TFieldSet;
begin
  Result:= Member [Index] as TFieldSet;
  
end;

destructor TFormItemInfo.Destroy;
begin
  FRealTopLeft.Free;
  FRealBottomRight.Free;
  FAnnouncedTopLeft.Free;
  FAnnouncedBottomRight.Free;
  ItemInfo.Free;

  inherited Free;
  
  
end;

function TFormItemInfo.GetItemType: EItemType;
begin
  Result:= ItemInfo.ItemType;
  
end;

function TFormItemInfo.GetValue: WideString;
begin
  Result:= '                          ';
  StringToWideChar (FValue, PWideChar (Result),  10);

end;

procedure TFormItemInfo.LoadFromFile (var InputFile: TextFile; Parent: TFieldSet);
var
  TmpStr: String;
  ty: Integer;
  
begin
  Readln (InputFile, TmpStr);
  FAnnouncedTopLeft.Free;
  FAnnouncedTopLeft:= ReadPointInfo (TmpStr);
  FRealTopLeft.Free;
  FRealTopLeft:= ReadPointInfo (TmpStr);

  Readln (InputFile, TmpStr);
  FAnnouncedBottomRight.Free;
  FAnnouncedBottomRight:= ReadPointInfo (TmpStr);
  FRealBottomRight.Free;
  FRealBottomRight:= ReadPointInfo (TmpStr);

  Readln (InputFile, TmpStr);
  GetFirstSegment (TmpStr);
  TmpStr:= GetFirstSegment (TmpStr);
  if TmpStr= 'True' then
    FInheritsParentProperties:= True
  else
    FInheritsParentProperties:= False;

  if FInheritsParentProperties then
  begin
    FItemInfo.Free;
    FItemInfo:= Parent.FFieldSetInfo.Copy;
    
  end
  else
  begin

    //Reading Caption
    Readln (InputFile, TmpStr);
    GetFirstSegment (TmpStr);
    FCaption:= GetFirstSegment (TmpStr);

    //Reading Type
    Readln (InputFile, TmpStr);
    GetFirstSegment (TmpStr);
    TmpStr:= GetFirstSegment (TmpStr);
    for ty:= 0 to ItemTypeSize - 1 do
      if TmpStr= EnumCaptions [Ord (elItemType)][ty] then
      begin
        FItemInfo.ItemType:= EItemType (ty);
        Break;
        
      end;

    if FItemInfo.ItemType<> itCheck then
    begin
      //Reading Language
      Readln (InputFile, TmpStr);
      GetFirstSegment (TmpStr);
      TmpStr:= GetFirstSegment (TmpStr);
      for ty:= 0 to LanguageIDSize - 1 do
        if TmpStr= EnumCaptions [Ord (elLanguageID)][ty] then
        begin
          FItemInfo.LanguageID:= ELanguageID (ty);
          Break;

        end;

      //Reading Case
      Readln (InputFile, TmpStr);
      GetFirstSegment (TmpStr);
      TmpStr:= GetFirstSegment (TmpStr);
      for ty:= 0 to LetterCaseSize - 1 do
        if TmpStr= EnumCaptions [Ord (elLetterCase)][ty] then
          FItemInfo.LetterCase:= ELetterCase (ty);

      //Reading SVM Set Name
      Readln (InputFile, TmpStr);
      GetFirstSegment (TmpStr);
      FItemInfo.SVMCollectionID:= GetFirstSegment (TmpStr);

      //Reading Use Range
      Readln (InputFile, TmpStr);
      GetFirstSegment (TmpStr);
      TmpStr:= GetFirstSegment (TmpStr);
      if TmpStr= 'True' then
        FItemInfo.HasRange:= True
      else
        FItemInfo.HasRange:= False;

      // Reading Range
      if FItemInfo.HasRange then
      begin
        Readln (InputFile, TmpStr);
        GetFirstSegment (TmpStr);
        TmpStr:= GetFirstSegment (TmpStr);
        FItemInfo.RangeLetters:= TmpStr;
        
      end;
      
    end;
    
  end;

end;

constructor TFormItemInfo.SimpleCreate;
begin
  inherited;
   
end;

function TFormItemInfo.ToString: String;
begin
  Result:= '#Field' + NEW_LINE +
            TAB_CHAR + '.TopLeft' + TAB_CHAR + TAB_CHAR + RealTopLeft.ToString + NEW_LINE +
            TAB_CHAR + '.BottomRight' + TAB_CHAR + RealBottomRight.ToString + NEW_LINE +
            TAB_CHAR + '.Inherits' + TAB_CHAR + TAB_CHAR + BooleanToString (FInheritsParentProperties) + NEW_LINE;
  if not Self.FInheritsParentProperties then
    Result:= Result + Self.FItemInfo.ToString;
end;

{ TFieldSet }

procedure TFieldSet.Add (Item: TFormItemInfo);
begin
  inherited Add (item);
  
end;

procedure TFieldSet.AddCharacterToPostWord (var Data: TIntegerArray);
var
  Index: Integer;
  i: Integer;

begin
  Index:= Length (FPostProcessorWord);
  SetLength (FPostProcessorWord, Index+ 1);
  
  SetLength (FPostProcessorWord [Index], Length (Data));
  for i:= 0 to High (Data) do
    FPostProcessorWord [Index][i]:= Data [i];

end;

function TFieldSet.Copy: TFieldSet;
var
  CopyInstance: TFieldSet;
  ItemCounter: Integer;
begin
  CopyInstance:= TFieldSet.Create;
  CopyInstance.FFieldSetInfo:= Self.FFieldSetInfo.Copy;
  CopyInstance.FHasInfo:= Self.FHasInfo;
  CopyInstance.FCaption:= Self.FCaption;
  CopyInstance.FPostProcessor:= Self.FPostProcessor;
  CopyInstance.FCorrectedValue:= Self.FCorrectedValue;
  
  for ItemCounter:= 0 to Self.Size - 1 do
    CopyInstance.Add (Self.ItemInfo[ItemCounter].Copy);
  Result:= CopyInstance;
end;

constructor TFieldSet.Create;
begin
  
  inherited Create;

  FHasInfo:= False;
  FCaption:= '';
  FPostProcessor:= '';
  FFieldSetInfo:= TItemInfo.Create;

end;

destructor TFieldSet.Destroy;
var
  i: Integer;

begin
  FFieldSetInfo.Free;

  for i:= 0 to High (FPostProcessorWord) do
    SetLength (FPostProcessorWord [i], 0);
  SetLength (FPostProcessorWord, 0);

  inherited Destroy;

end;

function TFieldSet.Get (Index: Integer): TFormItemInfo;
begin
  Result:= Member [Index] as TFormItemInfo;
  
end;

function TFieldSet.GetRect: TRect;
var
  MyRect: TRect;
  Top, Left, Bottom, Right: Integer;
  Counter: Integer;
  ActiveItemInfo: TFormItemInfo;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  ActiveItemInfo:= TFormItemInfo (Ptr^);
  
  Top:= ActiveItemInfo.RealTopLeft.r;
  Left:= ActiveItemInfo.RealTopLeft.c;
  Bottom:= ActiveItemInfo.RealBottomRight.r;
  Right:= ActiveItemInfo.RealBottomRight.c;

  for Counter:= 1 to Size - 1 do
  begin
    Inc (Ptr);
    ActiveItemInfo:= TFormItemInfo (Ptr^);
    
    Top:= Min (Top, ActiveItemInfo.FRealTopLeft.r);
    Left:= Min (Left, ActiveItemInfo.FRealTopLeft.c);
    Bottom:= Max (Bottom, ActiveItemInfo.FRealBottomRight.r);
    Right:= Max (Right, ActiveItemInfo.FRealBottomRight.c);

  end;
  MyRect:= Rect (Left, Top, Right, Bottom);
  Result:= MyRect;
  
end;

function TFieldSet.GetValue (var Value: String; var Error: String): Boolean;
var
  Counter: Integer;
begin
  Value:= '';
  Error:= '';

  if Self.FHasInfo and (Self.FFieldSetInfo.ItemType= itCheck) then
  begin
    for Counter:= 0 to Self.Size - 1 do
      if Self.ItemInfo [Counter].FValue= 'True' then
        if Value= '' then
          Value:= Self.ItemInfo [Counter].FCaption
        else
          Value:= Value + ', ' + Self.ItemInfo[Counter].FCaption;
  end
  else
  begin
    for Counter:= 0 to Self.Size - 1 do
      Value:= Value + Self.ItemInfo[Counter].FValue;
  end;

  Result:= True;

end;

function TFieldSet.GetValue: String;
var
  Error: String;
  
begin
  GetValue (Result, Error);
  
end;

procedure TFieldSet.SetFieldSetInfo (fsInfo: TItemInfo);
begin
  if FFieldSetInfo<> nil then
    FFieldSetInfo:= nil;

  FFieldSetInfo:= fsInfo;
  
end;

constructor TFieldSet.SimpleCreate;
begin
  inherited Create;
  
  FHasInfo:= False;
  FCaption:= '';
  FPostProcessor:= '';
  FFieldSetInfo:= nil;

end;

function TFieldSet.ToString: String;
var
  i: Integer;
  
begin
  Result:= '##FieldSet' + NEW_LINE +
            TAB_CHAR + '.Caption' + TAB_CHAR + TAB_CHAR + Self.Caption + NEW_LINE +
            TAB_CHAR + '.PostProcessor' + TAB_CHAR + Self.PostProcessor + NEW_LINE +
            TAB_CHAR + '.NumOfFields' + TAB_CHAR + IntToStr (Self.Size) + NEW_LINE +
            TAB_CHAR + '.HasItemInfo' + TAB_CHAR + BooleanToString (Self.HasInfo) + NEW_LINE;

  if Self.HasInfo then
    Result:= Result + Self.FFieldSetInfo.ToString;

  for i:= 0 to Size - 1 do
    Result:= Result + ItemInfo [i].ToString;
end;

{ TLocationCheckCollection }

procedure TLocationCheckCollection.Add (item: TLocationCheck);
begin
  inherited Add (item);
  
end;

function TLocationCheckCollection.Copy: TLocationCheckCollection;
var
  CopyInstance: TLocationCheckCollection;
  ItemCounter: Integer;
  
begin
  CopyInstance:= TLocationCheckCollection.Create;
  for ItemCounter:= 0 to Self.Size - 1 do
    CopyInstance.Add (Self.LocationCheck[ItemCounter].Copy);
    
  Result:= CopyInstance;
  
end;

function TLocationCheckCollection.Get (Index: Integer): TLocationCheck;
begin
  Result:= Member [Index] as TLocationCheck;
  
end;

function TLocationCheckCollection.ToString: String;
var
  i: Integer;
begin
  Result:= '#LocationChecks' + NEW_LINE +
            TAB_CHAR + IntToStr (Size) + NEW_LINE;
            
  for i:= 0 to Size - 1 do
    Result:= Result + LocationCheck [i].ToString;
    
end;

{ TLocationCheck }

function TLocationCheck.Copy: TLocationCheck;
begin
  Result:= TLocationCheck.Create (FLocationType);
  Result.FRealTopLeft:= FRealTopLeft.Copy;
  Result.FRealTopRight:= FRealTopRight.Copy;
  Result.RealBotLeft:= Self.FRealBotLeft.Copy;
  Result.RealBotRight:= Self.FRealBotRight.Copy;
  Result.FRealWidth:= Self.FRealWidth;
  Result.FRealHeight:= Self.FRealHeight;

  Result.FAnnouncedTop:= Self.FAnnouncedTop;
  Result.FAnnouncedBottom:= Self.FAnnouncedBottom;
  Result.FAnnouncedLeft:= Self.FAnnouncedLeft;
  Result.FAnnouncedRight:= Self.FAnnouncedRight;
  Result.FAnnouncedWidth:= Self.FAnnouncedWidth;
  Result.FAnnouncedHeight:= Self.FAnnouncedHeight;

end;

constructor TLocationCheck.Create (LocationCheckType: ELocationCheckType);
begin
  inherited Create;

  FLocationType:= LocationCheckType;

  FRealTopRight:= nil;
  FRealTopLeft:= nil;
  FRealBotLeft:= nil;
  FRealBotRight:= nil;

  FAnnouncedTop:= 0;
  FAnnouncedBottom:= 0;
  FAnnouncedRight:= 0;
  FAnnouncedLeft:= 0;

end;

destructor TLocationCheck.Destroy;
begin
  if FRealTopRight<> nil then
    FRealTopRight.Free;
  if FRealTopLeft<> nil then
    FRealTopLeft.Free;
  if FRealBotRight<> nil then
    FRealBotRight.Free;
  if FRealBotLeft<> nil then
    FRealBotLeft.Free;
    
  inherited;

end;

function TLocationCheck.LoadFromFile (var InputFile: TextFile): Boolean;
var
  TempStr: String;
  
begin
  Readln (InputFile, TempStr);
  FAnnouncedTop:= ReadIntegerInfo (TempStr);

  Readln (InputFile, TempStr);
  FAnnouncedBottom:= ReadIntegerInfo (TempStr);

  Readln (InputFile, TempStr);
  FAnnouncedRight:= ReadIntegerInfo (TempStr);

  Readln (InputFile, TempStr);
  FAnnouncedLeft:= ReadIntegerInfo (TempStr);

  if RealTopLeft<> nil then
    RealTopLeft.Free;
  RealTopLeft:= TPoint.Create (FAnnouncedTop, FAnnouncedLeft);

  if RealTopRight<> nil then
    RealTopRight.Free;
  RealTopRight:= TPoint.Create (FAnnouncedTop, FAnnouncedRight);

  if RealBotLeft<> nil then
    RealBotLeft.Free;
  RealBotLeft:= TPoint.Create (FAnnouncedBottom, FAnnouncedLeft);

  if RealBotRight<> nil then
    RealBotRight.Free;
  RealBotRight:= TPoint.Create (FAnnouncedBottom, FAnnouncedRight);

  Readln (InputFile, TempStr);
  FAnnouncedWidth:= ReadIntegerInfo (TempStr);
  RealWidth:= FAnnouncedLeft;

  Readln (InputFile, TempStr);
  FAnnouncedHeight:= ReadIntegerInfo (TempStr);
  RealHeight:= FAnnouncedLeft;

  Result:= True;
  
end;

function TLocationCheck.ToString: String;
begin
  Result:= '#LocationCheck' + NEW_LINE +
            TAB_CHAR + '.LocationType' + TAB_CHAR +
            '????'+ NEW_LINE+
            TAB_CHAR + '.TopLine' + TAB_CHAR + TAB_CHAR + IntToStr (RealTopLeft.r)+ NEW_LINE +
            TAB_CHAR + '.BottomLine' + TAB_CHAR + IntToStr (RealBotLeft.r) + NEW_LINE+
            TAB_CHAR + '.RightLeft' + TAB_CHAR + TAB_CHAR + IntToStr (RealTopLeft.c)+ NEW_LINE +
            TAB_CHAR + '.LeftRight' + TAB_CHAR + IntToStr (RealBotRight.c) + NEW_LINE;
            
end;

procedure TLocationCheck.SetRealBotLeft (const Value: TPoint);
begin
  if FRealBotLeft<> nil then
    FRealBotLeft.Free;

  FRealBotLeft:= Value;
  
end;

procedure TLocationCheck.SetRealBotRight (const Value: TPoint);
begin
  if FRealBotRight<> nil then
    FRealBotRight.Free;

  FRealBotRight:= Value;
  
end;

procedure TLocationCheck.SetRealTopLeft (const Value: TPoint);
begin
  if FRealTopLeft<> nil then
    FRealTopLeft.Free;

  FRealTopLeft:= Value;
  
end;

procedure TLocationCheck.SetRealTopRight (const Value: TPoint);
begin
  if FRealTopRight<> nil then
    FRealTopRight.Free;

  FRealTopRight:= Value;
  
end;

{ TItemInfo }

function TItemInfo.Copy: TItemInfo;
var
  CopyInstance: TItemInfo;
  Counter: Integer;
begin
  CopyInstance:= TItemInfo.Create;
  CopyInstance.LanguageID:= Self.LanguageID;
  CopyInstance.SVMCollectionID:= Self.SVMCollectionID;
  CopyInstance.HasRange:= Self.HasRange;
  CopyInstance.ItemType:= Self.ItemType;
  //itNumber
  CopyInstance.RangeNumber:= Self.RangeNumber;
  // itNumbers
  CopyInstance.Length:= Self.Length;
  if Self.RangeNumbers<> nil then
  begin
    if CopyInstance.RangeNumbers= nil then
      CopyInstance.RangeNumbers:= TStringList.Create;
    for Counter:= 0 to Self.RangeNumbers.Count - 1 do
      CopyInstance.RangeNumbers.Add (Self.RangeNumbers [counter]);
  end;
  //itLetter
  CopyInstance.LetterCase:= Self.LetterCase;
  CopyInstance.RangeLetters:= Self.RangeLetters;
  Result:= CopyInstance;
end;

constructor TItemInfo.Create;
begin
  RangeNumbers:= nil;
  
end;

destructor TItemInfo.Destroy;
begin
  if RangeNumbers<> nil then
    RangeNumbers.Free;

  inherited;
  
end;

procedure TItemInfo.SetValue (itInfo: TItemInfo);
var
  counter: Integer;
  
begin
  LanguageID:= itInfo.LanguageID;
  HasRange:= itInfo.HasRange;
  ItemType:= itInfo.ItemType;
  //itNumber
  RangeNumber:= itInfo.RangeNumber;
  // itNumbers
  Length:= itInfo.Length;
  if itInfo.RangeNumbers<> nil then
  begin
    if RangeNumbers= nil then
      RangeNumbers:= TStringList.Create;
   for counter:= 0 to itInfo.RangeNumbers.Count - 1 do
      RangeNumbers.Add (itInfo.RangeNumbers [counter]);
  end;
  //itLetter
  LetterCase:= itInfo.LetterCase;
  RangeLetters:= itInfo.RangeLetters;
end;

function TItemInfo.ToString: String;
begin
  Result:= TAB_CHAR + '.ItemType' + TAB_CHAR ;//EnumCaptions [Ord (elItemType)] [Ord (Self.ItemType)] + NEW_LINE;
  if Self.ItemType<> itCheck then
  begin
    Result:= Result +
//            TAB_CHAR + '.Language' + TAB_CHAR + EnumCaptions [Ord (elLanguageID)] [Ord (Self.LanguageID)] + NEW_LINE +
//            TAB_CHAR + '.Case' + TAB_CHAR + TAB_CHAR + EnumCaptions [Ord (elLetterCase)] [Ord (Self.LetterCase)] + NEW_LINE +
            TAB_CHAR + '.SVMSet' + TAB_CHAR + Self.SVMCollectionID + NEW_LINE +
            TAB_CHAR + '.UseRange' + TAB_CHAR + BooleanToString (HasRange) + NEW_LINE;
    if Self.HasRange then
    begin
      Result:= Result + TAB_CHAR + '.RangeSet' + TAB_CHAR;
      case Self.ItemType of
        itNumber: Result:= Result + Self.RangeNumber;
        itLetter: Result:= Result + Self.RangeLetters;
      end;
      Result:= Result + NEW_LINE;
    end;
  end;
end;

{ TFormInfo }

function TFormInfo.Copy: TFormInfo;
var
  CopyInstance: TFormInfo;
begin
  CopyInstance:= TFormInfo.Create;
  CopyInstance.FFormItemCollection:= Self.FormItemCollection.Copy;
  CopyInstance.FLocationCheckCollection:= Self.LocationCheckCollection.Copy;
  CopyInstance.FSVMInfoCollection:= Self.SVMInfoCollection.Copy;
  Result:= CopyInstance;
  
end;

constructor TFormInfo.Create;
begin
  inherited;
  
  FFormItemCollection:= TFormItemCollection.Create;
  FLocationCheckCollection:= TLocationCheckCollection.Create;
  FSVMInfoCollection:= TSVMSetCollection.Create;
  
end;

destructor TFormInfo.Destroy;
begin
  FormItemCollection.Free;
  LocationCheckCollection.Free;
  SVMInfoCollection.Free;

  inherited;
  
end;

function TFormInfo.LoadFromBinaryFile (FileName: String): Boolean;
var
  fiFile: File;
begin
  // PROBLEMS HERE...
  AssignFile (fiFile, FileName);
  Reset (fiFile, 1);
  BlockRead (fiFile, Self, SizeOf (Self));
  CloseFile (fiFile);
  Result:= True;
  
end;

function TFormInfo.LoadFromTextFile (FileName: String): Boolean;
var
  FdFile: TextFile;
  TmpWideStr: WideString;
  Value, TmpStr: String;
  NumLocationChecks, NumFieldSets, NumFields, NumSVMSets,
  NumDictionaries: Integer;
  Counter: Integer;
  lc: TLocationCheck;
  fs: TFieldSet;
  fi: TFormItemInfo;
  tx: Integer;
  PostProcessorInfo: TPostProcessorInfo;
  SvmSet: TSVMSetInfo;
  SvmSetPath: String;
  Ch: Char;
  i, j, k,
  Version: Integer;

begin
  Result:= False;
  if not FileExists (FileName) then
    Exit;

  AssignFile (FdFile, FileName);
  Reset (FdFile);

  SvmSetPath:= ExtractFilePath (ExtractFilePath (Application.ExeName) + FileName);

  Read (FdFile, Ch);
  if Ch<> #239 then
    Exit;
  Read (FdFile, Ch);
  if Ch<> #187 then
    Exit;
  Read (FdFile, Ch);
  if Ch<> #191 then
    Exit;

  Version:= 1;
  if not EOF (fdFile) then
  begin
    Readln (fdFile, TmpStr);
    while TmpStr [1]= '%' do
      Readln (FdFile, TmpStr);

    Value:= GetFirstSegment (TmpStr);
    if Value= '.Version' then
    begin
      if GetFirstSegment (TmpStr)= '2' then
        Version:= 2;
      Readln (FdFile, TmpStr);
      Value:= GetFirstSegment (TmpStr);
      
    end;

    //reading location checks
    if Value<> '##LocationChecks' then
      Exit;
      
    Readln (FdFile, tx);
    NumLocationChecks:= tx;

    if NumLocationChecks<> 1 then
      raise EInvalidConfigFile.Create ('Invalid LocationCheck Count');

    for Counter:= 1 to numLocationChecks do
    begin
      Readln (FdFile, TmpStr);
      if TmpStr<> '#LocationCheck' then
        Exit;

      Readln (FdFile, TmpStr);
      if Pos (UpperCase ('Rect'), UpperCase (TmpStr)) > 0 then
        lc:= TLocationCheck.Create (elcRect)
      else if Pos (UpperCase ('Line'), UpperCase (TmpStr)) > 0 then
        lc:= TLocationCheck.Create (elcLine)
      else if Pos (UpperCase ('Circle'), UpperCase (TmpStr)) > 0 then
        lc:= TLocationCheck.Create (elcCircle)
      else
      begin
        raise EInvalidConfigFile.Create ('Invaid Location Check Type!');
        Exit;
        
      end;

      if not lc.LoadFromFile (FdFile) then
      begin
        lc.Free;
        Exit;

      end;

      LocationCheckCollection.Add (lc);
      
    end;

    //reading svm sets
    Readln (FdFile, TmpStr);
    if TmpStr<> '##SVMSets' then exit;
    Readln (FdFile, tx);
      
    numSVMSets:= tx;
    for Counter:= 1 to numSVMSets do
    begin
      Readln (FdFile, TmpStr);
      if TmpStr<> '#SVMSet' then
        Exit;

      svmset:= TSVMSetInfo.Create;

      Readln (FdFile, TmpStr);
      GetFirstSegment (TmpStr);
      svmset.FCollectionID:= GetFirstSegment (TmpStr);

      Readln (FdFile, TmpStr);
      GetFirstSegment (TmpStr);
      svmset.FNumberOfSVMs:= StrToInt (GetFirstSegment (TmpStr));

      Readln (FdFile, TmpStr);
      GetFirstSegment (TmpStr);
      svmset.FFileName:= GetFirstSegment (TmpStr);
      svmset.LoadSVMInfosFromFile (svmsetpath);
      SVMInfoCollection.Add (svmset);
      
    end;

    //reading PostProcessors
    Readln (FdFile, TmpStr);
    if TmpStr<> '##PostProcessor' then
      Exit;
    Readln (FdFile, tx);
    FPostProcessorInfoCollection:= TPostProcessorInfoCollection.Create;
      
    NumDictionaries:= tx;
    for Counter:= 1 to NumDictionaries do
    begin
      Readln (FdFile, TmpStr);
      if TmpStr<> '#Dictionary' then
        Exit;

      PostProcessorInfo:= TPostProcessorInfo.Create;

      Readln (FdFile, TmpStr);
      GetFirstSegment (TmpStr);
      PostProcessorInfo.FDictionaryName:= GetFirstSegment (TmpStr);

      PostProcessorInfoCollection.Add (PostProcessorInfo);
      
    end;
    
    //reading field sets
    Readln (FdFile, TmpStr);
    if TmpStr<> '##FieldSets' then
      Exit;
      
    Readln (FdFile, tx);
    NumFieldSets:= tx;
    for counter:= 1 to NumFieldSets do
    begin
      Readln (FdFile, TmpStr);
      if TmpStr<> '##FieldSet' then
        Exit;

      fs:= TFieldSet.Create;

//      Readln (FdFile, TmpStr);
      TmpWideStr:= ReadWideString (fdFile);
      GetFirstSegment (TmpWideStr);
      fs.Caption:= GetFirstSegment (TmpWideStr);

      Readln (FdFile, TmpStr);
      GetFirstSegment (TmpStr);
      fs.PostProcessor:= GetFirstSegment (TmpStr);

      Readln (FdFile, TmpStr);
      GetFirstSegment (TmpStr);
      numFields:= StrToInt (GetFirstSegment (TmpStr));

      Readln (FdFile, TmpStr);
      GetFirstSegment (TmpStr);
      fs.FHasInfo:= False;
      if (GetFirstSegment (TmpStr)= 'True') then
      begin
        fs.FHasInfo:= True;

        //Reading Item Type
        Readln (FdFile, TmpStr);
        GetFirstSegment (TmpStr);
        TmpStr:= GetFirstSegment (TmpStr);
        for tx:= 0 to ItemTypeSize - 1 do
          if TmpStr= EnumCaptions [Ord (elItemType)][tx] then
          begin
            fs.FFieldSetInfo.ItemType:= EItemType (tx);
            Break;
            
          end;

        if fs.FFieldSetInfo.ItemType<> itCheck then
        begin
          //Reading Language
          Readln (FdFile, TmpStr);
          GetFirstSegment (TmpStr);
          TmpStr:= GetFirstSegment (TmpStr);
          for tx:= 0 to LanguageIDSize - 1 do
            if TmpStr= EnumCaptions [Ord (elLanguageID)][tx] then
            begin
              fs.FFieldSetInfo.LanguageID:= ELanguageID (tx);
              Break;

            end;

          //Reading Case
          Readln (FdFile, TmpStr);
          GetFirstSegment (TmpStr);
          TmpStr:= GetFirstSegment (TmpStr);
          for tx:= 0 to LetterCaseSize - 1 do
            if TmpStr= EnumCaptions [Ord (elLetterCase)][tx] then
            begin
              fs.FFieldSetInfo.LetterCase:= ELetterCase (tx);
              Break;
              
            end;

          //Reading SVM Set Name
          Readln (FdFile, TmpStr);
          GetFirstSegment (TmpStr);
          fs.FFieldSetInfo.SVMCollectionID:= GetFirstSegment (TmpStr);

          //Reading Use Range
          Readln (FdFile, TmpStr);
          GetFirstSegment (TmpStr);
          TmpStr:= GetFirstSegment (TmpStr);
          if TmpStr= 'True' then
            fs.FFieldSetInfo.HasRange:= True
          else
            fs.FFieldSetInfo.HasRange:= False;

          // Reading Range
          if fs.FFieldSetInfo.HasRange then
          begin
            Readln (FdFile, TmpStr);
            GetFirstSegment (TmpStr);
            TmpStr:= GetFirstSegment (TmpStr);
            fs.FFieldSetInfo.RangeLetters:= TmpStr;
            
          end;

        end;
        
      end;

      //Reading Fields
      for tx:= 0 to numFields - 1 do
      begin
        Readln (FdFile, TmpStr);
        if TmpStr<> '#Field' then exit;

        fi:= TFormItemInfo.Create;
        fi.LoadFromFile (FdFile, fs);

        fs.Add (fi);
        
      end;
      FormItemCollection.Add (fs);
      
    end;
    
    Result:= True;
    
  end;

  if Version= 2 then
  begin
    for i:= 0 to FormItemCollection.Size- 1 do
      for j:= 0 to FormItemCollection.FieldSet [i].Size- 1 do
      begin

        FormItemCollection.FieldSet [i].ItemInfo [j].AnnouncedTopLeft.Move (-LocationCheckCollection.LocationCheck [0].FAnnouncedTop,
        -LocationCheckCollection.LocationCheck [0].AnnouncedLeft);
        FormItemCollection.FieldSet [i].ItemInfo [j].AnnouncedBottomRight.Move (-LocationCheckCollection.LocationCheck [0].FAnnouncedTop,
        -LocationCheckCollection.LocationCheck [0].FAnnouncedLeft);
        FormItemCollection.FieldSet [i].ItemInfo [j].RealTopLeft.Move (-LocationCheckCollection.LocationCheck [0].FAnnouncedTop,
        -LocationCheckCollection.LocationCheck [0].FAnnouncedLeft);
        FormItemCollection.FieldSet [i].ItemInfo [j].RealBottomRight.Move (-LocationCheckCollection.LocationCheck [0].FAnnouncedTop,
        -LocationCheckCollection.LocationCheck [0].FAnnouncedLeft);

      end;


  end;

  CloseFile (FdFile);
  
end;

function TFormItemCollection.ToString: String;
var
  i: Integer;
begin
  Result:= '##FieldSets' + NEW_LINE +
            TAB_CHAR + IntToStr (Size) + NEW_LINE;

  for i:= 0 to Size - 1 do
    Result:= Result + FieldSet [i].ToString;
end;

procedure TFormInfo.SaveToBinaryFile (FileName: String);
var
  fiFile: File;
begin
  AssignFile (fiFile, FileName);
  Rewrite (fiFile, 1);
  BlockWrite (fiFile, Self, SizeOf (Self));
  CloseFile (fiFile);
end;

procedure TFormInfo.SaveToTextFile (FileName: String);
var
  FdFile: TextFile;
begin
  AssignFile (FdFile, FileName);
  Rewrite (FdFile);
  Write (FdFile, Self.ToString);
  CloseFile (FdFile);
end;

function TFormInfo.ToString: String;
begin
  Result:= LocationCheckCollection.ToString + SVMInfoCollection.ToString + FormItemCollection.ToString;
  
end;

{ TProcessedFormInfo }

constructor TProcessedFormInfo.Create;
begin
  Self.FormInfo:= TFormInfo.Create;
  Self.FormImage:= TBitmap.Create;
  Self.Visited:= False;
  
end;

destructor TProcessedFormInfo.Destroy;
begin
  Self.FormInfo.Free;
  Self.FormImage.Free;

  inherited;
  
end;

{ TFormTypeInfo }

procedure TFormTypeInfo.Clear;
begin
  Caption:= '';
  DefinitionFile:= '';
  SampleImage:= '';
end;

function TFormItemCollection.Copy: TFormItemCollection;
var
  CopyInstance: TFormItemCollection;
  ItemCounter: Integer;
begin
  CopyInstance:= TFormItemCollection.Create;
  for ItemCounter:= 0 to Self.Size - 1 do
    CopyInstance.Add (Self.FieldSet [ItemCounter].Copy);
  Result:= CopyInstance;
end;

{ TSVMSetCollection }

procedure TSVMSetCollection.Add (NewSVMSet: TSVMSetInfo);
begin
  inherited Add (NewSVMSet);
  
end;

function TSVMSetCollection.Copy: TSVMSetCollection;
var
  CopyInstance: TSVMSetCollection;
  ItemCounter: Integer;
begin
  CopyInstance:= TSVMSetCollection.Create;
  for ItemCounter:= 0 to Self.Size - 1 do
    CopyInstance.Add (Self.SVMSet[ItemCounter].Copy);
  Result:= CopyInstance;
end;

function TSVMSetCollection.GetSVMSetByName (Name: String): TSVMSetInfo;
var
  i: Integer;

begin
  Result:= nil;
  for i:= 0 to Self.Size - 1 do
    if Self.SVMSet [i].FCollectionID= Name then
    begin
      Result:= Self.SVMSet [i];
      Exit;

    end;


end;

function TSVMSetCollection.GetSVMSetIndexByName (Name: String): Integer;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 0 to Size- 1 do
  begin
    if TSVMSetInfo (Ptr^).CollectionID= Name then
    begin
      Result:= i;
      Exit;

    end;
    Inc (Ptr);
    
  end;

  raise EInvalidName.Create (Name);
  
end;

function TSVMSetCollection.GetSVMSetInfo (Index: Integer): TSVMSetInfo;
begin
  Result:= Member [Index] as TSVMSetInfo;
  
end;

function TSVMSetCollection.ToString: String;
var
  i: Integer;
begin
  Result:= '##SVMSets' + NEW_LINE +
            TAB_CHAR + IntToStr (Size) + NEW_LINE;

  for i:= 0 to Size - 1 do
    Result:= Result + SVMSet [i].ToString;
end;

{ TSVMInfo }

function TSVMInfo.Copy: TSVMInfo;
begin
  Result:= TSVMInfo.Create;
  Result.SVMName:= Self.SVMName;
  Result.KernelName:= Self.KernelName;
  Result.Caption:= Self.Caption;
end;

{ TSVMSetInfo }

procedure TSVMSetInfo.Add (NewSVMInfo: TSVMInfo);
begin
  inherited Add (NewSVMInfo);
end;

function TSVMSetInfo.Copy: TSVMSetInfo;
var
  CopyInstance: TSVMSetInfo;
  ItemCounter: Integer;
begin
  CopyInstance:= TSVMSetInfo.Create;
  CopyInstance.FCollectionID:= Self.FCollectionID;
  for ItemCounter:= 0 to Self.Size - 1 do
    CopyInstance.Add (Self.SVMInfo[ItemCounter].Copy);
  Result:= CopyInstance;
  
end;

function TSVMSetInfo.GetSVMInfo (Index: Integer): TSVMInfo;
begin
  Result:= Member [Index] as TSVMInfo;
  
end;

function TSVMSetInfo.LoadSVMInfosFromFile (Path: String): Boolean;
var
  FdFile: TextFile;
  TmpStr: String;
  TmpWideStr: WideString;
  NumSVMs: Integer;
  Counter: Integer;
  Tx: Integer;
  SVM: TSVMInfo;
  Ch: Char;

  
begin
  Result:= False;
  AssignFile (FdFile, path + Self.FFileName);
  Reset (FdFile);
  Read (FdFile, Ch);
  if Ch<> #239 then
    Exit;
  Read (FdFile, Ch);
  if Ch<> #187 then
    Exit;
  Read (FdFile, Ch);
  if Ch<> #191 then
    Exit;

  if not EOF (FdFile) then
  begin
    Readln (FdFile, TmpStr);
    while TmpStr [1]= '%' do
      Readln (FdFile, TmpStr);

    //reading number of svms
    if TmpStr<> '##SVMSet' then
      Exit;
      
    Readln (FdFile, Tx);
    NumSVMs:= Tx;
    for Counter:= 1 to NumSVMs do
    begin
      Readln (FdFile, TmpStr);
      if TmpStr<> '#SVM' then Exit;
      SVM:= TSVMInfo.Create;

      TmpWideStr:= ReadWideString (FdFile);
      GetFirstSegment (TmpWideStr);
      SVM.Index:= StrToInt (GetFirstSegment (TmpWideStr));

      TmpWideStr:= ReadWideString (FdFile);
      GetFirstSegment (TmpWideStr);
      SVM.Caption:= GetFirstSegment (TmpWideStr);

      Readln (FdFile, TmpStr);
      GetFirstSegment (TmpStr);
      SVM.KernelName:= GetFirstSegment (TmpStr);

      Readln (FdFile, TmpStr);
      GetFirstSegment (TmpStr);
      SVM.SVMName:= GetFirstSegment (TmpStr);

      Add (SVM);
      
    end;
    
    Result:= True;

  end;

  CloseFile (FdFile);
end;

function TSVMSetInfo.ToString: String;
begin
  Result:= '#SVMSet' + NEW_LINE +
            TAB_CHAR + '.ID' + TAB_CHAR + TAB_CHAR + FCollectionID + NEW_LINE +
            TAB_CHAR + '.NumOfSVMs' + TAB_CHAR + IntToStr (FNumberOfSVMs) + NEW_LINE +
            TAB_CHAR + '.DescriptionFile' + TAB_CHAR + FFileName + NEW_LINE;
end;

{ TFormTypeInfoCollection }

function TFormTypeInfoCollection.GetFormTypeInfo(
  Index: Integer): TFormTypeInfo;
begin
  Result:= Member [Index] as TFormTypeInfo;
  
end;

procedure TFormTypeInfoCollection.Add (NewFormTypeInfo: TFormTypeInfo);
begin
  inherited Add (NewFormTypeInfo);
  
end;


{ TProcessedFormInfoCollection }

procedure TProcessedFormInfoCollection.Add(
  NewProcessedFormInfo: TProcessedFormInfo);
begin
  inherited Add (NewProcessedFormInfo);
  
end;

function TProcessedFormInfoCollection.GetProcessedFormInfo(
  Index: Integer): TProcessedFormInfo;
begin
  Result:= Member [Index] as TProcessedFormInfo;
  
end;

{ TPostProcessorInfoCollection }

function TPostProcessorInfoCollection.GetPostProcessorInfo(
  Index: Integer): TPostProcessorInfo;
begin
  Result:= Member [Index] as TPostProcessorInfo;
  
end;

{ TPostProcessorInfo }

constructor TPostProcessorInfo.Create;
begin
  inherited Create;

  FDictionaryName:= '';
  
end;

constructor TPostProcessorInfo.Create (DicName: String);
begin
  inherited Create;

  FDictionaryName:= DicName;
  
end;

end.
